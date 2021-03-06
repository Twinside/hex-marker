{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
-- | This module help building binary parsers used to mark
-- data of a file and produce an HTML report displaying all the
-- information of the file.
module Data.Marker( -- * Basic types
                    Marker
                  , Markeable( .. )
                  , markBytesRead
                  -- * Data markers
                  -- ** Simple data marking
                  , mark4bitsEach
                  , markWord8
                  , markWord16be
                  , markWord16le
                  , markWord32be
                  , markWord32le
                  -- ** Marking and atomic transformation
                  , markTransformWord8
                  , markTransformWord16be
                  , markTransformWord16le
                  , markTransformWord32be
                  , markTransformWord32le
                  -- ** Monadic marking
                  , markTransformWord8M
                  , markTransformWord16beM
                  , markTransformWord16leM
                  , markTransformWord32beM
                  , markTransformWord32leM

                  -- * Combinatoric marking
                  , markByteString
                  , markAllRemainingByte
                  , delimitateRegion
                  , skipUntil
                  , renderByteDump
                  , subZone
                  ) where

import Data.Monoid( (<>) )
import Control.Applicative( (<$>), (<*>), pure, (<*) )
import Control.Monad( when )
import Data.Monoid( mempty )
import Data.Bits( (.|.), (.&.)
                , unsafeShiftL
                , unsafeShiftR
                )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List(intersperse)
import qualified Data.Text as T
import Data.Word( Word8, Word16, Word32 )
import Data.Vector( (!) )
import qualified Data.Vector as V
import Control.Monad.RWS( RWS, ask, asks, gets
                        , tell, runRWS, modify
                        , put )
import Control.Monad.Error( ErrorT, lift, throwError, runErrorT )
import Text.Printf( printf )
import Text.Blaze( Markup, toMarkup )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8( renderMarkup )
import Text.Groom

data MarkEntry = MarkEntry
    { markName     :: !T.Text
    , markShow     :: !T.Text
    , markOffset   :: {-# UNPACK #-} !Int
    , markSize     :: {-# UNPACK #-} !Int
    , markChildren :: [MarkEntry]
    }
    deriving (Eq, Show)

data MarkerSetup = MarkerSetup
    { setupData     :: B.ByteString
    , setupFilename :: FilePath
    }

data MarkerState = MarkerState
    { markerIndex   :: !Int }

type MarkerMonad =
    RWS MarkerSetup [MarkEntry] MarkerState

type Marker a =
    ErrorT String MarkerMonad a

class Representable a where
    represent :: a -> T.Text

instance Representable B.ByteString where
    represent _ = "<STR>"

instance Show a => Representable a where
    represent = T.pack . groom

class Representable a => Markeable a where
    parseMark :: T.Text -> Marker a

instance Markeable Word8 where
    parseMark = markWord8


byteTexts :: V.Vector T.Text
byteTexts = V.fromListN (b + 1) [T.pack $ printf "%02X " v  | v <- [0 .. b]]
  where b = fromIntegral $ (maxBound :: Word8)

dumpDescriptions :: [MarkEntry] -> Markup
dumpDescriptions entries = mapM_ declare $ zip (map (:[]) [0 :: Int ..]) entries
  where globalTag idx = H.div H.! A.class_ "hidden info" 
                              H.! A.id (flattenIdx idx)
        flattenIdx idx =
            H.toValue . T.pack . ("i_" ++) . concat . intersperse "_" $ map show idx

        declare (idx, entry) = do
            globalTag idx $ do
                H.div H.! A.class_ "name" $ toMarkup $ markName entry
                H.div H.! A.class_ "offset" $ toMarkup $ ("Offset " <>) $ show $ markOffset entry
                H.div H.! A.class_ "size" $ toMarkup $ ("Size " <>) $ show $ markSize entry
                H.div H.! A.class_ "data" $ H.pre $ toMarkup $ markShow entry

            mapM_ declare . zip (map (:idx) [0..]) $ markChildren entry

dumpMarkers :: Int -> MarkerSetup -> [MarkEntry] -> Markup
dumpMarkers bytePerLine setup lst = fst $ markuper (zip (map (:[]) [0 :: Int ..]) lst) True 0
  where rawData = setupData setup

        spanTag (idx, entry) = H.span H.! A.id (H.toValue elemId)
                                      H.! A.class_ (H.toValue ("region" :: T.Text))
                                      H.! A.title (H.toValue $ markName entry)
          where elemId  = "r_" ++ concat (intersperse "_" $ map show idx)
                   

        markuper [] _ lineIndex = (mempty, lineIndex)
        markuper ((idx, entry@MarkEntry { markChildren = _ : _ }) : rest)
          needEnclosing lineIndex =
              (children >> next, finalIndex)
            where (html, childIndex) =
                        markuper (zip (map (:idx) [0..]) $ markChildren entry) True lineIndex
                  children = H.span H.! A.class_ "subregion"
                                    H.! A.title (H.toValue $ markName entry) $ html
                  (next, finalIndex) = markuper rest needEnclosing childIndex

        markuper ((idx, entry) : rest) needEnclosing lineIndex 
            -- If we're accross the max, split and render
            | lineIndex + markSize entry > bytePerLine = (finalMarkup >> next, ixFinal)
                where diff = bytePerLine - lineIndex
                      firstPart = entry { markSize = diff }
                      secondPart = entry
                          { markSize = markSize entry - diff
                          , markOffset = markOffset entry + diff
                          }
                      
                      (before, _) = markuper [(idx, firstPart)] False lineIndex
                      (after, ix2) = markuper [(idx, secondPart)] False 0
                      (next, ixFinal) =  markuper rest True ix2
                      combinedMarkup = before >> "\n" >> after
                      finalMarkup | needEnclosing = spanTag (idx, entry) combinedMarkup
                                  | otherwise = combinedMarkup

                    

        markuper ((idx, entry) : rest) needEnclosing lineIndex = (markup >> nextMarkup, idx')
            where (nextMarkup, idx') = markuper rest needEnclosing $ lineIndex + size
                  offset = markOffset entry
                  size = markSize entry
                  bytes = [byteTexts ! fromIntegral (B.index rawData o) 
                                        | o <- [offset .. offset + size - 1]]
                  markup
                    | not needEnclosing = toMarkup $ T.concat bytes
                    | otherwise = spanTag (idx, entry) . toMarkup $ T.concat bytes

renderByteDump :: B.ByteString -> Marker a -> LB.ByteString
renderByteDump str action = renderMarkup document
  where setup = MarkerSetup { setupData = str, setupFilename = "" }
        (finalValue, MarkerState readedSize, history) =
            runRWS (runErrorT action) setup (MarkerState 0)

        txt = case finalValue of
          Left err -> "Error : " ++ err
          Right _v -> "OK"

        bytePerLine = 16
        document = H.html $ do
            H.head $ do
                H.title "Hex dump"
                H.link H.! A.rel "stylesheet"
                       H.! A.type_ "text/css"
                       H.! A.href "hexmark.css"
                H.script H.! A.src "jquery-2.0.3.min.js" $ ""
                H.script H.! A.src "hexmark.js" $ ""

            H.body $ do
                toMarkup $ "Readed " ++ show readedSize
                H.div $ toMarkup txt
                H.table $ H.tr $ do
                    H.td $ H.pre H.! A.class_ "offsets" $ toMarkup 
                         $ T.concat [ T.pack $ printf "%08X\n" v | v <- [0, bytePerLine .. B.length str]]
                    H.td $ H.pre $ dumpMarkers bytePerLine setup history

                H.div H.! A.class_ "infocontainer" $ dumpDescriptions history

getByte :: Marker Word8
getByte = B.index <$> asks setupData <*> gets markerIndex

markBytesRead :: Marker Int
markBytesRead = gets markerIndex

addIndex :: Int -> Marker ()
addIndex size = lift . modify $ \s -> s { markerIndex = markerIndex s + size }

incIndex :: Marker ()
incIndex = addIndex 1

mark4bitsEach :: T.Text -> Marker (Word8, Word8)
mark4bitsEach = markTransformWord8 $ \v ->
    ((v `unsafeShiftR` 4) .&. 0xF, v .&. 0xF)

markWord8 :: T.Text -> Marker Word8
markWord8 = markTransformWord8 id

markTransformWord8M :: Representable a
                    => (Word8 -> Marker a) -> T.Text -> Marker a
markTransformWord8M transform txt = do
    offset <- gets markerIndex
    v <- getByte <* incIndex
    final <- transform v
    tell [MarkEntry { 
            markName = txt,
            markShow = represent final,
            markOffset = offset,
            markSize = 1,
            markChildren = []
        }]

    pure final

markTransformWord8 :: Representable a
                   => (Word8 -> a) -> T.Text -> Marker a
markTransformWord8 transform = markTransformWord8M (return . transform)

markTransformWord16beM :: Representable a
                      => (Word16 -> Marker a) -> T.Text -> Marker a
markTransformWord16beM transform txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    let v = v1 `unsafeShiftL` 8 .|. v2
    final <- transform v
    tell [MarkEntry { 
            markName = txt,
            markShow = represent final,
            markOffset = offset,
            markSize = 2,
            markChildren = []
        }] 

    pure final

markTransformWord16be :: Representable a
                      => (Word16 -> a) -> T.Text -> Marker a
markTransformWord16be transform =
    markTransformWord16beM (return . transform)

markWord16be :: T.Text -> Marker Word16
markWord16be = markTransformWord16be id

markTransformWord16leM :: Representable a
                       => (Word16 -> Marker a) -> T.Text -> Marker a
markTransformWord16leM transform txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    let v = v1 .|. v2 `unsafeShiftL` 8
    final <- transform v
    tell [MarkEntry { 
            markName = txt,
            markShow = represent final,
            markOffset = offset,
            markSize = 2,
            markChildren = []
        }]

    pure final

markTransformWord16le :: Representable a
                      => (Word16 -> a) -> T.Text -> Marker a
markTransformWord16le transform =
    markTransformWord16leM (return . transform)

markWord16le :: T.Text -> Marker Word16
markWord16le = markTransformWord16le id

markTransformWord32beM :: Representable a
                       => (Word32 -> Marker a) -> T.Text -> Marker a
markTransformWord32beM transform txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    v3 <- fromIntegral <$> getByte <* incIndex
    v4 <- fromIntegral <$> getByte <* incIndex
    let v =  v1 `unsafeShiftL` 24
         .|. v2 `unsafeShiftL` 16 
         .|. v3 `unsafeShiftL` 8
         .|. v4
    final <- transform v

    tell [MarkEntry { 
            markName = txt,
            markShow = represent final,
            markOffset = offset,
            markSize = 4,
            markChildren = []
        }]

    pure final

markTransformWord32be :: Representable a
                      => (Word32 -> a) -> T.Text -> Marker a
markTransformWord32be transform =
    markTransformWord32beM (return . transform)

markWord32be :: T.Text -> Marker Word32
markWord32be = markTransformWord32be id

markTransformWord32leM :: Representable a
                       => (Word32 -> Marker a) -> T.Text -> Marker a
markTransformWord32leM transform txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    v3 <- fromIntegral <$> getByte <* incIndex
    v4 <- fromIntegral <$> getByte <* incIndex
    let v =  v4 `unsafeShiftL` 24
         .|. v3 `unsafeShiftL` 16 
         .|. v2 `unsafeShiftL` 8
         .|. v1
    final <- transform v

    tell [MarkEntry { 
            markName = txt,
            markShow = represent final,
            markOffset = offset,
            markSize = 4,
            markChildren = []
        }] 

    pure final

markTransformWord32le :: Representable a
                      => (Word32 -> a) -> T.Text -> Marker a
markTransformWord32le transform =
    markTransformWord32leM (return . transform)

markWord32le :: T.Text -> Marker Word32
markWord32le = markTransformWord32le id

markAllRemainingByte :: T.Text -> Marker B.ByteString
markAllRemainingByte txt = do
    offset <- gets markerIndex
    str <- asks setupData
    markByteString txt $ B.length str - offset

markByteString :: T.Text -> Int -> Marker B.ByteString
markByteString txt size = do
    offset <- gets markerIndex
    str <- asks setupData
    when (B.length str < offset + size) $
        throwError "Requiring too much data - markByteString"

    when (size > 0) $
        tell [MarkEntry {
                markName = txt,
                markShow = "<STR>",
                markOffset = offset,
                markSize = size,
                markChildren = []
            }]

    addIndex size
    pure . B.take size $ B.drop offset str

delimitateRegion :: (Representable a)
                 => T.Text -> (B.ByteString -> Bool) -> Marker a -> Marker a
delimitateRegion str predicate action = do
    offset <- gets markerIndex
    currentSetup <- ask
    let rawData = B.drop offset $ setupData currentSetup
        size = delimitate (0 :: Int) rawData
        subSetup = currentSetup { setupData = B.take size rawData }

        (v, MarkerState readedSize, history) =
            runRWS (runErrorT action) subSetup (MarkerState 0)
        teller val =
            when (readedSize > 0) $
                tell [MarkEntry {
                        markName = str,
                        markShow = val,
                        markOffset = offset,
                        markSize = readedSize,
                        markChildren = updateOffset offset <$> history
                    }]

    addIndex readedSize
    case v of
      Left err -> teller (T.pack err) >> throwError err
      Right val -> teller (represent val) >> return val

  where updateOffset ofs entry = entry { markOffset = ofs + markOffset entry }
        delimitate i currentStr
            | predicate currentStr = i
            | otherwise = case B.uncons currentStr of
                Just (_, rest) -> delimitate (i + 1) rest
                Nothing -> i + 1

subZone :: (Representable a) => T.Text -> Marker a -> Marker a
subZone txt action = do
    offset <- gets markerIndex
    currentSetup <- ask
    let (v, MarkerState endOffset, history) =
            runRWS (runErrorT action) currentSetup (MarkerState offset)

        teller value = when (endOffset - offset > 0) $
            tell [MarkEntry {
                    markName = txt,
                    markShow = value,
                    markOffset = offset,
                    markSize = endOffset - offset,
                    markChildren = history
                }]
    put $ MarkerState endOffset
    case v of
      Left err -> teller (T.pack err) >> throwError err
      Right val -> teller (represent val) >> return val

skipUntil :: (Word8 -> Bool) -> Marker ()
skipUntil predicate = do
   idx <- gets markerIndex
   rawData <- asks setupData
   let new_idx = delimitate rawData idx
   when (new_idx - idx > 0) $
    tell [MarkEntry
        { markName = "skipped"
        , markShow = ""
        , markOffset = idx
        , markSize = new_idx - idx
        , markChildren = []
        }]
   modify $ \s -> s { markerIndex = new_idx }
  where delimitate str i 
            | i >= B.length str = i
            | otherwise = case B.uncons str of
                Nothing -> i + 1
                Just (v, _) ->
                    if predicate v then i else delimitate str (i + 1)

