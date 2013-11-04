{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Marker( Marker
                  , Markeable( .. )
                  , markBytesRead
                  , mark4bitsEach
                  , markWord8
                  , markWord16be
                  , markWord16le
                  , markWord32be
                  , markWord32le
                  , markByteString
                  , delimitateRegion
                  , skipUntil
                  , renderByteDump
                  , subZone
                  ) where

import Control.Applicative( (<$>), (<*>), pure, (<*) )
import Control.Monad( when )
import Data.Monoid( mempty )
import Data.Bits( (.|.), (.&.)
                , unsafeShiftL
                , unsafeShiftR
                )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
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

instance Show a => Representable a where
    represent = T.pack . show

class Representable a => Markeable a where
    parseMark :: T.Text -> Marker a

instance Markeable Word8 where
    parseMark = markWord8


byteTexts :: V.Vector T.Text
byteTexts = V.fromListN (b + 1) [T.pack $ printf "%02X " v  | v <- [0 .. b]]
  where b = fromIntegral $ (maxBound :: Word8)

dumpDescription :: MarkEntry -> Markup
dumpDescription entry = H.div H.! A.class_ "hidden info" $ do
    H.span H.! A.class_ "name" $ toMarkup $ markName entry
    H.span H.! A.class_ "offset" $ toMarkup $ show $ markOffset entry
    H.span H.! A.class_ "size" $ toMarkup $ show $ markSize entry
    H.span H.! A.class_ "data" $ H.pre $ toMarkup $ groom $ markShow entry

dumpMarkers :: Int -> MarkerSetup -> [MarkEntry] -> Markup
dumpMarkers bytePerLine setup lst = fst $ markuper (zip [0 :: Int ..] lst) True 0
  where rawData = setupData setup

        markuper [] _ lineIndex = (mempty, lineIndex)
        markuper ((_, entry@MarkEntry { markChildren = _ : _ }) : rest)
          needEnclosing lineIndex =
              (children >> next, finalIndex)
            where (html, childIndex) =
                        markuper (zip [0..] $ markChildren entry) True lineIndex
                  children = H.span H.! A.class_ "subregion" $ html
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
                      finalMarkup | needEnclosing = H.span combinedMarkup
                                  | otherwise = combinedMarkup

                    

        markuper ((idx, entry) : rest) needEnclosing lineIndex = (markup >> nextMarkup, idx')
            where (nextMarkup, idx') = markuper rest needEnclosing $ lineIndex + size
                  offset = markOffset entry
                  size = markSize entry
                  bytes = [byteTexts ! fromIntegral (B.index rawData o) 
                                        | o <- [offset .. offset + size - 1]]
                  spanTag = H.span H.! A.id (H.toValue $ show idx)
                                   H.! A.class_ (H.toValue ("region" :: T.Text))
                                   H.! A.title (H.toValue $ markName entry)
                  markup
                    | not needEnclosing = toMarkup $ T.concat bytes
                    | otherwise = spanTag . toMarkup $ T.concat bytes

renderByteDump :: B.ByteString -> Marker a -> LB.ByteString
renderByteDump str action = renderMarkup document
  where setup = MarkerSetup { setupData = str, setupFilename = "" }
        (finalValue, MarkerState readedSize, history) =
            runRWS (runErrorT action) setup (MarkerState 0)

        txt = case finalValue of
          Left err -> "Error : " ++ err
          Right _v -> "OK"

        document = H.html $ do
            H.head $ do
                H.title "Hex dump"
                H.link H.! A.rel "stylesheet"
                       H.! A.type_ "text/css"
                       H.! A.href "hexmark.css"

            H.body $ do
                toMarkup $ "Readed " ++ show readedSize
                H.div $ toMarkup txt
                H.table $ H.tr $ do
                    H.td $ H.pre $ dumpMarkers 16 setup history
                    H.td $ mapM_ dumpDescription history

getByte :: Marker Word8
getByte = B.index <$> asks setupData <*> gets markerIndex

markBytesRead :: Marker Int
markBytesRead = gets markerIndex

addIndex :: Int -> Marker ()
addIndex size = lift . modify $ \s -> s { markerIndex = markerIndex s + size }

incIndex :: Marker ()
incIndex = addIndex 1

mark4bitsEach :: T.Text -> Marker (Word8, Word8)
mark4bitsEach txt = do
    v <- markWord8 txt
    return ((v `unsafeShiftR` 4) .&. 0xFF, v .&. 0xFF)

markWord8 :: T.Text -> Marker Word8
markWord8 txt = do
    offset <- gets markerIndex
    v <- getByte <* incIndex
    tell [MarkEntry { 
            markName = txt,
            markShow = represent v,
            markOffset = offset,
            markSize = 1,
            markChildren = []
        }]

    pure v

markWord16be :: T.Text -> Marker Word16
markWord16be txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    let v = v1 `unsafeShiftL` 8 .|. v2
    tell [MarkEntry { 
            markName = txt,
            markShow = represent v,
            markOffset = offset,
            markSize = 2,
            markChildren = []
        }] 

    pure v

markWord16le :: T.Text -> Marker Word16
markWord16le txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    let v = v1 .|. v2 `unsafeShiftL` 8
    tell [MarkEntry { 
            markName = txt,
            markShow = represent v,
            markOffset = offset,
            markSize = 2,
            markChildren = []
        }] 

    pure v

markWord32be :: T.Text -> Marker Word32
markWord32be txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    v3 <- fromIntegral <$> getByte <* incIndex
    v4 <- fromIntegral <$> getByte <* incIndex
    let v =  v1 `unsafeShiftL` 24
         .|. v2 `unsafeShiftL` 16 
         .|. v3 `unsafeShiftL` 8
         .|. v4

    tell [MarkEntry { 
            markName = txt,
            markShow = represent v,
            markOffset = offset,
            markSize = 4,
            markChildren = []
        }] 

    pure v

markWord32le :: T.Text -> Marker Word32
markWord32le txt = do
    offset <- gets markerIndex
    v1 <- fromIntegral <$> getByte <* incIndex
    v2 <- fromIntegral <$> getByte <* incIndex
    v3 <- fromIntegral <$> getByte <* incIndex
    v4 <- fromIntegral <$> getByte <* incIndex
    let v =  v4 `unsafeShiftL` 24
         .|. v3 `unsafeShiftL` 16 
         .|. v2 `unsafeShiftL` 8
         .|. v1

    tell [MarkEntry { 
            markName = txt,
            markShow = represent v,
            markOffset = offset,
            markSize = 4,
            markChildren = []
        }] 

    pure v

markByteString :: T.Text -> Int -> Marker B.ByteString
markByteString txt size = do
    offset <- gets markerIndex
    str <- asks setupData
    when (B.length str < offset + size) $
        throwError "Requiring too much data - markByteString"

    tell [MarkEntry {
            markName = txt,
            markShow = "<STR>",
            markOffset = offset,
            markSize = size,
            markChildren = []
        }]

    addIndex size
    pure . B.take size $ B.drop offset str

delimitateRegion :: (Show a) => T.Text -> (B.ByteString -> Bool) -> Marker a -> Marker a
delimitateRegion str predicate action = do
    offset <- gets markerIndex
    currentSetup <- ask
    let (_size, localString) = delimitate (0 :: Int) rawData
        rawData = setupData currentSetup
        subSetup = currentSetup { setupData = localString }
        (v, MarkerState readedSize, history) =
            runRWS (runErrorT action) subSetup (MarkerState 0)
    addIndex readedSize
    case v of
      Left err -> do
          tell [MarkEntry {
                    markName = str,
                    markShow = T.pack err,
                    markOffset = offset,
                    markSize = readedSize,
                    markChildren = updateOffset offset <$> history
                }]
          throwError err

      Right val -> do
          tell [MarkEntry {
                    markName = str,
                    markShow = represent val,
                    markOffset = offset,
                    markSize = readedSize,
                    markChildren = updateOffset offset <$> history
                }]

          return val

  where updateOffset ofs entry = entry { markOffset = ofs + markOffset entry }
        delimitate i currentStr
            | predicate currentStr = (i, currentStr)
            | otherwise = case B.uncons currentStr of
                Just (_, rest) -> delimitate (i + 1) rest
                Nothing -> (i + 1, B.empty)

subZone :: (Show a) => T.Text -> Marker a -> Marker a
subZone txt action = do
    offset <- gets markerIndex
    currentSetup <- ask
    let (v, MarkerState endOffset, history) =
            runRWS (runErrorT action) currentSetup (MarkerState offset)

        teller value =
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

