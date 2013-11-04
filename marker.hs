{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative( (<$>), (<*>) )
import Control.Monad( when, replicateM )
import Data.Marker
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.Environment( getArgs )

data JpgComponent = JpgComponent
    { componentIdentifier       :: !Word8
      -- | Stored with 4 bits
    , horizontalSamplingFactor  :: !Word8
      -- | Stored with 4 bits
    , verticalSamplingFactor    :: !Word8
    , quantizationTableDest     :: !Word8
    }
    deriving Show

data JpgFrameHeader = JpgFrameHeader
    { jpgFrameHeaderLength   :: !Word16
    , jpgSamplePrecision     :: !Word8
    , jpgHeight              :: !Word16
    , jpgWidth               :: !Word16
    , jpgImageComponentCount :: !Word8
    , jpgComponents          :: ![JpgComponent]
    }
    deriving Show
instance Markeable JpgComponent where
    parseMark txt = subZone txt $ do
        ident <- markWord8 "compnent identifier"
        (horiz, vert) <- mark4bitsEach "sampling factor"
        quantTableIndex <- markWord8 "Quant table index"
        return JpgComponent
            { componentIdentifier = ident
            , horizontalSamplingFactor = horiz
            , verticalSamplingFactor = vert
            , quantizationTableDest = quantTableIndex
            }

instance Markeable JpgFrameHeader where
    parseMark txt = subZone txt $ do
        beginOffset <- markBytesRead
        hdr <- JpgFrameHeader
            <$> markWord16be "frame length"
            <*> markWord8 "Sample precision"
            <*> markWord16be "Height"
            <*> markWord16be "Width"

        compCount <- markWord8 "component count"
        finalHdr <- hdr compCount
                 <$> replicateM (fromIntegral compCount) (parseMark "comp descr")

        endOffset <- markBytesRead
        let frameHdrLength = fromIntegral (jpgFrameHeaderLength finalHdr)
        when (beginOffset - endOffset < frameHdrLength) $ do
           _ <- markByteString "padding" $
                    frameHdrLength - (endOffset - beginOffset)
           return ()

        return finalHdr

data JpgFrameKind =
      JpgBaselineDCTHuffman
    | JpgExtendedSequentialDCTHuffman
    | JpgProgressiveDCTHuffman
    | JpgLosslessHuffman
    | JpgDifferentialSequentialDCTHuffman
    | JpgDifferentialProgressiveDCTHuffman
    | JpgDifferentialLosslessHuffman
    | JpgExtendedSequentialArithmetic
    | JpgProgressiveDCTArithmetic
    | JpgLosslessArithmetic
    | JpgDifferentialSequentialDCTArithmetic
    | JpgDifferentialProgressiveDCTArithmetic
    | JpgDifferentialLosslessArithmetic
    | JpgQuantizationTable
    | JpgHuffmanTableMarker
    | JpgStartOfScan
    | JpgEndOfImage
    | JpgAppSegment Word8
    | JpgExtensionSegment Word8

    | JpgRestartInterval
    | JpgRestartIntervalEnd Word8
    deriving (Eq, Show)

data JpgScanSpecification = JpgScanSpecification
    { componentSelector :: !Word8
      -- | Encoded as 4 bits
    , dcEntropyCodingTable :: !Word8
      -- | Encoded as 4 bits
    , acEntropyCodingTable :: !Word8

    }
    deriving Show

data JpgScanHeader = JpgScanHeader
    { scanLength :: !Word16
    , scanComponentCount :: !Word8
    , scans :: [JpgScanSpecification]

      -- | (begin, end)
    , spectralSelection    :: (Word8, Word8)

      -- | Encoded as 4 bits
    , successiveApproxHigh :: !Word8

      -- | Encoded as 4 bits
    , successiveApproxLow :: !Word8
    }
    deriving Show

data JpgFrame =
      JpgAppFrame        !Word8 B.ByteString
    | JpgExtension       !Word8 B.ByteString
    {-| JpgQuantTable      ![JpgQuantTableSpec]-}
    {-| JpgHuffmanTable    ![(JpgHuffmanTableSpec, HuffmanTreeInfo)]-}
    | JpgScanBlob        !JpgScanHeader !L.ByteString
    | JpgScans           !JpgFrameKind !JpgFrameHeader
    | JpgIntervalRestart !Word16
    deriving Show

data JpgImage = JpgImage { jpgFrame :: [JpgFrame]}
    deriving Show

instance Markeable JpgFrameKind where
    parseMark _ = do
        -- no lookahead :(
        {-word <- getWord8-}
        word2 <- markWord8 "JPG Frame kind"
        return $ case word2 of
            0xC0 -> JpgBaselineDCTHuffman
            0xC1 -> JpgExtendedSequentialDCTHuffman
            0xC2 -> JpgProgressiveDCTHuffman
            0xC3 -> JpgLosslessHuffman
            0xC4 -> JpgHuffmanTableMarker
            0xC5 -> JpgDifferentialSequentialDCTHuffman
            0xC6 -> JpgDifferentialProgressiveDCTHuffman
            0xC7 -> JpgDifferentialLosslessHuffman
            0xC9 -> JpgExtendedSequentialArithmetic
            0xCA -> JpgProgressiveDCTArithmetic
            0xCB -> JpgLosslessArithmetic
            0xCD -> JpgDifferentialSequentialDCTArithmetic
            0xCE -> JpgDifferentialProgressiveDCTArithmetic
            0xCF -> JpgDifferentialLosslessArithmetic
            0xD9 -> JpgEndOfImage
            0xDA -> JpgStartOfScan
            0xDB -> JpgQuantizationTable
            0xDD -> JpgRestartInterval
            a | a >= 0xF0 -> JpgExtensionSegment a
              | a >= 0xE0 -> JpgAppSegment a
              | a >= 0xD0 && a <= 0xD7 -> JpgRestartIntervalEnd a
              | otherwise -> error ("Invalid frame marker (" ++ show a ++ ")")


parseFrames :: Marker [JpgFrame]
parseFrames = do
    kind <- parseMark "" :: Marker JpgFrameKind
    let parseNextFrame = do
            word <- markWord8 "JPG Mark starter"
            when (word /= 0xFF) $ do
                fail $ "Invalid Frame marker (" ++ show word ++ ")"
            parseFrames
    case kind of
        JpgEndOfImage -> return []
{-  
        JpgAppSegment c ->
            trace "AppSegment" $
            (\frm lst -> JpgAppFrame c frm : lst) <$> takeCurrentFrame <*> parseNextFrame
        JpgExtensionSegment c ->
            trace "ExtSegment" $
            (\frm lst -> JpgExtension c frm : lst) <$> takeCurrentFrame <*> parseNextFrame
        JpgQuantizationTable ->
            trace "QuantTable" $
            (\(TableList quants) lst -> JpgQuantTable quants : lst) <$> get <*> parseNextFrame
        JpgRestartInterval ->
            trace "RestartInterval" $
            (\(RestartInterval i) lst -> JpgIntervalRestart i : lst) <$> get <*> parseNextFrame
        JpgHuffmanTableMarker ->
            trace "HuffmanTable" $
            (\(TableList huffTables) lst ->
                    JpgHuffmanTable [(t, packHuffmanTree . buildPackedHuffmanTree $ huffCodes t) | t <- huffTables] : lst)
                    <$> get <*> parseNextFrame
        JpgStartOfScan ->
            trace "StartOfScan" $
            (\frm imgData ->
                let (d, other) = extractScanContent imgData
                in
                case runGet parseFrames (L.drop 1 other) of
                  Left _ -> [JpgScanBlob frm d]
                  Right lst -> JpgScanBlob frm d : lst
            ) <$> get <*> getRemainingLazyBytes

-}
        _ -> (\hdr lst -> JpgScans kind hdr : lst)
                    <$> parseMark "JpgScans hdr"  <*> parseNextFrame


instance Markeable JpgImage where
    parseMark txt = do
        v <- markWord16be "Jpeg start marker"
        when (v /= 0xFFD8)
             (fail "Invalid Jpeg start marker")
        skipUntil (== 0xFF)
        _ <- markWord8 "Marker start"
        JpgImage <$> parseFrames 

main :: IO ()
main = do
    args <- getArgs
    case args of
      (filename:outfile:_) -> do
          f <- B.readFile filename
          let out = renderByteDump f (parseMark "" :: Marker JpgImage)
          L.writeFile outfile out

      _ -> putStrLn "not enough arguments"

