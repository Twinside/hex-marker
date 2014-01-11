{-# LANGUAGE OverloadedStrings #-}
module Data.Marker.Jpg( JpgImage, markJpeg ) where

import Control.Applicative( (<$>), (<*>) )
import Control.Monad( when, replicateM, forM )
import qualified Data.ByteString as B
import Data.Int(Int16)
import Data.Marker
import qualified Data.Text as T
import Data.Word

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
    , _jpgSamplePrecision     :: !Word8
    , _jpgHeight              :: !Word16
    , _jpgWidth               :: !Word16
    , _jpgImageComponentCount :: !Word8
    , _jpgComponents          :: ![JpgComponent]
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

instance Markeable JpgScanSpecification where
    parseMark txt = subZone txt $ do
        compSel <- markWord8 "Component selector"
        (dc, ac) <- mark4bitsEach "entropy table selctor"
        return JpgScanSpecification {
            componentSelector = compSel
          , dcEntropyCodingTable = dc
          , acEntropyCodingTable = ac
          }

instance Markeable JpgScanHeader where
    parseMark txt = subZone txt $ do
        thisScanLength <- markWord16be "scan length"
        compCount <- markWord8 "Component count"
        comp <- replicateM (fromIntegral compCount) (parseMark "Scan header component")
        specBeg <- markWord8 "spectral start"
        specEnd <- markWord8 "spectral end"
        (approxHigh, approxLow) <- mark4bitsEach "Approximation"

        return JpgScanHeader {
            scanLength = thisScanLength,
            scanComponentCount = compCount,
            scans = comp,
            spectralSelection = (specBeg, specEnd),
            successiveApproxHigh = approxHigh,
            successiveApproxLow = approxLow
        }

data JpgQuantTableSpec = JpgQuantTableSpec
    { -- | Stored on 4 bits
      quantPrecision     :: !Word8

      -- | Stored on 4 bits
    , quantDestination   :: !Word8

    , quantTable         :: [Int16]
    }
    deriving Show

data JpgFrame =
      JpgAppFrame        !Word8 B.ByteString
    | JpgExtension       !Word8 B.ByteString
    | JpgQuantTable      ![JpgQuantTableSpec]
    | JpgHuffmanTable    ![JpgHuffmanTableSpec]
    | JpgScanBlob        !JpgScanHeader !B.ByteString
    | JpgScans           !JpgFrameKind !JpgFrameHeader
    | JpgIntervalRestart !Word16
    deriving Show

data JpgHuffmanTableSpec = JpgHuffmanTableSpec
    { -- | 0 : DC, 1 : AC, stored on 4 bits
      huffmanTableClass       :: !DctComponent
      -- | Stored on 4 bits
    , huffmanTableDest        :: !Word8

    , huffSizes :: ![Word8]
    , huffCodes :: ![[Word8]]
    }
    deriving Show

data JpgImage = JpgImage { _jpgFrame :: [JpgFrame]}
    deriving Show

instance Markeable JpgFrameKind where
  -- no lookahead :(
  {-word <- getWord8-}
  parseMark _ = markTransformWord8 aux "JPG Frame kind"
    where aux word2 = case word2 of
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

markGetCount :: (Show a, Markeable a) => T.Text -> T.Text -> Marker [a]
markGetCount txt elementText = subZone txt $ do
    count <- fromIntegral <$> markWord16be "count"
    inner (count - 2 :: Int)
  where inner 0 = return []
        inner n | n < 0 = fail "Out of allowed size"
        inner size = do
            onStart <- markBytesRead
            el <- parseMark elementText
            onEnd <- markBytesRead
            (el :) <$> inner (size - (onEnd - onStart))

instance Markeable JpgQuantTableSpec where
    parseMark txt = subZone txt $ do
        (precision, dest) <- mark4bitsEach "Quant precision & dest"
        coeffs <- replicateM 64 $ if precision == 0
                then fromIntegral <$> markWord8 "Quant coeff 8bits"
                else fromIntegral <$> markWord16be "Quant coeff 16bit"
        return JpgQuantTableSpec
            { quantPrecision = precision
            , quantDestination = dest
            , quantTable = coeffs
            }

takeCurrentFrame :: T.Text -> Marker B.ByteString
takeCurrentFrame txt = do
    size <- fromIntegral <$> markWord16be "Frame size"
    markByteString txt $ size - 2

-- | Enumeration used to search in the tables for different components.
data DctComponent = DcComponent | AcComponent
    deriving (Eq, Show)

instance Markeable JpgHuffmanTableSpec where
    parseMark txt = subZone txt $ do
        (huffClass, huffDest) <- mark4bitsEach "huff class & dest"
        sizes <- subZone "Huffman Sizes" $ replicateM 16 (markWord8 "size")
        codes <- forM sizes $ \s -> subZone "Huffman coeffs" $
            replicateM (fromIntegral s) (markWord8 "Huffman coeff")
        return JpgHuffmanTableSpec
            { huffmanTableClass =
                if huffClass == 0 then DcComponent else AcComponent
            , huffmanTableDest = huffDest
            , huffSizes = sizes
            , huffCodes = codes
            }

isScanContent :: B.ByteString -> Bool
isScanContent str
    | B.length str < 2 = False
    | otherwise = v1 == 0xFF && v2 /= 0 && not isReset
        where v1 = B.index str 0
              v2 = B.index str 1
              isReset = 0xD0 <= v2 && v2 <= 0xD7

newtype RestartInterval = RestartInterval Word16
    deriving Show

instance Markeable RestartInterval where
    parseMark txt = subZone txt $ do
        size <- markWord16be "restart block size (must be 4)"
        when (size /= 4) (fail "Invalid jpeg restart interval size")
        RestartInterval <$> markWord16be "Restart interval value"

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
        JpgQuantizationTable ->
            (\quants lst -> JpgQuantTable quants : lst)
                    <$> markGetCount "Quantization tables" "Quant table"
                    <*> parseNextFrame
        JpgHuffmanTableMarker ->
            (\huffTables lst -> JpgHuffmanTable huffTables : lst)
                    <$> markGetCount "Huffman tables" "Huffman table"
                    <*> parseNextFrame
        JpgAppSegment c ->
            (\frm lst -> JpgAppFrame c frm : lst)
                    <$> takeCurrentFrame "App segment data"
                    <*> parseNextFrame
        JpgExtensionSegment c ->
            (\frm lst -> JpgExtension c frm : lst)
                    <$> takeCurrentFrame "Extension segment data"
                    <*> parseNextFrame
        JpgRestartInterval ->
            (\(RestartInterval i) lst -> JpgIntervalRestart i : lst)
                    <$> (parseMark "Restart interval") <*> parseNextFrame
        JpgStartOfScan -> do
            frm <- parseMark "scan header"
            sub <- delimitateRegion "Scan content" isScanContent
                        (markAllRemainingByte "bytes")

            (JpgScanBlob frm sub :) <$> parseNextFrame

        _ -> (\hdr lst -> JpgScans kind hdr : lst)
                    <$> parseMark "JpgScans hdr"  <*> parseNextFrame


instance Markeable JpgImage where
    parseMark _txt = do
        v <- markWord16be "Jpeg start marker"
        when (v /= 0xFFD8)
             (fail "Invalid Jpeg start marker")
        skipUntil (== 0xFF)
        _ <- markWord8 "Marker start"
        JpgImage <$> parseFrames 

markJpeg :: Marker JpgImage
markJpeg = parseMark ""

