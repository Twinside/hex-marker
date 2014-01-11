{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Marker.Tiff( TiffInfo, markTiff ) where

import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad( when, replicateM )
import Data.Word( Word16 )
import Data.Bits( (.&.), unsafeShiftR )
import Data.Marker
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word( Word32 )
import qualified Data.ByteString as B

data Endianness = EndianLittle
                | EndianBig
                deriving (Eq, Show)

instance Markeable Endianness where
    parseMark _ = do
        tag <- markWord16le "Endianness format"
        case tag of
            0x4949 -> return EndianLittle
            0x4D4D -> return EndianBig
            _ -> fail "Invalid endian tag value"

-- | Because having a polymorphic get with endianness is to nice
-- to pass on, introducing this helper type class, which is just
-- a superset of Binary, but formalising a parameter passing
-- into it.
class MarkeableParam a b where
    getP :: T.Text -> a -> Marker b

data TiffHeader = TiffHeader
    { hdrEndianness :: !Endianness
    , hdrOffset     :: {-# UNPACK #-} !Word32
    }
    deriving (Eq, Show)

instance MarkeableParam Endianness Word16 where
    getP t EndianLittle = markWord16le t
    getP t EndianBig = markWord16be t

instance MarkeableParam Endianness Word32 where
    getP t EndianLittle = markWord32le t
    getP t EndianBig = markWord32be t

instance Markeable TiffHeader where
    parseMark t = subZone t $ do
        endian <- parseMark ""
        magic <- getP "Magic number" endian
        let magicValue = 42 :: Word16
        when (magic /= magicValue)
             (fail "Invalid TIFF magic number")
        TiffHeader endian <$> getP "hdr offset" endian

data TiffPlanarConfiguration =
      PlanarConfigContig    -- = 1
    | PlanarConfigSeparate  -- = 2
    deriving Show

planarConfgOfConstant :: Word32 -> Marker TiffPlanarConfiguration
planarConfgOfConstant 0 = pure PlanarConfigContig
planarConfgOfConstant 1 = pure PlanarConfigContig
planarConfgOfConstant 2 = pure PlanarConfigSeparate
planarConfgOfConstant v = fail $ "Unknown planar constant (" ++ show v ++ ")"

data TiffCompression =
      CompressionNone           -- 1
    | CompressionModifiedRLE    -- 2
    | CompressionLZW            -- 5
    | CompressionJPEG           -- 6
    | CompressionPackBit        -- 32273
    deriving Show

data IfdType = TypeByte
             | TypeAscii
             | TypeShort
             | TypeLong
             | TypeRational
             | TypeSByte
             | TypeUndefined
             | TypeSignedShort
             | TypeSignedLong
             | TypeSignedRational
             | TypeFloat
             | TypeDouble
             deriving Show

instance MarkeableParam Endianness IfdType where
    getP _ endianness = getP "ifdType" endianness >>= conv
      where
        conv :: Word16 -> Marker IfdType
        conv 1  = return TypeByte
        conv 2  = return TypeAscii
        conv 3  = return TypeShort
        conv 4  = return TypeLong
        conv 5  = return TypeRational
        conv 6  = return TypeSByte
        conv 7  = return TypeUndefined
        conv 8  = return TypeSignedShort
        conv 9  = return TypeSignedLong
        conv 10 = return TypeSignedRational
        conv 11 = return TypeFloat
        conv 12 = return TypeDouble
        conv _  = fail "Invalid TIF directory type"

data TiffTag = TagPhotometricInterpretation
             | TagCompression -- ^ Short type
             | TagImageWidth  -- ^ Short or long type
             | TagImageLength -- ^ Short or long type
             | TagXResolution -- ^ Rational type
             | TagYResolution -- ^ Rational type
             | TagResolutionUnit --  ^ Short type
             | TagRowPerStrip -- ^ Short or long type
             | TagStripByteCounts -- ^ Short or long
             | TagStripOffsets -- ^ Short or long
             | TagBitsPerSample --  ^ Short
             | TagColorMap -- ^ Short
             | TagTileWidth
             | TagTileLength
             | TagTileOffset
             | TagTileByteCount
             | TagSamplesPerPixel -- ^ Short
             | TagArtist
             | TagDocumentName
             | TagSoftware
             | TagPlanarConfiguration -- ^ Short
             | TagOrientation
             | TagSampleFormat -- ^ Short
             | TagInkSet
             | TagSubfileType
             | TagFillOrder
             | TagYCbCrCoeff
             | TagYCbCrSubsampling
             | TagYCbCrPositioning
             | TagReferenceBlackWhite
             | TagXPosition
             | TagYPosition
             | TagExtraSample
             | TagImageDescription

             | TagJpegProc
             | TagJPEGInterchangeFormat
             | TagJPEGInterchangeFormatLength
             | TagJPEGRestartInterval
             | TagJPEGLosslessPredictors
             | TagJPEGPointTransforms
             | TagJPEGQTables
             | TagJPEGDCTables
             | TagJPEGACTables

             | TagUnknown Word16
             deriving (Eq, Show)

tagOfWord16 :: Word16 -> TiffTag
tagOfWord16 = aux
  where aux 255 = TagSubfileType
        aux 256 = TagImageWidth
        aux 257 = TagImageLength
        aux 258 = TagBitsPerSample
        aux 259 = TagCompression
        aux 262 = TagPhotometricInterpretation
        aux 266 = TagFillOrder
        aux 269 = TagDocumentName
        aux 270 = TagImageDescription
        aux 273 = TagStripOffsets
        aux 274 = TagOrientation
        aux 277 = TagSamplesPerPixel
        aux 278 = TagRowPerStrip
        aux 279 = TagStripByteCounts
        aux 282 = TagXResolution
        aux 283 = TagYResolution
        aux 284 = TagPlanarConfiguration
        aux 286 = TagXPosition
        aux 287 = TagYPosition
        aux 296 = TagResolutionUnit
        aux 305 = TagSoftware
        aux 315 = TagArtist
        aux 320 = TagColorMap
        aux 322 = TagTileWidth
        aux 323 = TagTileLength
        aux 324 = TagTileOffset
        aux 325 = TagTileByteCount
        aux 332 = TagInkSet
        aux 338 = TagExtraSample
        aux 339 = TagSampleFormat
        aux 529 = TagYCbCrCoeff
        aux 512 = TagJpegProc
        aux 513 = TagJPEGInterchangeFormat
        aux 514 = TagJPEGInterchangeFormatLength
        aux 515 = TagJPEGRestartInterval
        aux 517 = TagJPEGLosslessPredictors
        aux 518 = TagJPEGPointTransforms
        aux 519 = TagJPEGQTables
        aux 520 = TagJPEGDCTables
        aux 521 = TagJPEGACTables
        aux 530 = TagYCbCrSubsampling
        aux 531 = TagYCbCrPositioning
        aux 532 = TagReferenceBlackWhite
        aux v = TagUnknown v

instance MarkeableParam Endianness TiffTag where
  getP _ endianness = tagOfWord16 <$> getP "TiffTag" endianness

data ExtendedDirectoryData =
      ExtendedDataNone
    | ExtendedDataAscii !B.ByteString
    | ExtendedDataShort !(V.Vector Word16)
    | ExtendedDataLong  !(V.Vector Word32)
    deriving (Eq, Show)

instance MarkeableParam (Endianness, ImageFileDirectory) ExtendedDirectoryData where
  getP t (endianness, ifd) = subZone t $ fetcher ifd
    where
      align ImageFileDirectory { ifdOffset = offset } = do
        readed <- markBytesRead
        _ <- markByteString "padding" . fromIntegral
                                      $ fromIntegral offset - readed
        return ()

      getE :: (MarkeableParam Endianness a) => Marker a
      getE = getP "extended data" endianness

      getVec count = V.replicateM (fromIntegral count)

      fetcher ImageFileDirectory { ifdType = TypeAscii, ifdCount = count } | count > 1 =
          align ifd >> (ExtendedDataAscii <$> markByteString "extended string" (fromIntegral count))
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = 2, ifdOffset = ofs } =
          pure . ExtendedDataShort $ V.fromListN 2 valList
            where high = fromIntegral $ ofs `unsafeShiftR` 16
                  low = fromIntegral $ ofs .&. 0xFFFF
                  valList = case endianness of
                    EndianLittle -> [low, high]
                    EndianBig -> [high, low]
      fetcher ImageFileDirectory { ifdType = TypeShort, ifdCount = count } | count > 2 =
          align ifd >> (ExtendedDataShort <$> getVec count getE)
      fetcher ImageFileDirectory { ifdType = TypeLong, ifdCount = count } | count > 1 =
          align ifd >> (ExtendedDataLong <$> getVec count getE)
      fetcher _ = pure ExtendedDataNone

data TiffSampleFormat =
      TiffSampleUint
    | TiffSampleInt
    | TiffSampleDouble
    | TiffSampleUnknown
    deriving (Eq, Show)

unpackSampleFormat :: Word32 -> Marker TiffSampleFormat
unpackSampleFormat = aux
  where
    aux 1 = pure TiffSampleUint
    aux 2 = pure TiffSampleInt
    aux 3 = pure TiffSampleDouble
    aux 4 = pure TiffSampleUnknown
    aux v = fail $ "Undefined data format (" ++ show v ++ ")"

data ImageFileDirectory = ImageFileDirectory
    { ifdIdentifier :: !TiffTag
    , ifdType       :: !IfdType
    , ifdCount      :: !Word32
    , ifdOffset     :: !Word32
    , ifdExtended   :: !ExtendedDirectoryData
    }
    deriving Show

unLong :: String -> ExtendedDirectoryData -> Marker (V.Vector Word32)
unLong _ (ExtendedDataShort v) = pure $ V.map fromIntegral v
unLong _ (ExtendedDataLong v) = pure v
unLong errMessage _ = fail errMessage

cleanImageFileDirectory :: ImageFileDirectory -> ImageFileDirectory
cleanImageFileDirectory ifd@(ImageFileDirectory { ifdCount = 1 }) = aux $ ifdType ifd
    where aux TypeShort = ifd { ifdOffset = ifdOffset ifd `unsafeShiftR` 16 }
          aux _ = ifd
cleanImageFileDirectory ifd = ifd

instance MarkeableParam Endianness ImageFileDirectory where
  getP _t endianness = subZone "imagefiledirectory" $
    ImageFileDirectory <$> getE "tag" <*> getE "type" <*> getE "count" <*> getE "offset"
                       <*> pure ExtendedDataNone
        where getE :: (MarkeableParam Endianness a) => T.Text -> Marker a
              getE t = getP t endianness

instance MarkeableParam Endianness [ImageFileDirectory] where
  getP t endianness = subZone t $ do
    count <- getP "Count" endianness :: Marker Word16
    rez <- replicateM (fromIntegral count) $ getP "ImageFileDirectory" endianness
    _ <- getP "Unknown" endianness :: Marker Word32
    pure rez
  
fetchExtended :: Endianness -> [ImageFileDirectory] -> Marker [ImageFileDirectory]
fetchExtended endian = mapM $ \ifd -> do
        v <- getP "extended info" (endian, ifd)
        pure $ ifd { ifdExtended = v }

findIFD :: String -> TiffTag -> [ImageFileDirectory]
        -> Marker ImageFileDirectory
findIFD errorMessage tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> fail errorMessage
        (x:_) -> pure x

findPalette :: [ImageFileDirectory] -> Marker (Maybe a)
findPalette _ = return Nothing

findIFDData :: String -> TiffTag -> [ImageFileDirectory] -> Marker Word32
findIFDData msg tag lst = ifdOffset <$> findIFD msg tag lst

findIFDDefaultData :: Word32 -> TiffTag -> [ImageFileDirectory] -> Marker Word32
findIFDDefaultData d tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> pure d
        (x:_) -> pure $ ifdOffset x

findIFDExt :: String -> TiffTag -> [ImageFileDirectory]
           -> Marker ExtendedDirectoryData
findIFDExt msg tag lst = do
    val <- findIFD msg tag lst
    case val of
      ImageFileDirectory
        { ifdCount = 1, ifdOffset = ofs, ifdType = TypeShort } ->
               pure . ExtendedDataShort . V.singleton $ fromIntegral ofs
      ImageFileDirectory
        { ifdCount = 1, ifdOffset = ofs, ifdType = TypeLong } ->
               pure . ExtendedDataLong . V.singleton $ fromIntegral ofs
      ImageFileDirectory { ifdExtended = v } -> pure v


findIFDExtDefaultData :: [Word32] -> TiffTag -> [ImageFileDirectory]
                      -> Marker [Word32]
findIFDExtDefaultData d tag lst =
    case [v | v <- lst, ifdIdentifier v == tag] of
        [] -> pure d
        (ImageFileDirectory { ifdExtended = ExtendedDataNone }:_) -> return d
        (x:_) -> V.toList <$> unLong errorMessage (ifdExtended x)
            where errorMessage =
                    "Can't parse tag " ++ show tag ++ " " ++ show (ifdExtended x)

data TiffInfo = TiffInfo
    { _tiffHeader             :: TiffHeader
    , _tiffWidth              :: Word32
    , _tiffHeight             :: Word32
    , _tiffColorspace         :: TiffColorspace
    , _tiffSampleCount        :: Word32
    , _tiffRowPerStrip        :: Word32
    , _tiffPlaneConfiguration :: TiffPlanarConfiguration
    , _tiffSampleFormat       :: [TiffSampleFormat]
    , _tiffBitsPerSample      :: V.Vector Word32
    , _tiffCompression        :: TiffCompression
    , _tiffStripSize          :: V.Vector Word32
    , _tiffOffsets            :: V.Vector Word32
    , _tiffPalette            :: Maybe Int
    , _tiffYCbCrSubsampling   :: V.Vector Word32
    }
    deriving Show

data TiffColorspace =
      TiffMonochromeWhite0 -- ^ 0
    | TiffMonochrome       -- ^ 1
    | TiffRGB              -- ^ 2
    | TiffPaleted          -- ^ 3
    | TiffTransparencyMask -- ^ 4
    | TiffCMYK             -- ^ 5
    | TiffYCbCr            -- ^ 6
    | TiffCIELab           -- ^ 8
    deriving Show

unpackPhotometricInterpretation :: Word32 -> Marker TiffColorspace
unpackPhotometricInterpretation = aux
  where aux 0 = pure TiffMonochromeWhite0
        aux 1 = pure TiffMonochrome
        aux 2 = pure TiffRGB
        aux 3 = pure TiffPaleted
        aux 4 = pure TiffTransparencyMask
        aux 5 = pure TiffCMYK
        aux 6 = pure TiffYCbCr
        aux 8 = pure TiffCIELab
        aux v = fail $ "Unrecognized color space " ++ show v

unPackCompression :: Word32 -> Marker TiffCompression
unPackCompression = aux
  where
    aux 0 = pure CompressionNone
    aux 1 = pure CompressionNone
    aux 2 = pure CompressionModifiedRLE
    aux 5 = pure CompressionLZW
    aux 6 = pure CompressionJPEG
    aux 32773 = pure CompressionPackBit
    aux v = fail $ "Unknown compression scheme " ++ show v

instance MarkeableParam B.ByteString TiffInfo where
  getP t _ = subZone t $ do
    hdr <- parseMark "Header"
    readed <- markBytesRead
    _ <- markByteString "Padding" 
                . fromIntegral $ fromIntegral (hdrOffset hdr) - readed
    let endian = hdrEndianness hdr

    ifd <- fmap cleanImageFileDirectory <$> getP "Ifds" endian
    cleaned <- fetchExtended endian ifd

    let dataFind str tag = findIFDData str tag cleaned
        dataDefault def tag = findIFDDefaultData def tag cleaned
        extFind str tag = findIFDExt str tag cleaned
        extDefault def tag = findIFDExtDefaultData def tag cleaned

    TiffInfo hdr
        <$> dataFind "Can't find width" TagImageWidth
        <*> dataFind "Can't find height" TagImageLength
        <*> (dataFind "Can't find color space" TagPhotometricInterpretation
                     >>= unpackPhotometricInterpretation)
        <*> dataFind "Can't find sample per pixel" TagSamplesPerPixel
        <*> dataFind "Can't find row per strip" TagRowPerStrip
        <*> (dataDefault 1 TagPlanarConfiguration
                     >>= planarConfgOfConstant)
        <*> (extDefault [1] TagSampleFormat
                     >>= mapM unpackSampleFormat)
        <*> (extFind "Can't find bit per sample" TagBitsPerSample
                     >>= unLong "Can't find bit depth")
        <*> (dataFind "Can't find Compression" TagCompression
                     >>= unPackCompression)
        <*> (extFind "Can't find byte counts" TagStripByteCounts
                     >>= unLong "Can't find bit per sample")
        <*> (extFind "Strip offsets missing" TagStripOffsets
                     >>= unLong "Can't find strip offsets")
        <*> findPalette cleaned
        <*> (V.fromList <$> extDefault [2, 2] TagYCbCrSubsampling)

markTiff :: B.ByteString -> Marker TiffInfo
markTiff f = getP "TiffFile" f

