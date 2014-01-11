{-# LANGUAGE OverloadedStrings #-}
-- | Module implementing GIF decoding.
module Data.Marker.Gif
       ( GifImage
       , markGif
       ) where

import Control.Applicative( pure, (<$>), (<*>) )
import Control.Monad( replicateM_ )

import Data.Bits( (.&.), unsafeShiftR, testBit )
import Data.Word( Word8, Word16 )
import qualified Data.ByteString as B
import Data.Marker

{-
   <GIF Data Stream> ::=     Header <Logical Screen> <Data>* Trailer

   <Logical Screen> ::=      Logical Screen Descriptor [Global Color Table]

   <Data> ::=                <Graphic Block>  |
                             <Special-Purpose Block>

   <Graphic Block> ::=       [Graphic Control Extension] <Graphic-Rendering Block>

   <Graphic-Rendering Block> ::=  <Table-Based Image>  |
                                  Plain Text Extension

   <Table-Based Image> ::=   Image Descriptor [Local Color Table] Image Data

   <Special-Purpose Block> ::=    Application Extension  |
                                  Comment Extension
 -}

--------------------------------------------------
----            GifVersion
--------------------------------------------------
data GifVersion = GIF87a | GIF89a
    deriving Show

gif87aSignature, gif89aSignature :: B.ByteString
gif87aSignature = "GIF87a"
gif89aSignature = "GIF89a"

instance Markeable GifVersion where
    parseMark _ = do
        sig <- markByteString "GIF signature" (B.length gif87aSignature)
        case (sig == gif87aSignature, sig == gif89aSignature) of
            (True, _)  -> pure GIF87a
            (_ , True) -> pure GIF89a
            _          -> fail $ "Invalid Gif signature : " ++ (toEnum . fromEnum <$> B.unpack sig)


--------------------------------------------------
----         LogicalScreenDescriptor
--------------------------------------------------
-- | Section 18 of spec-gif89a
data LogicalScreenDescriptor = LogicalScreenDescriptor
  { -- | Stored on 16 bits
    screenWidth           :: !Word16
    -- | Stored on 16 bits
  , screenHeight          :: !Word16
    -- | Stored on 8 bits
  , backgroundIndex       :: !Word8

  -- | Stored on 1 bit
  , hasGlobalMap          :: !Bool
  -- | Stored on 3 bits
  , colorResolution       :: !Word8
  -- | Stored on 1 bit
  , isColorTableSorted    :: !Bool
  -- | Stored on 3 bits
  , colorTableSize        :: !Word8
  }
  deriving Show

instance Markeable LogicalScreenDescriptor where
    parseMark txt = subZone txt $ do
        w <- markWord16le "Width"
        h <- markWord16le "Height"
        packedField  <- markWord8 "Packed field"
        backgroundColorIndex  <- markWord8 "Background color index"
        _aspectRatio  <- markWord8 "Aspect ratio"
        return LogicalScreenDescriptor
            { screenWidth           = w
            , screenHeight          = h
            , hasGlobalMap          = packedField `testBit` 7
            , colorResolution       = (packedField `unsafeShiftR` 5) .&. 0x7 + 1
            , isColorTableSorted    = packedField `testBit` 3
            , colorTableSize        = (packedField .&. 0x7) + 1
            , backgroundIndex       = backgroundColorIndex
            }


--------------------------------------------------
----            ImageDescriptor
--------------------------------------------------
-- | Section 20 of spec-gif89a
data ImageDescriptor = ImageDescriptor
  { gDescPixelsFromLeft         :: !Word16
  , gDescPixelsFromTop          :: !Word16
  , gDescImageWidth             :: !Word16
  , gDescImageHeight            :: !Word16
  , gDescHasLocalMap            :: !Bool
  , gDescIsInterlaced           :: !Bool
  , gDescIsImgDescriptorSorted  :: !Bool
  , gDescLocalColorTableSize    :: !Word8
  }
  deriving Show

imageSeparator, extensionIntroducer, gifTrailer :: Word8
imageSeparator      = 0x2C
extensionIntroducer = 0x21
gifTrailer          = 0x3B

graphicControlLabel :: Word8
graphicControlLabel = 0xF9

parseDataBlocks :: Marker B.ByteString
parseDataBlocks = subZone "data blocks"
                $ B.concat <$> (markWord8 "block size" >>= aux)
 where aux    0 = pure []
       aux size = (:) <$> markByteString "block data" (fromIntegral size)
                      <*> (markWord8 "block size" >>= aux)


data GraphicControlExtension = GraphicControlExtension
    { gceDisposalMethod        :: !Word8 -- ^ Stored on 3 bits
    , gceUserInputFlag         :: !Bool
    , gceTransparentFlag       :: !Bool
    , gceDelay                 :: !Word16
    , gceTransparentColorIndex :: !Word8
    }
    deriving Show

instance Markeable GraphicControlExtension where
    parseMark txt = subZone txt $ do
        -- due to missing lookahead
        {-_extensionLabel  <- getWord8-}
        _size            <- markWord8 "size"
        packedFields     <- markWord8 "Packed field"
        delay            <- markWord16le "Delay"
        idx              <- markWord8 "Index"
        _blockTerminator <- markWord8 "Block terminator"
        return GraphicControlExtension
            { gceDisposalMethod        = (packedFields `unsafeShiftR` 2) .&. 0x07
            , gceUserInputFlag         = packedFields `testBit` 1
            , gceTransparentFlag       = packedFields `testBit` 0
            , gceDelay                 = delay
            , gceTransparentColorIndex = idx
            }

data GifImage = GifImage
    { _imgDescriptor   :: !ImageDescriptor
    , _imgLocalPalette :: !(Maybe Palette)
    , _imgLzwRootSize  :: !Word8
    , _imgData         :: B.ByteString
    }
    deriving Show

instance Markeable GifImage where
    parseMark txt = subZone txt $ do
        desc <- parseMark "Descriptor"
        let hasLocalColorTable = gDescHasLocalMap desc
        palette <- if hasLocalColorTable
           then Just <$> getPalette (gDescLocalColorTableSize desc)
           else pure Nothing

        GifImage desc palette <$> markWord8 "lzw root size"
                              <*> parseDataBlocks

data Block = BlockImage GifImage
           | BlockGraphicControl GraphicControlExtension

parseGifBlocks :: Marker [Block]
parseGifBlocks = markWord8 "Block type marker" >>= blockParse
  where blockParse v
          | v == gifTrailer = pure []
          | v == imageSeparator = (:) <$> (BlockImage <$> parseMark "image block")
                                      <*> parseGifBlocks
          | v == extensionIntroducer = do
                extensionCode <- markWord8 "Extension code"
                if extensionCode /= graphicControlLabel
                   then parseDataBlocks >> parseGifBlocks
                   else (:) <$> (BlockGraphicControl <$> parseMark "stuff")
                            <*> parseGifBlocks

        blockParse v = do
            fail ("Unrecognized gif block " ++ show v)

instance Markeable ImageDescriptor where
    parseMark txt = subZone txt $ do
        -- due to missing lookahead
        {-_imageSeparator <- getWord8-}
        imgLeftPos <- markWord16le "left pos"
        imgTopPos  <- markWord16le "top pos"
        imgWidth   <- markWord16le "width"
        imgHeight  <- markWord16le "height"
        packedFields <- markWord8 "Packed field"
        let tableSize = packedFields .&. 0x7
        return ImageDescriptor
            { gDescPixelsFromLeft = imgLeftPos
            , gDescPixelsFromTop  = imgTopPos
            , gDescImageWidth     = imgWidth
            , gDescImageHeight    = imgHeight
            , gDescHasLocalMap    = packedFields `testBit` 7
            , gDescIsInterlaced     = packedFields `testBit` 6
            , gDescIsImgDescriptorSorted = packedFields `testBit` 5
            , gDescLocalColorTableSize = if tableSize > 0 then tableSize + 1 else 0
            }


--------------------------------------------------
----            Palette
--------------------------------------------------
type Palette = ()

getPalette :: Word8 -> Marker Palette
getPalette bitDepth = replicateM_ (size * 3) (markWord8 "palette data")
  where size = 2 ^ (fromIntegral bitDepth :: Int)


--------------------------------------------------
----            GifImage
--------------------------------------------------
data GifHeader = GifHeader
  { gifVersion          :: GifVersion
  , gifScreenDescriptor :: LogicalScreenDescriptor
  , gifGlobalMap        :: !Palette
  }
  deriving Show

instance Markeable GifHeader where
    parseMark txt = subZone txt $ do
        version    <- parseMark "Signature"
        screenDesc <- parseMark "Screen descriptor"
        palette    <- subZone "Palette"
                    . getPalette $ colorTableSize screenDesc
        return GifHeader
            { gifVersion = version
            , gifScreenDescriptor = screenDesc
            , gifGlobalMap = palette
            }

data GifFile = GifFile
    { gifHeader  :: !GifHeader
    , gifImages  :: [(Maybe GraphicControlExtension, GifImage)]
    }
    deriving Show

associateDescr :: [Block] -> [(Maybe GraphicControlExtension, GifImage)]
associateDescr [] = []
associateDescr [BlockGraphicControl _] = []
associateDescr (BlockGraphicControl _ : rest@(BlockGraphicControl _ : _)) = associateDescr rest
associateDescr (BlockImage img:xs) = (Nothing, img) : associateDescr xs
associateDescr (BlockGraphicControl ctrl : BlockImage img : xs) =
    (Just ctrl, img) : associateDescr xs

instance Markeable GifFile where
    parseMark txt = subZone txt $ do
        hdr <- parseMark "Header"
        blocks <- parseGifBlocks
        return GifFile { gifHeader = hdr
                       , gifImages = associateDescr blocks }

markGif :: Marker GifFile
markGif = parseMark ""

