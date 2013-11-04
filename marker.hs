{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative( (<$>), (<*>), pure, (<*) )
import Data.Bits
import Data.ByteString as B
import Data.Text as T
import Data.Word( Word8, Word16, Word32 )
import Control.Monad.Trans.RWS( RWS, asks, gets, modify, tell )

data MarkEntry = MarkEntry
    { markName     :: T.Text
    , markShow     :: T.Text
    , markOffset   :: !Int
    , markSize     :: !Int
    , markChildren :: [MarkEntry]
    }
    deriving (Eq, Show)

data MarkerSetup = MarkerSetup
    { setupData     :: B.ByteString
    , setupFilename :: FilePath
    }

data MarkerState = MarkerState
    { markerIndex   :: !Int }

type Marker a =
    RWS MarkerSetup [MarkEntry] MarkerState a

getByte :: Marker Word8
getByte = B.index <$> asks setupData <*> gets markerIndex

incIndex :: Marker ()
incIndex = modify $ \s -> s { markerIndex = markerIndex s + 1 }

markWord8 :: T.Text -> Marker Word8
markWord8 txt = do
    offset <- gets markerIndex
    v <- getByte <* incIndex
    tell [MarkEntry { 
            markName = txt,
            markShow = T.pack $ show v,
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
            markShow = T.pack $ show v,
            markOffset = offset,
            markSize = 2,
            markChildren = []
        }]

    pure v

main :: IO ()
main = return ()

