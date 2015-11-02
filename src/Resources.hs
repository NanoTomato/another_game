module Resources where

    import Codec.BMP
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Data.Picture

    import qualified Data.ByteString as BS
    import Data.List
    import Data.List.Split
    import Data.String
    import Data.Functor
    import Data.Biapplicative
    import Text.Regex.TDFA
    import Control.Monad
    import System.Directory

    data Resource = Resource [ResItem]

    data ResItem = ButtonRes String (Picture, Picture) (Int, Int)

    loadResItem :: FilePath -> FilePath -> IO ResItem
    loadResItem d p = readBMP fullPath >>= return . readResource (takeWhile (/='.') p) where
                 fullPath = if last d == '/' then d ++ p else d ++ '/':p

    readResource :: FilePath -> Either Error BMP -> ResItem
    readResource ('b':name) f = ButtonRes buttonName pics size where
                 (buttonName,pics, size) = if name =~ nameRegex then (makeButtonRes name f) else errorButtonResData
                 makeButtonRes _ (Left _) = errorButtonResData
                 makeButtonRes n (Right f) = (resName, bimap modify modify imgs, (w,h)) where
                                                          [resName, size] = splitOn "_" n
                                                          [w,h] = map (read :: String -> Int) $ splitOn "x" size
                                                          imgs = splitAt (w*h) $ BS.unpack $ bmpRawImageData f
                                                          modify img = bitmapOfByteString w h (BS.pack img) True
                 addTransparency [] = []
                 addTransparency (b:g:r:t) = (if [r,g,b] == [0,255,255] then 0 else 255):b:g:r:(addTransparency t)
                 errorButtonResData = ("error", (circleSolid 20, circle 20), (0,0))
                 nameRegex = "^[a-zA-Z]+_[0-9]{1,2}x[0-9]{1,2}"

    gameResDir = "GAME_RES_DIR"
