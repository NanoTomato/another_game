module Resources where

    import Codec.BMP
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Data.Picture

    import qualified Data.ByteString as BS
    import Data.List
    import Data.List.Split
    import Data.String
    import Data.Functor
    import Text.Regex.TDFA
    import Control.Arrow
    import Control.Monad
    import System.Directory

    data Resource = Resource [ResItem]

    data ResItem = ButtonRes String (Picture, Picture) (Int, Int)

    loadResItem :: FilePath -> FilePath -> String -> IO ResItem
    loadResItem d p pd = liftM (readResource (takeWhile (/='.') p)) (readBMP fullPath) where
                 fullPath = if pd `elem` tails d then d ++ p else d ++ pd ++ p

    readResource :: FilePath -> Either Error BMP -> ResItem
    readResource ('b':name) f = ButtonRes buttonName pics size where
                 (buttonName,pics, size) = if name =~ nameRegex then makeButtonRes name f else errorButtonResData
                 makeButtonRes _ (Left _) = errorButtonResData
                 makeButtonRes n (Right f) = (resName, modify *** modify $ imgs, (w,h)) where
                                                          [resName, size] = splitOn "_" n
                                                          [w,h] = map (read :: String -> Int) $ splitOn "x" size
                                                          imgs = splitAt (w*h*3) $ BS.unpack $ bmpRawImageData f
                                                          modify img = bitmapOfByteString w h (BS.pack $ addTransparency img) True
                 errorButtonResData = ("error", (circleSolid 20, circle 20), (0,0))
                 nameRegex = "^[a-zA-Z]+_[0-9]{1,2}x[0-9]{1,2}"

    addTransparency [] = []
    addTransparency (b:g:r:t) = (if [r,g,b] == [0,255,255] then 0 else 255):b:g:r:addTransparency t

    gameResDir = "GAME_RES_DIR"
