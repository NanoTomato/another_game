module Main where

    import Graphics.Gloss.Interface.IO.Game
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Data.Picture


    import System.Process
    import System.Environment
    import System.Directory
    import Codec.BMP

    import Data.String
    import qualified Data.ByteString as BS
    import Control.Applicative
    import Control.Monad
    import Data.Fixed

    import Page
    import Pages.MenuPage


    windowTitle = "Test"
    fps = 60

    loadSprite :: FilePath -> IO Picture
    loadSprite p = readBMP p >>= (\f -> return $ bitmapOfByteString 20 20 (addTransparency f) True) where
            addTransparency (Left _)  = BS.pack []
            addTransparency (Right b) = BS.pack . modify . BS.unpack $ (bmpRawImageData b) where
                modify [] = []
                modify (b:g:r:t) = (if [r,g,b] == [0,255,255] then 0 else 255):b:g:r:(modify t)

    main :: IO ()
    main = do
--        imagesDir <- getEnv "GAME_PICS_DIR"
--        bitmaps <- getDirectoryContents imagesDir >>=
--                   mapM (\p -> (return $ (,) $ takeWhile (/='.') p) <*> loadSprite (imagesDir ++ ('/':p))) . filter (\n -> not $ elem n [".",".."])
        playIO (InWindow "Game" (1920,1080) (0,0))
               white
               fps
               (Menu [])
               draw
               handle
               update
