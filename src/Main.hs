module Main where

    import Graphics.Gloss.Interface.IO.Game

    import System.Process
    import System.Environment
    import System.Directory

    import Data.String
    import qualified Data.ByteString as BS
    import Control.Applicative
    import Control.Monad
    import Data.Fixed
    import Data.Maybe

    import Page
    import Pages.MenuPage
    import Pages.Loader


    windowTitle = "Test"
    fps = 60
    delimeterDict = [("Windows_NT", "\\")]

    main :: IO ()
    main = do
        os <- getEnv "OS"
        let pathDelimeter = fromMaybe "/" $ lookup os delimeterDict
        playIO (InWindow "Game" (1920,1080) (0,0))
               white
               fps
               (loaderPage (menuPage pathDelimeter) "menu" pathDelimeter (menuPage pathDelimeter))
               topDraw
               topHandle
               topUpdate
