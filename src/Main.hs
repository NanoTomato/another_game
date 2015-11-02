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

    import Page
    import Pages.MenuPage
    import Pages.Loader


    windowTitle = "Test"
    fps = 60

    main :: IO ()
    main = playIO (InWindow "Game" (1920,1080) (0,0))
                  white
                  fps
                  (loaderPage menuPage "menu" menuPage)
                  topDraw
                  topHandle
                  topUpdate
