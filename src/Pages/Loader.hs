module Pages.Loader where

    import Page
    import Resources

    import Control.Monad
    import System.Environment
    import System.Directory

    import Graphics.Gloss.Interface.IO.Game

    loaderPage :: Page -> String -> Page -> Page
    loaderPage p d n = Page {listeners = loaderListeners,
                             update    = (updateLoader [] (-1) (Resource [])),
                             handle    = stdHandler,
                             draw      = stdDraw,
                             load      = stdLoad,
                             worldInfo = LoaderInfo {cursorPos = (0,0),
                                                     nextPage  = n,
                                                     prevPage  = p,
                                                     nextDir   = d,
                                                     progress  = 0}}

    drawLoader (Page _ _ _ _ _ (LoaderInfo _ _ _ str prog)) = return $ Pictures [label,barFrame,barFilling] where
                    label = translate 0 50 $ text $ str
                    barFrame = (uncurry rectangleWire) loadingBarParams
                    barFilling = translate ((prog-1)*loadingBarWidth/2) 0 $ rectangleSolid (prog*loadingBarWidth) loadingBarHeight

    updateLoader :: [FilePath] -> Int -> Resource -> Float -> Page -> IO Page
    updateLoader _ (-1) _ _ (Page ls _ h d l (LoaderInfo cp np pp nd _)) =
        fmap (++('/':nd)) (getEnv gameResDir) >>=
        (\dir -> fmap ((,) dir) $ fmap (filter (\i -> not $ elem i [".",".."])) (getDirectoryContents dir)) >>=
        (\(dir,dcs) -> let newUpdateLoader = updateLoader dcs cnt (Resource [])
                           cnt = length dcs
                       in
                         return $ Page ls newUpdateLoader h d l (LoaderInfo cp np pp dir (1-(fromIntegral $ length dcs)/(fromIntegral cnt))))
    updateLoader [] _ r _ p = return $ (load.nextPage.worldInfo $ p) (nextPage.worldInfo $ p) r
    updateLoader (f:fs) cnt (Resource rs) _ (Page ls _ h d l (LoaderInfo cp np pp nd pr)) =
        (loadResItem nd f) >>= (\r -> let newUpdateLoader = updateLoader fs cnt (Resource (r:rs))
                                      in return $ Page ls newUpdateLoader h d l (LoaderInfo cp np pp nd (1-(fromIntegral $ length fs)/(fromIntegral cnt))))

    loaderListeners = [Shortcut [exitLoader] (\p -> return.prevPage.worldInfo $ p)]

    exitLoader :: Event -> Bool
    exitLoader (EventKey (SpecialKey KeyEsc) Up _ _) = True
    exitLoader _ = False

    loadingBarParams@(loadingBarWidth,loadingBarHeight) = (1000,30)
