module Pages.GamePage where

    import Page
    import Math
    import GameStructures
    import Mechanics
    import Pages.Loader
    import Pages.GameMenuPage
    import System.Exit (exitSuccess)

    import Graphics.Gloss.Data.Picture (circleSolid, circle, pictures)
    import Graphics.Gloss.Interface.IO.Game

    import Control.Applicative

    gamePage :: String -> World -> Page
    gamePage pd w = Page {listeners = gamePageListeners,
                          update    = gameUpdater,
                          handle    = stdHandler,
                          draw      = gameDraw,
                          load      = stdLoad,
                          pathDlmtr = pd,
                          worldInfo = GameInfo (0,0) w (0,0)}

    gameUpdater _ (Page ls u h d ld pd (GameInfo cp w wsz)) =
        (\w' -> return $ Page ls u h d ld pd (GameInfo cp w' wsz)) =<< updateWorld w

    gameDraw p@(Page _ _ _ _ _ _ (GameInfo _ (World _ _ _ pic) _)) = pictures.(:[pic]) <$> stdDraw p

    -- =============================================================== --

    gamePageListeners :: [Listener]
    gamePageListeners = map stopListener directionDict
                     ++ map walkListener directionDict
                     ++ [Shortcut [toGameMenuHandler] toGameMenuReaction]
                     ++ [Shortcut [exitL] (const exitSuccess)]

    exitL :: Event -> Bool
    exitL (EventKey (SpecialKey KeySpace) Up _ _) = True
    exitL _ = False

    toGameMenuHandler (EventKey (SpecialKey KeyEsc) Up _ _) = True
    toGameMenuHandler _ = False

    toGameMenuReaction p@(Page _ _ _ _ _ _ (GameInfo _ w _)) =
        return $ loaderPage p "TestGameMenu" pd $ gameMenuPage w pauseMenuListeners pd where
            pd = pathDlmtr p

    -- =============================================================== --

    pauseMenuListeners :: [Listener]
    pauseMenuListeners = [Button [backToGameHandler] (circleSolid 20, circleSolid 20) backToGameReaction]

    backToGameHandler (EventKey (MouseButton LeftButton) Up _ p) = ppDistance (0,0) p < 20
    backToGameHandler (EventKey (SpecialKey KeyEsc) Up _ _) = True
    backToGameHandler _ = False

    backToGameReaction p = return $ loaderPage p "TestGame" pd $ gamePage pd (world $ worldInfo p) where
        pd = pathDlmtr p

    -- =============================================================== --

    walkListener :: (Char, GameEvent) -> Listener
    walkListener (c,ge) = Shortcut [handler] reaction where
        handler (EventKey (Char c) Down _ _) = True
        handler _ = False
        reaction (Page ls u h d l pd (GameInfo cp (World s ges us pic) wi)) =
            return $ Page ls u h d l pd (GameInfo cp (World s (ge:ges) us pic) wi)

    stopListener :: (Char, GameEvent) -> Listener
    stopListener (c,ge) = Shortcut [handler] reaction where
        handler (EventKey (Char c) up _ _) = True
        handler _ = False
        reaction (Page ls u h d l pd (GameInfo cp (World s ges us pic) wi)) =
            return $ Page ls u h d l pd (GameInfo cp (World s (filter (/=ge) ges) us pic) wi)
