module Pages.GamePage where

    import Page
    import Math
    import GameStructures
    import Mechanics
    import Pages.Loader
    import Pages.GameMenuPage

    import Graphics.Gloss.Data.Picture (circleSolid, circle, pictures)
    import Graphics.Gloss.Interface.IO.Game

    gamePage :: World -> Page
    gamePage w = Page {listeners = gamePageListeners,
                       update    = gameUpdater,
                       handle    = stdHandler,
                       draw      = gameDraw,
                       load      = stdLoad,
                       worldInfo = GameInfo (0,0) w (0,0)}

    gameUpdater _ (Page ls u h d ld (GameInfo cp w wsz)) =
        (\w' -> return $ Page ls u h d ld (GameInfo cp (w') wsz)) =<< updateWorld w

    gameDraw p@(Page _ _ _ _ _ (GameInfo _ (World _ _ pic) _)) = fmap (pictures.(:[pic])) $ stdDraw p

    -- =============================================================== --

    gamePageListeners :: [Listener]
    gamePageListeners = [Shortcut [toGameMenuHandler] (toGameMenuReaction)]

    toGameMenuHandler (EventKey (SpecialKey KeyEsc) Up _ _) = True
    toGameMenuHandler _ = False

    toGameMenuReaction p@(Page _ _ _ _ _ (GameInfo _ w _)) =
        return $ loaderPage p "TestGameMenu" $ (gameMenuPage w pauseMenuListeners)

    -- =============================================================== --

    pauseMenuListeners :: [Listener]
    pauseMenuListeners = [Button [backToGameHandler] (circleSolid 20, circleSolid 20) (backToGameReaction)]

    backToGameHandler (EventKey (MouseButton LeftButton) Up _ p) = ppDistance (0,0) p < 20
    backToGameHandler (EventKey (SpecialKey KeyEsc) Up _ _) = True
    backToGameHandler _ = False

    backToGameReaction p = return $ loaderPage p "TestGame" $ gamePage $ (world $ worldInfo $ p)

    -- =============================================================== --

    walkListener :: Event -> Bool
    walkListener (EventKey (Char c) Down _ _) = elem c "wasd"
    walkListener _ = False

    stopListener :: Event -> Bool
    stopListener (EventKey (Char c) Down _ _) = elem c "wasd"
    stopListener _ = False

    directionDict :: [(Char,Direction)]
    directionDict = [('w',DUp),
                     ('a',DLeft),
                     ('s',DDown),
                     ('d',DRight)]
