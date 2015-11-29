module Pages.PauseMenu where

    import Page
    import Math
    import GameStructures
    import Pages.Loader
    import Pages.GamePage (gamePage)

    import Graphics.Gloss.Data.Picture (circleSolid, circle, pictures, color)
    import Graphics.Gloss.Interface.IO.Game

    
    pauseMenuListeners :: [Listener]
    pauseMenuListeners = [Button [backToGameHandler] (circleSolid 20, circleSolid 20) (backToGameReaction)]

    backToGameHandler (EventKey (MouseButton LeftButton) Up _ p) = ppDistance (0,0) p < 20
    backToGameHandler (EventKey (SpecialKey KeyEsc) Up _ _) = True
    backToGameHandler _ = False

    backToGameReaction p@(Page _ _ _ _ _ (GameInfo _ _ w)) = return $ loaderPage p "TestGame" $ gamePage $ w
