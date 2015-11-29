module Pages.GameMenuPage where

    import Page
    import Math
    import GameStructures

    import Graphics.Gloss.Data.Picture (circleSolid, circle, pictures, color)
    import Graphics.Gloss.Interface.IO.Game

    gameMenuPage :: World -> [Listener] -> Page
    gameMenuPage w ls = Page {listeners = ls,
                              update    = \_ -> return,
                              handle    = stdHandler,
                              draw      = gameMenuDraw,
                              load      = stdLoad,
                              worldInfo = GameInfo (0,0) w (0,0)}

    gameMenuDraw p@(Page _ _ _ _ _ (GameInfo _ (World _ _ pic) _)) =
        fmap (pictures.(:[color (makeColor 0 0 0 0.2) $ pic])) $ stdDraw p
