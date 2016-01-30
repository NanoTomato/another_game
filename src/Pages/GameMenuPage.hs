module Pages.GameMenuPage where

    import Page
    import Math
    import GameStructures

    import Graphics.Gloss.Data.Picture (circleSolid, circle, pictures, color, rectangleSolid)
    import Graphics.Gloss.Interface.IO.Game

    import Control.Applicative

    gameMenuPage :: World -> [Listener] -> String -> Page
    gameMenuPage w ls pd = Page {listeners = ls,
                                 update    = const return,
                                 handle    = stdHandler,
                                 draw      = gameMenuDraw,
                                 load      = stdLoad,
                                 pathDlmtr = pd,
                                 worldInfo = GameInfo "" (0,0) w (0,0)}

    gameMenuDraw p@(Page _ _ _ _ _ _ (GameInfo _ _ (World _ _ _ pic) (x,y))) =
        pictures.(:color (makeColor 1 1 1 0.2) (rectangleSolid 5000 5000):[pic]) <$> stdDraw p
