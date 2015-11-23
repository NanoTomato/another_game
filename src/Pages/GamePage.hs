module Pages.GamePage where

    import Page
    import Math
    import GameStructures
    import Mechanics

    import Graphics.Gloss.Data.Picture (circleSolid, circle, pictures)
    import Graphics.Gloss.Interface.IO.Game

    gamePage :: World -> Page
    gamePage w = Page {listeners = [],
                       update    = gameUpdater,
                       handle    = stdHandler,
                       draw      = gameDraw,
                       load      = stdLoad,
                       worldInfo = GameInfo (0,0) w (0,0)}

    gameUpdater _ (Page ls u h d ld (GameInfo cp w wsz)) = return $ Page ls u h d ld (GameInfo cp (updateWorld w) wsz)

    gameDraw p@(Page _ _ _ _ _ (GameInfo _ (World _ pic) _)) = fmap (pictures.(:[pic])) $ stdDraw p

    walkListener :: Event -> Bool
    walkListener (EventKey (Char c) Down _ _) = elem c "wasd"
    walkListener _ = False

    stopListener :: Event -> Bool
    stopListener (EventKey (Char c) Down _ _) = elem c "wasd"
    stopListener _ = False

    directionDict :: [(Char,Math.Vector)]
    directionDict = [('w',(0,1)),
                     ('s',(0,-1)),
                     ('a',(-1,0)),
                     ('d',(1,0))]

