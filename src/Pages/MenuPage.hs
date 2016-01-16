module Pages.MenuPage where

    import Page
    import Pages.Loader
    import Pages.GamePage
    import GameStructures (World(..), Unit(Creature,Wall), UnitDrawInfo(CreatureDrawInfo,WallDrawInfo))

    import System.Exit (exitSuccess)

    import Graphics.Gloss.Data.Picture (circleSolid, circle, blank, rectangleWire)
    import Graphics.Gloss.Interface.IO.Game

    menuPage :: String -> Page
    menuPage pd = Page {listeners = menuListeners,
                        update    = \_ -> return,
                        handle    = stdHandler,
                        draw      = stdDraw,
                        load      = stdLoad,
                        pathDlmtr = pd,
                        worldInfo = SimplePageInfo (0,0)}

    menuListeners = [ResCell "exitButton" (0,-50) (\_ -> exitSuccess),
                     ResCell "playButton" (0,50) (\p -> return $ loaderPage p "TestGame" (pathDlmtr p) $ gamePage (pathDlmtr p) $ World [] testGameUnits blank),
                     Shortcut [exitListener] (\_ -> exitSuccess)
                    ]

    exitListener :: Event -> Bool
    exitListener (EventKey (SpecialKey KeySpace) Up _ _) = True
    exitListener _ = False

    testGameUnits = [Creature (0,0) (CreatureDrawInfo $ circleSolid 30) (2,2) 5 1 30 [],
                     Creature (280,250) (CreatureDrawInfo $ circle 30) (0,0) 0 1 30 [],
                     Creature (60,500) (CreatureDrawInfo $ circle 30) (0,0) 0 1 30 [],
                     Creature (250,120) (CreatureDrawInfo $ circle 30) (0,0) 0 1 30 [],
                     Wall (130,130) (WallDrawInfo $ rectangleWire 100 20) (100, 20),
                     Wall (400,50) (WallDrawInfo $ rectangleWire 20 1000) (20,1000)]
