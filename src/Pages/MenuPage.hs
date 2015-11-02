module Pages.MenuPage where

    import Page
    import Math (ppDistance)

    import System.Exit (exitSuccess)

    import Graphics.Gloss.Data.Picture (circleSolid, circle)
    import Graphics.Gloss.Interface.IO.Game

    menuPage :: Page
    menuPage = Page {listeners = menuListeners,
                     update    = \_ -> return,
                     handle    = stdHandler,
                     draw      = stdDraw,
                     load      = stdLoad,
                     worldInfo = SimplePageInfo (0,0)}

    menuListeners = [ResCell "exitButton" (0,0) (\_ -> exitSuccess),
                     Shortcut [exitListener] (\_ -> exitSuccess)
                    ]

    exitButtonListener :: Event -> Bool
    exitButtonListener (EventKey (MouseButton LeftButton) Up _ p) = ppDistance p (0,0) < 20
    exitButtonListener _ = False

    exitListener :: Event -> Bool
    exitListener (EventKey (SpecialKey KeySpace) Up _ _) = True
    exitListener _ = False
