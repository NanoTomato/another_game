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
                     cursorPos = (0,0)}

    menuListeners = [Button [exitButtonListener] (circleSolid 20, circle 20) (\_ -> exitSuccess),
                     Shortcut [exitListener] (\_ -> exitSuccess)
                    ]

    exitButtonListener :: Event -> Bool
    exitButtonListener (EventKey (MouseButton LeftButton) Up _ p) = ppDistance p (0,0) < 20
    exitButtonListener _ = False

    exitListener :: Event -> Bool
    exitListener (EventKey (SpecialKey KeySpace) Up _ _) = True
    exitListener _ = False
