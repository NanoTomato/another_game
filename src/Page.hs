{-# LANGUAGE Rank2Types #-}

module Page where

    import Math

    import Data.List
    import Data.Maybe
    import Data.Functor

    import Graphics.Gloss.Data.Picture (Picture, pictures, blank)
    import Graphics.Gloss.Interface.IO.Game


    type Reaction = (Page -> IO Page)
    type EventFilter = (Event -> Bool)

    data Page = Page {listeners :: [Listener],
                      update    :: Float -> Page -> IO Page,
                      handle    :: Event -> Page -> IO Page,
                      draw      :: Page -> IO Picture,
                      cursorPos :: Math.Point}

    stdHandler :: Event -> Page -> IO Page
    stdHandler (EventMotion p) (Page ls u h d _) = return $ Page ls u h d p
    stdHandler e p = maybeModify p $ reaction <$> find (any ($ e) . eventFilters) (listeners p)

    stdDraw :: Page -> IO Picture
    stdDraw p = return $ pictures $ map (drawListener $ cursorPos p) (listeners p) where
            drawListener _ (Shortcut _ _) = blank
            drawListener p (Button efs (p1,p2) _) = if any ($ genButtonPressEvent p) efs then p2 else p1

    genButtonPressEvent :: Math.Point -> Event
    genButtonPressEvent = EventKey (MouseButton LeftButton) Up (Modifiers Down Down Down)

    testButton :: Math.Point -> Listener
    testButton p = Button [] ((uncurry translate) p $ circleSolid 10,(uncurry translate) p $ circle 10) return

    data Listener = Button [EventFilter] (Picture, Picture) Reaction
                  | Shortcut [EventFilter] Reaction

    eventFilters :: Listener -> [EventFilter]
    eventFilters (Button efs _ _) = efs
    eventFilters (Shortcut efs _) = efs

    reaction :: Listener -> Reaction
    reaction (Button _ _ r) = r
    reaction (Shortcut _ r) = r

    maybeModify :: a -> Maybe (a -> IO a) -> IO a
--  maybeModify x = maybe x ($ x)  -- Just for lulz
    maybeModify x (Just f) = f x
    maybeModify x Nothing  = return x

    topUpdate :: Float -> Page -> IO Page
    topUpdate t p = (update p) t p
    topHandle :: Event -> Page -> IO Page
    topHandle e p = (handle p) e p
    topDraw   :: Page -> IO Picture
    topDraw     p = draw p $ p
