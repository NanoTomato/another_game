{-# LANGUAGE Rank2Types #-}

module Page where

    import Math
    import Resources

    import Data.List
    import Data.Maybe
    import Data.Functor

    import Graphics.Gloss.Data.Picture (Picture, pictures, blank)
    import Graphics.Gloss.Interface.IO.Game


    type Reaction = (Page -> IO Page)
    type EventFilter = (Event -> Bool)

    data WorldInfo = SimplePageInfo {cursorPos::Math.Point}
                   | LoaderInfo {cursorPos::Math.Point,
                                 nextPage ::Page,
                                 prevPage ::Page,
                                 nextDir  ::String,
                                 progress ::Float}

    data Page = Page {listeners :: [Listener],
                      update    :: Float -> Page -> IO Page,
                      handle    :: Event -> Page -> IO Page,
                      draw      :: Page -> IO Picture,
                      load      :: Page -> Resource -> Page,
                      worldInfo :: WorldInfo}

    data Listener = Button [EventFilter] (Picture, Picture) Reaction
                  | Shortcut [EventFilter] Reaction
                  | ResCell String Math.Point Reaction

    stdHandler :: Event -> Page -> IO Page
    stdHandler (EventMotion p) (Page ls u h d ld _) = return $ Page ls u h d ld $ SimplePageInfo p
    stdHandler e p = maybeModify p $ reaction <$> find (any ($ e) . eventFilters) (listeners p)

    stdDraw :: Page -> IO Picture
    stdDraw p = return $ pictures $ map (drawListener $ cursorPos . worldInfo $ p) (listeners p) where
            drawListener _ (Shortcut _ _) = blank
            drawListener p (Button efs (p1,p2) _) = if any ($ genButtonPressEvent p) efs then p2 else p1

    stdLoad :: Page -> Resource -> Page
    stdLoad (Page l u h d ld wi) (Resource brs) = Page (map (findRes brs) l) u h d ld wi where
            findRes rs (ResCell n p r) = fromMaybe (testButton p) $ fmap (loadSingleRes p r) $ find (checkRes n) rs
            findRes _ l = l
            loadSingleRes p r (ButtonRes n imgs (w,h)) = Button [mousePressEventHandler $ makeBox p (fromIntegral w,fromIntegral h)] imgs r where
                mousePressEventHandler (f,t) (EventKey (MouseButton LeftButton) Up _ p) = Math.pointInBox p f t
                mousePressEventHandler _ _ = False
            checkRes str (ButtonRes n _ _) = str == n

    genButtonPressEvent :: Math.Point -> Event
    genButtonPressEvent = EventKey (MouseButton LeftButton) Up (Modifiers Down Down Down)

    testButton :: Math.Point -> Listener
    testButton p = Button [] ((uncurry translate) p $ circleSolid 10,(uncurry translate) p $ circle 10) return

    eventFilters :: Listener -> [EventFilter]
    eventFilters (Button efs _ _) = efs
    eventFilters (Shortcut efs _) = efs
    eventFilters _ = []

    reaction :: Listener -> Reaction
    reaction (Button _ _ r) = r
    reaction (Shortcut _ r) = r
    reaction _ = return

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
