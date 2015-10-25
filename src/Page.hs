{-# LANGUAGE Rank2Types #-}

module Page where

    import Math

    import Data.List
    import Data.Maybe
    import Data.Functor

    import Graphics.Gloss.Data.Picture (Picture)
    import Graphics.Gloss.Interface.IO.Game (Event)


    type Reaction = (Page p) => (p -> p)
    type EventFilter = (Event -> Bool)

    class Page a where
            listeners :: a -> [Listener]
            update :: Float -> a -> IO a
            handle :: Event -> a -> IO a
            draw   :: a -> IO Picture

            handle e p = return $ maybeModify p $ reaction <$> find (any ($ e) . eventFilters) (listeners p)

    data Listener = Button [EventFilter] (Picture, Picture) Reaction
                  | Shortcut [EventFilter] Reaction

    eventFilters :: Listener -> [EventFilter]
    eventFilters (Button efs _ _) = efs
    eventFilters (Shortcut efs _) = efs

    reaction :: Listener -> Reaction
    reaction (Button _ _ r) = r
    reaction (Shortcut _ r) = r

    maybeModify :: a -> Maybe (a->a) -> a
--  maybeModify x = maybe x ($ x)  -- Just for lulz
    maybeModify x (Just f) = f x
    maybeModify x Nothing  = x
