module Page where

    import Graphics.Gloss.Data.Picture (Picture)
    import Graphics.Gloss.Interface.IO.Game (Event)

    type Reaction = (World -> World)
    type World = String

    class Page a where
            update :: (Float -> a -> a)
            handle :: (Event -> a -> a)
            draw   :: (a -> Picture)
