module Page where

    import Math

    import Graphics.Gloss.Data.Picture (Picture)
    import Graphics.Gloss.Interface.IO.Game (Event)

    type Reaction = (World -> World)
    type World = String

    class Page a where
            update :: (Float -> a -> IO a)
            handle :: (Event -> a -> IO a)
            draw   :: (a -> IO Picture)

    data Listener = Button Line (Picture, Picture) [Listener]
                  | Shortcut [Event] Reaction
