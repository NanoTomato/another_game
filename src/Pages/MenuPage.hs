module Pages.MenuPage where

    import Page

    import Graphics.Gloss.Data.Picture (circleSolid)

    data MenuPage = Menu [Listener]

    instance Page MenuPage where
                update _ w = return w
                handle _ w = return w
                draw     _ = return $ circleSolid 20
