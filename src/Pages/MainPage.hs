module Pages.MainPage where

    import Page

    import Graphics.Gloss.Data.Picture (circleSolid)

    data MainPage = MainPage

    instance Page MainPage where
                update _ w = w
                handle _ w = w
                draw     _ = circleSolid 20
