module Pages.GamePage where

    import Page
    import Math
    import Mechanics

    import Graphics.Gloss.Data.Picture (circleSolid, circle)
    import Graphics.Gloss.Interface.IO.Game

    gamePage :: World -> Page
    gamePage w = Page {listeners = [],
                       update    = \_ -> return,
                       handle    = stdHandler,
                       draw      = stdDraw,
                       load      = stdLoad,
                       worldInfo = GameInfo (0,0) w (0,0)}

    splitUnitList :: [Unit] -> ([Unit],[Unit],[Unit],[Unit],[Unit])
    splitUnitList us = splitUnitList' us ([],[],[],[],[]) where
                      splitUnitList' [] ls = ls
                      splitUnitList' (c@(Creature _ _ _ _ _ _):us) (cs,ps,is,es,ws) = splitUnitList' us (c:cs,ps,is,es,ws)
                      splitUnitList' (p@(Projectile _ _ _ _):us)   (cs,ps,is,es,ws) = splitUnitList' us (cs,p:ps,is,es,ws)
                      splitUnitList' (i@(Item _ _ _):us)           (cs,ps,is,es,ws) = splitUnitList' us (cs,ps,i:is,es,ws)
                      splitUnitList' (e@(Effect _ _ _ _ _):us)     (cs,ps,is,es,ws) = splitUnitList' us (cs,ps,is,e:es,ws)
                      splitUnitList' (w@(Wall _ _ _):us)           (cs,ps,is,es,ws) = splitUnitList' us (cs,ps,is,es,w:ws)

    updateCreatures :: [Unit] -> [Unit] -> [Unit] -> [Unit]
    updateCreatures [] _ _ = []
    updateCreatures (c@(Creature pos di s h r i):cs) ws is = if h < 0
                                                                then updateCreatures cs ws is
                                                                else (Creature (pvAddVector pos s) di s h r i):(updateCreatures cs ws is)

    updateProjectiles :: [Unit] -> [Unit] -> [Unit] -> [Unit] -> [Unit] -> ([Unit],[Unit])
    updateProjectiles [] _ _ rs acc = (rs, acc)
    updateProjectiles ((Projectile pos di s r):ps) cs ws rs acc = if any (creatureHit newPos) cs || any (\w->let (p1,p2)=makeWallBox w in pointInBox newPos p1 p2) ws
                                                                  then updateProjectiles ps cs ws (r:rs) acc
                                                                  else updateProjectiles ps cs ws rs (Projectile newPos di s r:acc) where
                                                                      newPos = pvAddVector pos s
                                                                      creatureHit p (Creature pos _ _ _ r i) = cpContaining (pos, r) p

    applyEffects :: [Unit] -> [Unit] -> [Unit]
    applyEffects [] us = us
    applyEffects (e@(Effect _ _ _ ip f):es) us = if isEffectEnded e us  -- TODO: Rename this
                                                 then applyEffects es $ map (\u -> if ip u then f u else u) us
                                                 else modify e:applyEffects es (map (\u -> if ip u then f u else u) us) where
                                                     isEffectEnded (Effect _ _ Instant _ _) _ = True
                                                     isEffectEnded (Effect _ _ Constant _ _) _ = False
                                                     isEffectEnded (Effect _ _ (Timer t) _ _) _ = t <= 0
                                                     isEffectEnded (Effect _ _ Single ip _) us = any ip us
                                                     modify (Effect p di (Timer t) ip f) = Effect p di (Timer (t-1)) ip f
                                                     modify e = e

    makeWallBox :: Unit -> (Math.Point,Math.Point)
    makeWallBox (Wall p _ s) = makeBox p s

    updateWorld :: World -> World
    updateWorld (World us) = World $ applyEffects (es++rs) $ concat [mcs,mps,is,ws] where
        (cs,ps,is,es,ws) = splitUnitList us
        mcs = updateCreatures cs ws is
        (rs,mps) = updateProjectiles ps cs ws [] []

    gameUpdater (Page ls u h d ld (GameInfo cp w wsz)) = return $ Page ls u h d ld (GameInfo cp (updateWorld w) wsz)
