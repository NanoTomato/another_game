module Mechanics where

    import GameStructures

    import Math

    import Data.List
    import Data.Maybe
    import Control.Arrow

    import Graphics.Gloss.Data.Vector (normalizeV, mulSV)
    import Graphics.Gloss.Data.Picture (Picture(Blank), pictures, translate)


    myTranslate = uncurry translate

    drawUnit :: Unit -> (Unit,Picture)
    drawUnit (Creature p di@(CreatureDrawInfo pic) s sl h r i si) = (Creature p di s sl h r i si,myTranslate p pic)
    drawUnit (Projectile p (ProjectileDrawInfo (pic:ps)) s i)     = (Projectile p (ProjectileDrawInfo ps) s i,myTranslate p pic)
    drawUnit (Item p di@(ItemDrawInfo pic) i)                     = (Item p di i,myTranslate p pic)
    drawUnit (Effect p di@(EffectDrawInfo (pic:ps)) at ip e)      = (Effect p (EffectDrawInfo ps) at ip e,myTranslate p pic)
    drawUnit (Wall p di@(WallDrawInfo pic) s)                     = (Wall p di s,myTranslate p pic)

    drawItemSocket :: ItemSocket -> [Picture]
    drawItemSocket EmptyItemSocket = [Blank]
    drawItemSocket (FilledItemSocket ii) = itemIcon ii : concatMap drawItemSocket (itemSockets ii)


    splitUnitList :: [Unit] -> ([Unit],[Unit],[Unit],[Unit],[Unit])
    splitUnitList us = splitUnitList' us ([],[],[],[],[]) where
                 splitUnitList' [] ls = ls
                 splitUnitList' (c@(Creature{}):us)   (cs,ps,is,es,ws) = splitUnitList' us (c:cs,ps,is,es,ws)
                 splitUnitList' (p@(Projectile{}):us) (cs,ps,is,es,ws) = splitUnitList' us (cs,p:ps,is,es,ws)
                 splitUnitList' (i@(Item{}):us)       (cs,ps,is,es,ws) = splitUnitList' us (cs,ps,i:is,es,ws)
                 splitUnitList' (e@(Effect{}):us)     (cs,ps,is,es,ws) = splitUnitList' us (cs,ps,is,e:es,ws)
                 splitUnitList' (w@(Wall{}):us)       (cs,ps,is,es,ws) = splitUnitList' us (cs,ps,is,es,w:ws)

    updateCreatures :: [(String,[GameEvent])] -> [Unit] -> [Unit] -> [Unit] -> [Unit] -> [Unit]
    updateCreatures _ [] _ _ _ = []
    updateCreatures ges (c@(Creature pos di s sl h r i si):cs) ws is acc =
        if h <= 0
        then updateCreatures ges cs ws is (c:acc)
        else Creature newPos di newSpeed sl h r i si : updateCreatures ges cs ws is (c:acc) where
                 newPos = pvAddVector pos newSpeed
                 newSpeed = foldl vvMagicAdd oldSpeed vs
                 oldSpeed = maxSpeed $ if si == Nothing then s else givenSpeed
                 maxSpeed v = if v==(0,0) then v else mulSV sl $ normalizeV v
                 givenSpeed = foldl vvScalarSum (0,0) givenDirections
                 givenDirections = map (\(Move d)->directionVector d) givenMoveEvents
                 givenMoveEvents = filter (flip elem $ map snd directionDict) givenEvents
                 givenEvents = fromMaybe [] $ lookup (fromJust si) ges
                 nextStepPos = pvAddVector pos oldSpeed
                 vs = wallIntVecs ++ creatIntVecs
                 wallIntVecs = [cbIntersectionVector (nextStepPos,r) (makeWallBox w)|w<-ws]
                 creatIntVecs = [ccIntersectionVector (nextStepPos,r) (position c,radius c)|c<-cs++acc]

    updateProjectiles :: [Unit] -> [Unit] -> [Unit] -> [Unit] -> [Unit] -> ([Unit],[Unit])
    updateProjectiles [] _ _ rs acc = (rs, acc)
    updateProjectiles (Projectile pos di s r : ps) cs ws rs acc =
        if any (creatureHit newPos) cs || any (\w->let (p1,p2)=makeWallBox w in pointInBox newPos p1 p2) ws
        then updateProjectiles ps cs ws (r:rs) acc
        else updateProjectiles ps cs ws rs (Projectile newPos di s r:acc) where
                 newPos = pvAddVector pos s
                 creatureHit p (Creature pos _ _ _ _ r i _) = cpContaining (pos, r) p

    applyEffects :: [Unit] -> [Unit] -> [Unit]
    applyEffects [] us = us
    applyEffects (e@(Effect _ _ _ ip f):es) us =
        if isEffectEnded e us  -- TODO: Rename this
        then applyEffects es $ map (\u -> if ip u then f u else u) us
        else modify e:applyEffects es (map (\u -> if ip u then f u else u) us) where
                 isEffectEnded (Effect _ _ Instant _ _) _ = True
                 isEffectEnded (Effect _ _ Constant _ _) _ = False
                 isEffectEnded (Effect _ _ (Timer t) _ _) _ = t <= 0
                 isEffectEnded (Effect _ _ Single ip _) us = any ip us
                 modify (Effect p di (Timer t) ip f) = Effect p di (Timer (t-1)) ip f
                 modify e = e

    makeWorldPicture :: [Unit] -> ([Unit],Picture)
    makeWorldPicture us = second pictures $ foldl (\(us,ps) -> (:us) *** (:ps)) ([],[]) $ map drawUnit us

    makeWallBox :: Unit -> (Math.Point,Math.Point)
    makeWallBox (Wall p _ s) = makeBox p s

    updateWorld' :: World -> IO [Unit]
    updateWorld' (World s ges us _) = return $ applyEffects (es++rs) $ concat [mcs,mps,is,ws] where
        (cs,ps,is,es,ws) = splitUnitList us
        mcs = updateCreatures [(s,ges)] cs ws is []
        (rs,mps) = updateProjectiles ps cs ws [] []

    updateWorld :: World -> IO World
    updateWorld w@(World pid ges _ _) = (return . uncurry (World pid updatedEvents) . makeWorldPicture) =<< updateWorld' w where
        updatedEvents = nub $ filter (\ge -> elem ge $ map snd directionDict) ges
