module GameStructures where

    import Math
    
    import Graphics.Gloss.Data.Picture (Picture, blank)
    import Data.List (intercalate)
    import Control.Monad

    data World = World [Unit] Picture

    data UnitDrawInfo = ItemDrawInfo Picture
                      | WallDrawInfo Picture
                      | EffectDrawInfo [Picture]
                      | CreatureDrawInfo Picture
                      | ProjectileDrawInfo [Picture]

    data EffectActionType = Timer Int
                          | Instant
                          | Single
                          | Constant

    data Unit = Creature {position  :: Math.Point,
                          drawInfo  :: UnitDrawInfo,
                          speed     :: Math.Vector,
                          speedLim  :: Float,
                          health    :: Float,
                          radius    :: Float,
                          inventory :: [ItemInfo]}
              | Projectile {position :: Math.Point,
                            drawInfo :: UnitDrawInfo,
                            speed    :: Math.Vector,
                            impact   :: Unit}
              | Item {position :: Math.Point,
                      drawInfo :: UnitDrawInfo,
                      itemData :: ItemInfo}
              | Effect {position   :: Math.Point,
                        drawInfo   :: UnitDrawInfo,
                        actionType :: EffectActionType,
                        impactPred :: Unit -> Bool,
                        effect     :: Unit -> Unit}
              | Wall {position :: Math.Point,
                      drawInfo :: UnitDrawInfo,
                      sizeDims :: (Float, Float)}

    data ItemInfo = ItemInfo {itemIcon    :: Picture,
                              itemSockets :: [ItemSocket]}

    data ItemSocket = EmptyItemSocket
                    | FilledItemSocket ItemInfo

    instance Show Unit where
                      show (Creature p _ sp sl h r _) = "Creature: " ++ show p ++ (';':show sp) ++ (';':show sl) ++ (';':show h) ++ (';':show r)
                      show (Projectile p _ sp _)      = "Projectile: " ++ show p ++ (';':show sp)
                      show (Item p _ _)               = "Item: " ++ show p
                      show (Effect p _ _ _ _)         = "Effect: " ++ show p
                      show (Wall p _ sd)              = "Wall: " ++ show p ++ (';':show sd)

    debugShowUnits (World us _) = (\x -> return $ World x blank) =<< mapM (\x -> print x >> return x) us
