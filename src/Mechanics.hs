module Mechanics where

    import Math

    import Data.List

    import Graphics.Gloss.Data.Picture (Picture(Blank), pictures)


    data World = World [Unit]

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

    drawItemSocket :: ItemSocket -> [Picture]
    drawItemSocket EmptyItemSocket = [Blank]
    drawItemSocket (FilledItemSocket ii) = (itemIcon ii):(concat $ map drawItemSocket $ itemSockets ii)
