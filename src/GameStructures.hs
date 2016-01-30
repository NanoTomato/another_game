module GameStructures where

    import Math

    import Graphics.Gloss.Data.Picture (Picture, blank)
    import Graphics.Gloss.Interface.IO.Game (SpecialKey(..))
    import Data.List (intercalate)
    import Control.Monad

    data GameEvent = Move Direction
                   | Apply
                   | Open deriving Show

    data Direction = DUp
                   | DDown
                   | DLeft
                   | DRight deriving (Eq, Show)

    directionDict :: [(Char,GameEvent)]
    directionDict = [('w',Move DUp),
                     ('a',Move DLeft),
                     ('s',Move DDown),
                     ('d',Move DRight)]

    instance Eq GameEvent where
        (Move a) == (Move b) = a == b
        _ == _ = False

    negateDirection :: Direction -> Direction
    negateDirection DUp    = DDown
    negateDirection DDown  = DUp
    negateDirection DLeft  = DRight
    negateDirection DRight = DLeft

    directionVector :: Direction -> Vector
    directionVector DUp    = (0,1)
    directionVector DDown  = (0,-1)
    directionVector DLeft  = (-1,0)
    directionVector DRight = (1,0)


    data World = World { playerId :: String
                       , events   :: [GameEvent]
                       , units    :: [Unit]
                       , currPic  :: Picture}

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
                          inventory :: [ItemInfo],
                          serialId  :: Maybe String}
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
                      show (Creature p _ sp sl h r _ i) = "Creature(" ++ show i ++ "): " ++ show p ++ (';':show sp) ++ (';':show sl) ++ (';':show h) ++ (';':show r)
                      show (Projectile p _ sp _)        = "Projectile: " ++ show p ++ (';':show sp)
                      show (Item p _ _)                 = "Item: " ++ show p
                      show (Effect p _ _ _ _)           = "Effect: " ++ show p
                      show (Wall p _ sd)                = "Wall: " ++ show p ++ (';':show sd)

    debugShowUnits (World s es us _) = (\x -> return $ World s es x blank) =<< mapM (\x -> print x >> return x) us

