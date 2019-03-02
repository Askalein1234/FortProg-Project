module Helper where

fromJust :: Maybe a -> a
fromJust (Just b) = b
fromJust Nothing = error "GEHT NICHT!!! ANGEWACHSEN!!!"

tupelFirst :: (a, b) -> a
tupelFirst (x, y) = x

isNothing :: Maybe a -> Bool
isNothing (Just _)  = False
isNothing Nothing   = True

processMaybe :: Maybe a -> (a -> b) -> b -> b
processMaybe (Just x) f _ = f x
processMaybe Nothing  _ x = x