module Helper where

fromJust :: Maybe a -> a
fromJust (Just b) = b
fromJust Nothing  = error "GEHT NICHT!!! ANGEWACHSEN!!!"

tupelFirst :: (a, b) -> a
tupelFirst (x, _) = x

tupelSecond :: (a, b) -> b
tupelSecond (_, x) = x

isNothing :: Maybe a -> Bool
isNothing (Just _)  = False
isNothing Nothing   = True

processMaybe :: Maybe a -> (a -> b) -> b -> b
processMaybe (Just x) f _ = f x
processMaybe Nothing  _ x = x

replaceElem :: Int -> (a -> a) -> [a] -> [a]
replaceElem _ _ []     = []
replaceElem n f (x:xs) | n == 0    = ((f x):xs)
                       | otherwise = x : (replaceElem (n-1) f xs)
