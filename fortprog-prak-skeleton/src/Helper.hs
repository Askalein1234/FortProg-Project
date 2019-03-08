module Helper where

-- get a Just from a Maybe (you have to make sure it's a Just)
fromJust :: Maybe a -> a
fromJust (Just b) = b
-- Who doesn't know the Kangaroo Chronicles? CHANGE THAT!!!
fromJust Nothing  = error "GEHT NICHT!!! ANGEWACHSEN!!!"

-- get the first thing from a 2-tupel
tupelFirst :: (a, b) -> a
tupelFirst (x, _) = x

-- get the second thing from a 2-tupel
tupelSecond :: (a, b) -> b
tupelSecond (_, x) = x

-- find out if Maybe is Nothing
isNothing :: Maybe a -> Bool
isNothing (Just _)  = False
isNothing Nothing   = True

-- apply a function to the content of Just if Just
-- or be the neutral elemement otherwise
processMaybe :: Maybe a -> (a -> b) -> b -> b
processMaybe (Just x) f _ = f x
processMaybe Nothing  _ x = x

-- replace nth Element in list
replaceElem :: Int -> (a -> a) -> [a] -> [a]
replaceElem _ _ []     = []
replaceElem n f (x:xs) | n == 0    = ((f x):xs)
                       | otherwise = x : (replaceElem (n-1) f xs)
