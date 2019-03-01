module Helper(fromJust) where

fromJust :: Maybe a -> a
fromJust (Just b) = b
fromJust Nothing = error "GEHT NICHT!!! ANGEWACHSEN!!!"
