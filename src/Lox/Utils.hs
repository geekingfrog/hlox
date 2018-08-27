{-# LANGUAGE LambdaCase #-}

module Lox.Utils where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= \case
    True  -> t
    False -> f

headMay :: [a] -> Maybe a
headMay []      = Nothing
headMay (x : _) = Just x

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe xs = tail xs
