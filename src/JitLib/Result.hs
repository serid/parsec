module JitLib.Result where

data Result e t = Ok t | Err e deriving Show

instance Functor (Result e) where
    fmap f (Ok t) = Ok (f t)
    fmap _ (Err e) = Err e

instance Applicative (Result e) where
    pure = Ok
    (Ok f) <*> (Ok t) = Ok (f t)
    (Err e) <*> _ = Err e
    (Ok _) <*> (Err e) = Err e

instance Monad (Result e) where
    (Ok t) >>= k = k t
    (Err e) >>= _ = Err e

-- note: cannot implement proper Alternative since there is no "empty" value
(<||>) :: Result e t -> Result e t -> Result e t
(Ok t) <||> _ = Ok t
(Err _) <||> r = r

unwrap :: Result e t -> t
unwrap (Ok t) = t
unwrap (Err e) = undefined

