{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module TypeUtils where

import Data.Kind (Type)

data Elem xs x where
  Here :: Elem (x ': xs) x
  There :: Elem xs x -> Elem (y ': xs) x

class Indexable xs x where
  position :: Elem xs x

instance {-# OVERLAPS #-} Indexable (e : es) e where
  position = Here

instance {-# OVERLAPS #-} (Indexable es e) => Indexable (e' : es) e where
  position = There position

data All f xs where
  Nil :: All f '[]
  Cons :: f x -> All f xs -> All f (x ': xs)

type IsSubset xs ys = All (Elem ys) xs

data TypedList (types :: [Type]) (f :: e -> Type) where
  TNil :: TypedList '[] f
  TCons :: f e -> TypedList es f -> TypedList (e : es) f

getValue :: (Indexable es e) => TypedList es f -> f e
getValue = getValue' position
  where
    getValue' :: Elem es e -> TypedList es f -> f e
    getValue' Here (TCons x _) = x
    getValue' (There s) (TCons _ xs) = getValue' s xs

updateValue :: (Indexable es e) => f e -> TypedList es f -> TypedList es f
updateValue = updateValue' position
  where
    updateValue' :: Elem es e -> f e -> TypedList es f -> TypedList es f
    updateValue' Here x (TCons _ xs) = TCons x xs
    updateValue' (There s) x (TCons y xs) = TCons y (updateValue' s x xs)
