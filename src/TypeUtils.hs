{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module TypeUtils where

import Data.Kind (Type)
import Data.Typeable

data Elem xs x where
  Here :: Elem (x ': xs) x
  There :: Elem xs x -> Elem (y ': xs) x

class Indexable xs x where
  position :: Elem xs x

instance {-# OVERLAPS #-} Indexable (e : es) e where
  position = Here

instance {-# OVERLAPS #-} (Indexable es e) => Indexable (e' : es) e where
  position = There position

type Subset xs ys = TypedList xs (Elem ys)

class Subsettable as bs where
  subset :: Subset as bs

instance {-# OVERLAPPABLE #-} Subsettable '[] bs where
  subset = TNil

instance {-# OVERLAPS #-} (Subsettable as bs, Indexable bs x) => (Subsettable (x ': as) bs) where
  subset = TCons position subset

class Empty (types :: [Type]) where
  emptyTypeList :: (forall a. f a) -> TypedList types f

instance {-# OVERLAPS #-} Empty '[] where
  emptyTypeList _ = TNil

instance {-# OVERLAPS #-} (Empty es) => Empty (e : es) where
  emptyTypeList x = TCons x (emptyTypeList x)

data TypedList (types :: [Type]) (f :: Type -> Type) where
  TNil :: TypedList '[] f
  TCons :: f e -> TypedList es f -> TypedList (e : es) f

instance Show (TypedList '[] f) where
  show _ = ""

typeName :: Typeable e => Proxy e -> String
typeName p = show (typeRep p)

instance (Typeable (f e), Show (f e), Show (TypedList es f)) => Show (TypedList (e : es) f) where
  --  show _ = ""
  show (TCons x xs) = "[" ++ show (typeOf x) ++ "=" ++ show x ++ "]" ++ show xs

getValue :: (Indexable es e) => TypedList es f -> f e
getValue = getValue' position

mapTypedList :: (forall a. f a -> g a) -> TypedList es f -> TypedList es g
mapTypedList _ TNil = TNil
mapTypedList f (TCons x xs) = TCons (f x) (mapTypedList f xs)

traverseTypeList :: Monad m => (forall a. f a -> m (g a)) -> TypedList es f -> m (TypedList es g)
traverseTypeList _ TNil = return TNil
traverseTypeList f (TCons x xs) = TCons <$> f x <*> traverseTypeList f xs

getValue' :: Elem es e -> TypedList es f -> f e
getValue' Here (TCons x _) = x
getValue' (There s) (TCons _ xs) = getValue' s xs

updateValue :: (Indexable es e) => f e -> TypedList es f -> TypedList es f
updateValue = updateValue' position

updateValue' :: Elem es e -> f e -> TypedList es f -> TypedList es f
updateValue' Here x (TCons _ xs) = TCons x xs
updateValue' (There s) x (TCons y xs) = TCons y (updateValue' s x xs)

modifyValue' :: Elem es e -> (f e -> f e) -> TypedList es f -> TypedList es f
modifyValue' Here f (TCons x xs) = TCons (f x) xs
modifyValue' (There s) f (TCons y xs) = TCons y (modifyValue' s f xs)

promoteElement :: Elem as a -> Subset as bs -> Elem bs a
promoteElement Here (TCons eb _) = eb
promoteElement (There e) (TCons _ es) = promoteElement e es
