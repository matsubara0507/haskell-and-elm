{-# LANGUAGE FlexibleInstances #-}

module Elm.Class where

import           Data.Functor.Identity
import           Data.Text
import           Elm

class ToElmValue a where
  toElmValue :: a -> ElmValue

instance Monoid ElmValue where
  mempty = ElmEmpty
  mappend ElmEmpty a        = a
  mappend a ElmEmpty        = a
  mappend (Values a1 a2) a3 = Values a1 $ mappend a2 a3
  mappend a1 a2             = Values a1 a2

instance ElmType a => ElmType (Identity a) where
  toElmType (Identity a) = toElmType a

elmTypeToElmValue :: ElmType a => a -> ElmValue
elmTypeToElmValue a =
  case toElmType a of
    (ElmDatatype name _) -> ElmRef name
    (ElmPrimitive prim)  -> ElmPrimitiveRef prim

instance ElmType ElmDatatype where
  toElmType = id

toElmRecordType :: ToElmValue a => Text -> a -> ElmDatatype
toElmRecordType name = ElmDatatype name . RecordConstructor name . toElmValue
