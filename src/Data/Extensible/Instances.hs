{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Extensible.Instances where

import           Data.Aeson               hiding (KeyValue)
import           Data.Constraint          (Dict (..))
import           Data.Extensible          ((:*), Comp (..), Const' (..),
                                           Field (..), Forall, KeyValue, Record,
                                           getConst', getField, hfoldMap,
                                           hgenerateFor, hzipWith, library,
                                           proxyAssocKey)
import           Data.Functor.Identity    (Identity (..))
import qualified Data.HashMap.Strict      as HM
import           Data.Monoid              (Endo (..))
import           Data.Proxy               (Proxy (..))
import           Data.String              (fromString)
import           Elm                      (ElmType (..), ElmValue (ElmField))
import           Elm.Class
import           GHC.TypeLits             (KnownSymbol, symbolVal)
import           Web.FormUrlEncoded       (FromForm (..), parseUnique)
import           Web.Internal.HttpApiData (FromHttpApiData (..))

instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object"
    $ \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON))
      $ \m -> let k = symbolVal (proxyAssocKey m) in
        case HM.lookup (fromString k) v of
          Just a  -> Field . return <$> parseJSON a
          Nothing -> fail $ "Missing key: " ++ k

instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Record xs) where
  toJSON rec = Object $ HM.fromList $ flip appEndo [] $ hfoldMap getConst'
    $ hzipWith (\(Comp Dict) v -> Const' $ Endo
      ((fromString $ symbolVal $ proxyAssocKey v, toJSON $ getField v) : ))
    (library :: Comp Dict (KeyValue KnownSymbol ToJSON) :* xs) rec

instance Forall (KeyValue KnownSymbol FromHttpApiData) xs => FromForm (Record xs) where
  fromForm f = hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromHttpApiData))
    $ \m -> let k = symbolVal (proxyAssocKey m) in Field <$> parseUnique (fromString k) f

instance FromHttpApiData a => FromHttpApiData (Identity a) where
  parseUrlPiece = fmap Identity . parseUrlPiece

instance Forall (KeyValue KnownSymbol ElmType) xs => ToElmValue (Record xs) where
  toElmValue rec = hfoldMap getConst'
    $ hzipWith (\(Comp Dict) v -> Const'
        $ ElmField (fromString . symbolVal $ proxyAssocKey v) (elmTypeToElmValue . toElmType $ getField v))
      (library :: Comp Dict (KeyValue KnownSymbol ElmType) :* xs) rec
