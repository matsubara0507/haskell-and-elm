{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import           Data.Extensible
import           Data.Functor.Identity
import           Web.FormUrlEncoded
import           Web.HttpApiData

instance Forall (KeyTargetAre KnownSymbol FromHttpApiData) xs => FromForm (Record xs) where
  fromForm form =
    hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol FromHttpApiData)) $ \m ->
      let k = stringKeyOf m in Field <$> parseUnique k form

instance FromHttpApiData a => FromHttpApiData (Identity a) where
  parseUrlPiece = fmap pure . parseUrlPiece
