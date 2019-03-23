

module Experimenter.Parameter.Query
    ( queryParamSettings
    , queryParamSettingsGrouped
    ) where


import           Control.Lens         (view)
import           Control.Monad.Reader
import qualified Data.Function        as F
import qualified Data.List            as L
import           Database.Esqueleto

import           Experimenter.Models


queryParamSettings :: (MonadIO m) => Key Exp -> ReaderT SqlBackend m [(Entity ExpResult, Entity ParamSetting)]
queryParamSettings kExp =
  select $
  from $ \(expRes, paramSet) -> do
    where_ (expRes ^. ExpResultId ==. paramSet ^. ParamSettingExpResult)
    where_ (expRes ^. ExpResultExp ==. val kExp)
    orderBy [asc (expRes ^. ExpResultRepetition)]
    return (expRes, paramSet)


queryParamSettingsGrouped :: (MonadIO m) => Key Exp -> ReaderT SqlBackend m [(Entity ExpResult, [Entity ParamSetting])]
queryParamSettingsGrouped kExp = do
  res <- queryParamSettings kExp
  return $ map combine $ L.groupBy ((==) `F.on` view expResultRepetition . entityVal . fst) res
  where combine :: [(Entity ExpResult, Entity ParamSetting)] -> (Entity ExpResult, [Entity ParamSetting])
        combine xs@(x:_) = (fst x, map snd xs)
        combine []       = error "not possible"
