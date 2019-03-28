

module Experimenter.Parameter.Query
    ( queryParamSettings
    ) where


import           Control.Monad.Reader
import           Database.Esqueleto

import           Experimenter.Models


queryParamSettings :: (MonadIO m) => Key Exp -> ReaderT SqlBackend m [(Entity Exp, Entity ParamSetting)]
queryParamSettings kExp =
  select $
  from $ \(exp, paramSet) -> do
    where_ (exp ^. ExpId ==. paramSet ^. ParamSettingExp)
    where_ (exp ^. ExpId ==. val kExp)
    orderBy [asc (exp ^. ExpNumber)]
    return (exp, paramSet)


