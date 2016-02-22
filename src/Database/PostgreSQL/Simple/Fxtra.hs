module Database.PostgreSQL.Simple.Fxtra (
    singleQuery,
    module Database.PostgreSQL.Simple,
    ) where

import Control.Monad              (liftM)
import Control.Monad.IO.Class     (MonadIO (..))
import Data.Maybe                 (listToMaybe)
import Database.PostgreSQL.Simple

-- | TODO: rename to 'queryFirst'
singleQuery :: (ToRow q, FromRow r, MonadIO m) => Connection -> Query -> q -> m (Maybe r)
singleQuery conn q s =
    liftIO (listToMaybe `liftM` query conn q s)
