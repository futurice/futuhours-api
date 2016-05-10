module Database.PostgreSQL.Simple.Fxtra (
    singleQuery,
    module Database.PostgreSQL.Simple,
    module Database.PostgreSQL.Simple.Transaction,
    ToField(..),
    FromField(..),
    ) where

import Control.Monad                          (liftM)
import Control.Monad.IO.Class                 (MonadIO (..))
import Data.Maybe                             (listToMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField     (ToField (..))
import Database.PostgreSQL.Simple.FromField     (FromField (..))
import Database.PostgreSQL.Simple.Transaction

-- | TODO: rename to 'queryFirst'
singleQuery :: (ToRow q, FromRow r, MonadIO m) => Connection -> Query -> q -> m (Maybe r)
singleQuery conn q s =
    liftIO (listToMaybe `liftM` query conn q s)
