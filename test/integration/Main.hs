{-# LANGUAGE TypeApplications #-}

import           Config.Common (DatabaseConfig (..), pgConnectionString)
import           Control.Monad.Logger
import           Data.Pool (Pool)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import           Database.Persist.Postgresql (ConnectionString, SqlBackend, createPostgresqlPool)
import           Test.Tasty (TestTree, askOption, defaultIngredients, defaultMainWithIngredients,
                   includingOptions, localOption, testGroup)
import           Test.Tasty.Options (IsOption (..), OptionDescription (..))
import           Test.Tasty.Runners (NumThreads (..))

import qualified Cardano.Catalyst.Query.Esqueleto as Esql
import qualified Cardano.Catalyst.Query.Sql as Sql
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Test.Tasty as Tasty

import qualified Test.Cardano.Catalyst.Db
import qualified Test.Cardano.Catalyst.Query

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions dbOptions : defaultIngredients)
    tests

withPostgresPool :: ConnectionString -> (IO (Pool SqlBackend) -> TestTree) -> TestTree
withPostgresPool connStr =
  Tasty.withResource
   (runNoLoggingT $ createPostgresqlPool connStr numConnections)
   Pool.destroyAllResources
  where
    numConnections = 1

tests :: TestTree
tests =
  -- Force sequential execution, as these database tests need to execute one
  -- after the other.
  localOption (NumThreads 1) $
  -- Get database config
  askOption $ \(DbName dbName) ->
  askOption $ \(DbUser dbUser) ->
  askOption $ \(DbHost dbHost) ->
  askOption $ \(DbPass mDbPass) ->
  let
    connStr = pgConnectionString
      $ DatabaseConfig (T.unpack dbName) (T.unpack dbUser) (T.unpack dbHost) (T.unpack <$> mDbPass)
  in do
    -- Establish and share postgres connection between tests
    withPostgresPool connStr $ \getConnPool -> do
      testGroup "Integration tests"
        [ Test.Cardano.Catalyst.Db.tests (Sql.sqlQuery) getConnPool
        , Test.Cardano.Catalyst.Db.tests (Esql.esqlQuery) getConnPool
        , Test.Cardano.Catalyst.Query.tests (Sql.sqlQuery) getConnPool
        , Test.Cardano.Catalyst.Query.tests (Esql.esqlQuery) getConnPool
        ]

newtype DbName = DbName Text
newtype DbUser = DbUser Text
newtype DbHost = DbHost Text
newtype DbPass = DbPass (Maybe Text)

instance IsOption DbName where
  defaultValue = DbName ""
  parseValue str = Just $ DbName $ T.pack str
  optionName = return "db-name"
  optionHelp = return "Name of Postgres cardano-db-sync database to use for testing (WARNING will wipe database!)."

instance IsOption DbUser where
  defaultValue = DbUser ""
  parseValue str = Just $ DbUser $ T.pack str
  optionName = return "db-user"
  optionHelp = return "User of Postgres cardano-db-sync database to use for testing (WARNING will wipe database!)."

instance IsOption DbHost where
  defaultValue = DbHost ""
  parseValue str = Just $ DbHost $ T.pack str
  optionName = return "db-host"
  optionHelp = return "Host of Postgres cardano-db-sync database to use for testing (WARNING will wipe database!)."

instance IsOption DbPass where
  defaultValue = DbPass Nothing
  parseValue str = Just $ DbPass $ Just $ T.pack str
  optionName = return "db-pass"
  optionHelp = return "Password of Postgres cardano-db-sync database to use for testing (WARNING will wipe database!)."

dbOptions :: [OptionDescription]
dbOptions =
  [ Option (Proxy @DbName)
  , Option (Proxy @DbUser)
  , Option (Proxy @DbHost)
  , Option (Proxy @DbPass)
  ]
