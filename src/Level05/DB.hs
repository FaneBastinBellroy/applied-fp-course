{-# LANGUAGE OverloadedStrings #-}

module Level05.DB
  ( FirstAppDB (FirstAppDB),
    initDB,
    closeDB,
    addCommentToTopic,
    getComments,
    getTopics,
    deleteTopic,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.SQLite.Simple
  ( Connection,
    Query (fromQuery),
  )
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.SimpleErrors as Sql
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import Level05.AppM (AppM (..))
import Level05.DB.Types (DBComment)
import Level05.Types
  ( Comment,
    CommentText,
    Error (DBError),
    Topic,
    fromDBComment,
    getCommentText,
    getTopic,
    mkTopic,
  )

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB ::
  FirstAppDB ->
  IO ()
closeDB =
  Sql.close . dbConn

initDB ::
  FilePath ->
  IO (Either SQLiteResponse FirstAppDB)
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
    -- Query has an `IsString` instance so string literals like this can be
    -- converted into a `Query` type when the `OverloadedStrings` language
    -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB ::
  (a -> Either Error b) ->
  IO a ->
  AppM b
runDB f ioA =
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
  AppM $ f <$> ioA

-- Move your use of DB.runDBAction to this function to avoid repeating
-- yourself in the various DB functions.

getComments ::
  FirstAppDB ->
  Topic ->
  AppM [Comment]
getComments (FirstAppDB conn) t =
  let sql = "SELECT id,topic,comment,time FROM comments WHERE topic = @topic"
      queryToRun = Sql.queryNamed conn sql ["@topic" Sql.:= getTopic t] :: IO [DBComment]
      replaceBits :: Either SQLiteResponse [DBComment] -> Either Error [Comment]
      replaceBits res = case res of
        Left e -> Left $ DBError e
        -- If any DBComment turns into an error, throw an overall error
        Right r -> traverse fromDBComment r
   in runDB replaceBits (Sql.runDBAction queryToRun)

addCommentToTopic ::
  FirstAppDB ->
  Topic ->
  CommentText ->
  AppM ()
addCommentToTopic =
  error "Copy your completed 'appCommentToTopic' and refactor to match the new type signature"

getTopics ::
  FirstAppDB ->
  AppM [Topic]
getTopics =
  error "Copy your completed 'getTopics' and refactor to match the new type signature"

deleteTopic ::
  FirstAppDB ->
  Topic ->
  AppM ()
deleteTopic =
  error "Copy your completed 'deleteTopic' and refactor to match the new type signature"

-- Go to 'src/Level05/Core.hs' next.
