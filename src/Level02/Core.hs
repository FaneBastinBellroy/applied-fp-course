{-# LANGUAGE OverloadedStrings #-}

module Level02.Core (runApp, app) where

import Control.Monad (join)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (either)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Level02.Types
  ( ContentType (..),
    Error (..),
    RqType (..),
    errorLBS,
    mkCommentText,
    mkTopic,
    renderContentType,
  )
import Network.HTTP.Types
  ( Status,
    hContentType,
    status200,
    status400,
    status404,
  )
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    requestMethod,
    responseLBS,
    strictRequestBody,
  )
import Network.Wai.Handler.Warp (run)

-- | -------------------------------------------|
--  |- Don't start here, go to Level02.Types!  -|
--  |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse ::
  Status ->
  ContentType ->
  LBS.ByteString ->
  Response
mkResponse s cType =
  responseLBS s [(hContentType, renderContentType cType)]

resp200 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp200 = mkResponse status200

resp404 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp404 = mkResponse status404

resp400 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp400 = mkResponse status400

-- | ----------------------------------------------------------------------------------
--  These next few functions will take raw request information and construct         --
--  one of our types.                                                                --
--                                                                                   --
--  By breaking out these smaller functions, we're able to isolate our               --
--  validation requirements into smaller components that are simpler to maintain     --
--  and verify. It also allows for greater reuse and it also means that              --
--  validation is not duplicated across the application, maybe incorrectly.          --
-- ------------------------------------------------------------------------------------
mkAddRequest ::
  Text ->
  LBS.ByteString ->
  Either Error RqType
mkAddRequest topic commentText =
  AddRq <$> mkTopic topic
    <*> mkCommentText (lazyByteStringToStrictText commentText)
  where
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest ::
  Text ->
  Either Error RqType
mkViewRequest topic =
  ViewRq <$> mkTopic topic

mkListRequest ::
  Either Error RqType
mkListRequest =
  Right ListRq

-- | ----------------------------------
--  end of RqType creation functions --
-- ------------------------------------
mkErrorResponse ::
  Error ->
  Response
mkErrorResponse e =
  resp400 PlainText $ errorLBS e

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest ::
  Request ->
  IO (Either Error RqType)
mkRequest r =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case method of
    "GET" -> case path of
      ["list"] -> pure mkListRequest
      [t, "view"] -> pure $ mkViewRequest t
      --[a] -> pure mkListRequest
      _ -> pure $ Left PathError
    "POST" -> case path of
      [t, "add"] -> mkAddRequest t <$> body
      _ -> pure $ Left PathError
    _ -> pure $ Left MethodError
  where
    method = requestMethod r
    path = pathInfo r
    body = strictRequestBody r

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest ::
  RqType ->
  Either Error Response
handleRequest (AddRq _ _) =
  Right $ resp200 PlainText "Adding comments is not implemented yet!"
handleRequest (ViewRq _) =
  Right $ resp200 PlainText "Viewing topics is not implemented yet!"
handleRequest ListRq =
  Right $ resp200 PlainText "Listing topics is not implemented yet!"

handleErrorResponse :: Either Error Response -> Response
handleErrorResponse (Right r) = r
handleErrorResponse (Left e) = mkErrorResponse e

requestToResponse :: Request -> IO Response
requestToResponse r = do
  req <- mkRequest r
  pure $ handleErrorResponse $ req >>= handleRequest

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app ::
  Application
app rq cb =
  requestToResponse rq >>= cb

runApp :: IO ()
runApp = run 3000 app
