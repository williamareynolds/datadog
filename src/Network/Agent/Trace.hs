-- | Functions for manually instrumenting Datadog traces to send to a datadog agent.
--
-- It is recommended to read the APM Advanced Guide "Send traces to the Agent by
-- API" at https://docs.datadoghq.com/tracing/guide/send_traces_to_agent_by_api/

{-# LANGUAGE DuplicateRecordFields #-}

module Network.Agent.Trace ( withSpan
                           , withTrace
                           , HasServiceName(..)
                           , HasTraceTags(..)
                           , HasTraceID(..)
                           , HasSpanID(..)
                           , CanIncrementSpanID(..)
                           ) where

import Prelude hiding (error, span)

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Word (Word64, Word8)
import qualified Data.Map.Strict as MapS
import qualified Data.Text as T
import System.Random (randomIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.RWS (MonadReader, MonadWriter, MonadIO, liftIO, asks, tell)
import Control.Concurrent (MVar, takeMVar, putMVar, swapMVar, readMVar)

type SpanName = T.Text
type ResourceName = T.Text
type ServiceName = T.Text
type ID = Word64

type Traceable r m = ( MonadReader r m
                     , MonadWriter Trace m
                     , MonadIO m
                     , HasServiceName r
                     , HasTraceTags r
                     , HasTraceID r
                     , HasSpanID r
                     , CanIncrementSpanID r
                     )

newtype Traces = Traces { getTraces :: [Trace] }
  deriving ( ToJSON
           , Show
           )

newtype Trace = Trace { getSpans :: [Span] }
  deriving ( ToJSON
           , Show
           )

-- | The number of nanoseconds since the epoch.
type NanoPOSIXTime = Word64
type Nanoseconds = Word64

data SpanType =
    Web
  | Database
  | Cache
  | Custom
  deriving (Show)

instance ToJSON SpanType where
  toJSON Web      = toJSON ("web" :: T.Text)
  toJSON Database = toJSON ("db" :: T.Text)
  toJSON Cache    = toJSON ("cache" :: T.Text)
  toJSON Custom   = toJSON ("custom" :: T.Text)

class HasServiceName r where
  getServiceName :: r -> ServiceName

class HasTraceTags r where
  getTraceTags :: r -> MapS.Map T.Text T.Text

class HasTraceID r where
  getTraceID :: r -> MVar ID

class HasSpanID r where
  getSpanID :: r -> MVar ID

class CanIncrementSpanID r where
  incrementSpanID :: r -> r

data Span = Span { duration :: Nanoseconds
                 , error :: Word8
                 , meta :: MapS.Map T.Text T.Text
                 , metrics :: MapS.Map T.Text Double
                 , name :: SpanName
                 , parentID :: ID
                 , resource :: ResourceName
                 , service :: ServiceName
                 , spanID :: ID
                 , start :: NanoPOSIXTime
                 , traceID :: ID
                 } deriving (Show)

instance ToJSON Span where
  toJSON span =
    object [ "duration"  .= duration span
           , "error"     .= if error span >= 1 then (1 :: Word64) else 0
           , "meta"      .= meta span
           , "metrics"   .= metrics span
           , "name"      .= name span
           , "parent_id" .= parentID span
           , "resource"  .= resource span
           , "service"   .= service span
           , "span_id"   .= spanID span
           , "start"     .= start span
           , "trace_id"  .= traceID span
           ]

-- | Get a random trace ID.
getRandomTraceID :: IO Word64
getRandomTraceID = randomIO

-- | Return the current time in nanoseconds.
getNanoTime :: MonadIO m => m NanoPOSIXTime
getNanoTime = do
  time <- liftIO getPOSIXTime
  return . round $ time * 1000000

type ParentSpanID = ID
type NextSpanID = ID

-- | Get the span IDs needed for this span.
getSpanIDs :: MVar ID -> IO (ParentSpanID, NextSpanID)
getSpanIDs mv = do
  val <- takeMVar mv
  let nextVal = val + 1
  putMVar mv nextVal
  return (val, nextVal)

-- | Start a new trace with an initial span.
withTrace :: Traceable r m
          => SpanName
          -> ResourceName
          -> m a
          -> m a
withTrace name resourceName action = do
  traceIDMVar <- asks getTraceID
  newTraceID  <- liftIO getRandomTraceID
  _ <- liftIO $ swapMVar traceIDMVar newTraceID
  withSpan name resourceName action

-- | Perform an action in a new span.
withSpan :: Traceable r m
         => SpanName
         -> ResourceName
         -> m a
         -> m a
withSpan spanName resourceName action = do
  startTime   <- getNanoTime
  res         <- action
  endTime     <- getNanoTime
  serviceName <- asks getServiceName
  tags        <- asks getTraceTags
  traceIDMVar <- asks getTraceID
  traceID'    <- liftIO $ readMVar traceIDMVar
  spanIDMVar  <- asks getSpanID
  (parentID', currentID) <- liftIO $ getSpanIDs spanIDMVar
  let dur = endTime - startTime
      span = Span { duration = dur
                  , error = 0 -- Add error handling
                  , meta = tags
                  , metrics = MapS.empty
                  , name = spanName
                  , parentID = parentID'
                  , resource = resourceName
                  , service = serviceName
                  , spanID = currentID
                  , start = startTime
                  , traceID = traceID'
                  }
  tell $ Trace [span]
  return res
