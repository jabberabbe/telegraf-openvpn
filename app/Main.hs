{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main (main) where

import           Conduit                  hiding (connect)
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Data.Conduit.Network
import           Data.CSV.Conduit
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           Network.Socket
import           System.Environment
import           System.Exit

main :: IO ()
main = do
    socketPath <- getSocketPath
    bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        connect sock (SockAddrUnix socketPath)
        race_
            (flip execStateT undefined $ runConduit $ requestStatus sock)
            (flip execStateT undefined $ runConduit $ processStatus sock)
  where
    getSocketPath = do
        args <- getArgs
        when (length args /= 1) $ do
            putStrLn "Usage: telegraf-openvpn /path/to/openvpn-manage.sock"
            exitWith $ ExitFailure 1
        pure $ head args

-- Telegraf sends "\n" on stdin every tick
requestStatus :: Socket -> ConduitT () Void (StateT UTCTime IO) ()
requestStatus sock = stdinC .| mapC (const "status 2\n") .| sinkSocket sock

processStatus :: Socket -> ConduitT () Void (StateT UTCTime IO) ()
processStatus sock = sourceSocket sock .| intoCSV defCSVSettings .|
    mapM_C (\case {
    ; [ "TIME", _, parseTimestamp -> time ] ->
      put time

    ; [ "CLIENT_LIST", commonName, realAddress, notEmpty -> virtualIp4
      , notEmpty -> virtualIp6, read -> bytesRecv, read -> bytesSent, _, parseTimestamp -> connectedSince
      , notUndef -> username, read -> clientID, read -> peerID, cipher ] ->
          lift . putStrLn . influxLineFormat ClientStatus{..} =<< get

    ; _ -> pure ()
    })
  where
    parseTimestamp = posixSecondsToUTCTime . fromIntegral . (read :: String -> Integer)

    notEmpty "" = Nothing
    notEmpty x  = Just x

    notUndef "UNDEF" = Nothing
    notUndef x       = Just x

data ClientStatus = ClientStatus
    { commonName     :: String
    , realAddress    :: String
    , virtualIp4     :: Maybe String
    , virtualIp6     :: Maybe String
    , bytesRecv      :: Word
    , bytesSent      :: Word
    , connectedSince :: UTCTime
    , username       :: Maybe String
    , clientID       :: Word
    , peerID         :: Word
    , cipher         :: String }
    deriving (Show)

influxLineFormat :: ClientStatus -> UTCTime -> String
influxLineFormat p@ClientStatus{..} time =
    "openvpn," ++

    "commonName=" ++ escape commonName ++ "," ++
    "realAddress=" ++ escape realAddress ++ "," ++
    maybe "" (\ip -> "virtualIp4=" ++ escape ip ++ ",") virtualIp4 ++
    maybe "" (\ip -> "virtualIp6=" ++ escape ip ++ ",") virtualIp6 ++
    maybe "" (\uname -> "userName=" ++ escape uname ++ ",") username ++
    "cipher=" ++ escape cipher ++ " " ++

    "bytesRecv=" ++ show bytesRecv ++ "u," ++
    "bytesSent=" ++ show bytesSent ++ "u," ++
    "connectedSince=" ++ (timeToInfluxFormat connectedSince) ++ "u," ++
    "clientId=" ++ show clientID ++ "u," ++
    "peerId=" ++ show peerID ++ "u " ++

    (timeToInfluxFormat time)
  where
    escape xs = "\"" ++ concatMap f xs ++ "\""

    f '\\' = "\\\\"
    f '"'  = "\\\""
    f x    = [x]

    timeToInfluxFormat = show . floor . (1e9 *)
      . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

