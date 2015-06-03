{-# LANGUAGE OverloadedStrings #-}

module Redmine.Rest(expandOptions
                   , increaseQueryRange
                   , initOpt
                   , toJsonBody
                   , queryRedmineAvecOptions
                   , runQuery

                   , ParamRest
                   ) where

import Data.Aeson
import Data.Monoid
import qualified Data.Map as Map
import Redmine.Types
import Redmine.Manager
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Network.HTTP.Base (RequestMethod(..))
import Network.HTTP.Types.Header (Header, hContentType)
import qualified Data.Text as T

import Debug.Trace

queryRedmine :: RedmineMng -> RequestMethod -> S.ByteString -> (Maybe S.ByteString) -> IO L.ByteString
queryRedmine mng m req b = do
          request <- creerRqt mng m req b
          let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
          response  <- withManagerSettings settings $ httpLbs request
          return $ responseBody response

creerRqt :: RedmineMng -> RequestMethod -> S.ByteString -> Maybe S.ByteString -> IO Request
creerRqt (RedmineMng h) m r body                       =  fmap (f body) $ parseUrl $ S8.unpack (h <> r)
  where
    ct = (hContentType, "application/json") :: Header
    f (Just b) req = trace (S8.unpack b) req'
      where req' = req{method = S8.pack . show $ m, requestHeaders = ct : requestHeaders req, requestBody = RequestBodyBS b}
    f Nothing  req = req{method = S8.pack . show $ m, requestHeaders = ct : requestHeaders req}

creerRqt (RedmineMngWithProxy h u p) m r b               = fmap (addProxy u p) (creerRqt (RedmineMng h) m r b)
creerRqt (RedmineMngWithAuth h l pass) m r b             = fmap (applyBasicAuth l pass) (creerRqt (RedmineMng h) m r b)
creerRqt (RedmineMngWithAuthAndProxy h l pass u p) m r b = fmap (applyBasicAuth l pass) (creerRqt (RedmineMngWithProxy h u p) m r b)

type ParamRest = Map.Map S.ByteString S.ByteString

-- Remplace par urlEncodedBody
expandOptions :: ParamRest -> S.ByteString
expandOptions = Map.foldrWithKey (\k a res -> res <> k <> "=" <> a <> "&") "?"

bsAInt :: S.ByteString -> Int
bsAInt = read . S8.unpack

increaseQueryRange :: ParamRest -> ParamRest
increaseQueryRange param =
  let offset = bsAInt $ Map.findWithDefault "0" "offset" param
      limit  = bsAInt $ Map.findWithDefault "100" "limit" param
      nouvelOffset = offset + limit
  in Map.insert "offset" (S8.pack $ show nouvelOffset) param

-- Réécrire avec les autres modes
queryRedmineAvecOptions :: (FromJSON a, Monoid a, Collection a) =>
                           RedmineMng -> RequestMethod -> S.ByteString -> ParamRest -> Maybe S.ByteString -> Manager -> IO( Maybe a)
queryRedmineAvecOptions redmineMng m req param body mng = -- MaybeT IO $ withSocketsDo $
  do
    request   <- creerRqt redmineMng m (req <> expandOptions param) body
    --traceM (S8.unpack $ (req <> (expandOptions param)))
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    response  <- withManagerSettings settings $ httpLbs request
    parsedRes <- debugResult . eitherDecode . responseBody $ response
    --putStrLn $ show parsedRes
    case parsedRes of
      Just a | 0 == longueur a -> return $ Just a
             | otherwise       ->
                do let hausse = increaseQueryRange param
                   --traceM . show $ hausse
                   reste <- queryRedmineAvecOptions redmineMng m req hausse body mng
                   case reste of
                      Just b -> return . Just $ mappend a b
                      Nothing -> return $ Just a
                   --return (reste >>= (\b -> Just $ mappend a b ))

      Nothing -> return Nothing

-- |The function 'debugResult' is used to print the parsing error statement
-- and continue the processing.
debugResult :: Either String a -> IO(Maybe a)
debugResult res = case res of
                    Left msg -> putStrLn msg >> return Nothing
                    Right v  -> return (Just v)

--contentType = mkHeader HdrContentType "application/json"

runQuery :: FromJSON a => RedmineMng -> RequestMethod -> S.ByteString -> Maybe S.ByteString -> IO( Maybe a)
runQuery mng m requete body = do -- withSocketsDo $ do
  toto <- queryRedmine mng m requete body
  (debugResult . eitherDecode) toto

toJsonBody :: ToJSON a => a -> Maybe S8.ByteString
toJsonBody = Just . L8.toStrict . encode . toJSON

initOpt = Map.fromList [("offset","0"), ("limit","100")] :: Map.Map S8.ByteString S8.ByteString
