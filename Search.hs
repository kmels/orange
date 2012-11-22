module Search where

import Data.Maybe(fromJust,catMaybes)
import Data.Either(rights)
import Indexer((<:>))
import Control.Applicative((<|>))

import Database.Redis
import qualified Data.ByteString.Char8 as BS  

-- | Searches for filepaths given a query
searchFilePaths :: String -> Connection -> Maybe Int -> IO [BS.ByteString]
searchFilePaths query redisConn maxResults = do
  fids <- runRedis redisConn $ do
    zrange ("search" <:> query) 0 (fromIntegral . fromJust $ maxResults <|> (Just $ -1)) -- :: Either Reply [BS.ByteString]
  case fids of
    (Right fids') -> do 
      filePathMaybes <- mapM (\fid -> runRedis redisConn $ do
                                 get ("filePath" <:> BS.unpack fid)
                             ) fids'
      return $ catMaybes . rights $ filePathMaybes
    _ -> return []