{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- * http-conduit
import Network.HTTP.Conduit (Manager, def, newManager)

-- * http-reverse-proxy
import Network.HTTP.ReverseProxy ( ProxyDest(..)
                                  , defaultOnExc, waiProxyTo
                                  )

-- * http-types
import Network.HTTP.Types.Header (RequestHeaders)

-- * text
import Data.Text (Text, pack, unpack)

-- * transformers
import Control.Monad.IO.Class (liftIO)

-- * wai
import Network.Wai (Application, Middleware)
  
-- * wai-extra
import Network.Wai.Middleware.Rewrite (rewritePure)

-- * warp
import Network.Wai.Handler.Warp (run)

--------------------------------------------------------------------------------

-- This is designed to proxy to localhost:3000, which is running fibs2.hs
-- Specifically, for example, localhost:3001/fibs/10 proxies to
-- localhost:3000/fibs/20


main :: IO ()
main = run 3001 $ modApp proxy

proxy :: Application
proxy req = do
  manager :: Manager <- liftIO $ newManager def
  waiProxyTo (const $ return $ Right $ ProxyDest "localhost" 3000)
             defaultOnExc
             manager
             req

modApp :: Middleware
modApp = rewritePure convertPath
        

convertPath :: [Text] -> RequestHeaders -> [Text]
convertPath path0 _ = path1
  where
    len = length path0
    
    path1 = if len == 2
            then cmd : [pack (show num1)]
            else path0
    
    (cmd, num0) :: (Text, Int) = (head path0, read $ unpack $ last path0)
    num1 = 2*num0


