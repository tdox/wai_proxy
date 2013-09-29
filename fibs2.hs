{-# LANGUAGE TypeFamilies, QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod

import qualified Data.Text as T

data Fibs = Fibs

newtype Natural = Natural Int
                deriving (Show, Read, Eq, Num, Ord)


instance PathPiece Natural where
    toPathPiece (Natural i) = T.pack $ show i
    fromPathPiece s =
        case reads $ T.unpack s of
            (i, _):_
                | i < 1 -> Nothing
                | otherwise -> Just $ Natural i
            [] -> Nothing


mkYesod "Fibs" [parseRoutes|
/fibs/#Natural FibsR GET
|]


instance Yesod Fibs

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

getFibsR :: Natural -> GHandler Fibs Fibs RepPlain
getFibsR (Natural i) = return $ RepPlain $ toContent $ show $ fibs !! (i - 1)

main :: IO ()
main = warpDebug 3000 Fibs
