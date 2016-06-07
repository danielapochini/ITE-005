{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Import
import Yesod
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql


connStr = "dbname=d7eaanmqf3fql5 host=ec2-23-21-165-183.compute-1.amazonaws.com user=qpgypdhkhkmvdx password=ABwQOGG99Ol98AixMF8l7Ta8Dn port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Sitio pool)