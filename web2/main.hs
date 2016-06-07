{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|


Produtos json
   nome Text
   valor Double 
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET 
/prod ProdR GET POST 
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------

getProdR :: Handler Html
getProdR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
  [whamlet| 
    <form>
        Nome: <input type="text" id="nome">
        Valor: <input type="number" id="valor">
    <button #btn> OK
  |]  
  toWidget [julius|
     $(main);
     function main(){
         $("#btn").click(function(){
             $.ajax({
                 contentType: "application/json",
                 url: "@{ProdR}",
                 type: "POST",
                 data: JSON.stringify({"nome":$("#nome").val(), "valor":$("#valor").val()}),
                 success: function(data) {
                     alert(data.resp);
                     $("#nome").val(""),
                     $("#valor").val("");
                 }
            })
         });
     }
  |]

postProdR:: Handler ()
postProdR = do
    prod <- requireJsonBody :: Handler Produtos
    runDB $ insert prod
    sendResponse (object [pack "resp" .= pack "CREATED"])
    -- Linha 60: Le o json {nome:"Teste"} e converte para
    -- Cliente "Teste". O comando runDB $ insert (Clientes "Teste")
    -- {resp:"CREATED"}
    
getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet| 
    <h1> Ola Mundo
|] 

connStr = "dbname=d7eaanmqf3fql5 host=ec2-23-21-165-183.compute-1.amazonaws.com user=qpgypdhkhkmvdx password=ABwQOGG99Ol98AixMF8l7Ta8Dn port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)


