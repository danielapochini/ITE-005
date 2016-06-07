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
-- 15, 16, 17, 21 e 22
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Clientes json
   nome Text
   deriving Show
   
Produtoz json
   nome Text
   valor Double
   deriving Show
   
ClientesProdutos json
   clid ClientesId
   prid ProdutozId
   UniqueClientesProdutos clid prid
|]


mkYesod "Pagina" [parseRoutes|
/cadastro UserR GET POST OPTIONS
/cadastro/action/#ClientesId ActionR GET PUT DELETE
/produto ProdutoR GET POST
/venda VendaR POST
/venda/check/#ClientesId VendaCliR GET
|]

{--
curl https://haskell2-danpochini.c9users.io/cadastro \
  -v \
  -X POST \
  -H 'Content-Type: application/json' \
  -d '{"nome":"Dan"}'
--}

{--
curl https://haskell2-danpochini.c9users.io/cadastro/action/1 \
-v \
-X PUT \
-H "content-type: application/json" \
-d '{"nome":"Daniela"}'
--}

{--
curl https://haskell2-danpochini.c9users.io/cadastro/action/26 \
-v \
-X DELETE
--}

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------
getUserR :: Handler ()
getUserR = do
    allClientes <- runDB $ selectList [] [Asc ClientesNome]
    sendResponse (object [pack "data" .= fmap toJSON allClientes])

postUserR :: Handler ()
postUserR = do
    clientes <- requireJsonBody :: Handler Clientes
    runDB $ insert clientes
    sendResponse (object [pack "resp" .= pack "CREATED"])

getActionR :: ClientesId -> Handler ()
getActionR cid = do
    cliente <- runDB $ get404 cid
    sendResponse $ toJSON cliente

deleteActionR :: ClientesId -> Handler ()
deleteActionR cid = do
    runDB $ delete cid
    sendResponse (object [pack "resp" .= pack "DELETED"])
    
putActionR :: ClientesId -> Handler ()
putActionR cid = do
    cliente <- requireJsonBody :: Handler Clientes
    runDB $ update cid [ClientesNome =. clientesNome cliente]
    sendResponse (object [pack "resp" .= pack "UPDATE"])

optionsUserR :: Handler ()
optionsUserR = addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"

getProdutoR :: Handler ()
getProdutoR = do
    allProd <- runDB $ selectList [] [Asc ProdutozValor]
    sendResponse (object [pack "data" .= fmap toJSON allProd])
    
postProdutoR :: Handler ()
postProdutoR = do
    prod <- requireJsonBody :: Handler Produtoz
    runDB $ insert prod
    sendResponse (object [pack "resp" .= pack "CREATED"])

postVendaR :: Handler ()
postVendaR = do
    venda <- requireJsonBody :: Handler ClientesProdutos
    runDB $ insert venda
    sendResponse (object [pack "resp" .= pack "CREATED"])
    
{--
getVendaCliR :: ClientesId -> Handler ()    
getVendaCliR cid = do
    xs <- runDB $ (rawSql (pack $ "SELECT ??, ??, ?? FROM produtoz  \ 
        \ INNER JOIN clientes_produtos ON produtoz.id=clientes_produtos.prid \ 
        \ INNER JOIN clientes ON  clientes.id=clientes_produtos.clid \
        \ WHERE clientes_produtos.clid = " ++ (show $ fromSqlKey cid)) []) :: Handler [(Entity Produtoz,Entity ClientesProdutos,Entity Clientes)]
    sendResponse (object [pack "data" .= fmap (toJSON . (\(p,_,_) -> p)) xs])
--}

connStr = "dbname=d7eaanmqf3fql5 host=ec2-23-21-165-183.compute-1.amazonaws.com user=qpgypdhkhkmvdx password=ABwQOGG99Ol98AixMF8l7Ta8Dn port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)


