{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             TupleSections, OverloadedStrings,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Produtox json
   nome Text
   valor Double
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/produto/cadastro ProdutoR GET POST
/produto/checar/#ProdutoxId ChecarProdR GET
/erro ErroR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

-- renderiza a pagina no idioma de quem a acessa
instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage 
    
------------------------

-- Sempre que preciso um form, sera necessario
-- funcoes deste tipo
formProd :: Form Produtox
formProd = renderDivs $ Produtox <$> -- coloca Produtox pra dentro da Monad Form 
           --renderDivs encapsular cada entidade do formulario dentro de uma div
           areq textField "Nome: " Nothing <*> 
           -- <*> pq é uma função areq joga tudo pra dentro de Form
           areq doubleField "Valor: " Nothing  
           -- Nothing pq o campo começa vazio
           
           
                    --interpoladores: @ -> Para rotas, 
                    -- # -> para comandos haskell 
                    -- ^-> injeta html no html
                    --interpolador, coloca haskell dentro do html, tem q colocar a variavel enctype dentro das {}

getProdutoR :: Handler Html
getProdutoR = do
           (widget, enctype) <- generateFormPost formProd
           defaultLayout $ do 
           toWidget [cassius|
               label
                   color:red;
           |]
           [whamlet|
                 <form method=post enctype=#{enctype} action=@{ProdutoR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]


postProdutoR :: Handler Html
postProdutoR = do
           ((result, _), _) <- runFormPost formProd
           case result of 
               FormSuccess prod -> (runDB $ insert prod) >>= \piid -> redirect (ChecarProdR piid)
               _ -> redirect ErroR
           
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getChecarProdR :: ProdutoxId -> Handler Html
getChecarProdR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <p><b> #{produtoxNome produto}  
        <p><b> #{show $ produtoxValor produto}
    |]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    cadastro deu pau com sucesso
|]

connStr = "dbname=d7eaanmqf3fql5 host=ec2-23-21-165-183.compute-1.amazonaws.com user=qpgypdhkhkmvdx password=ABwQOGG99Ol98AixMF8l7Ta8Dn port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
