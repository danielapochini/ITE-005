{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Users json
   nome Text
   login Text
   senha Text
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/login LoginR GET POST
/usuario UsuarioR GET POST
/perfil/#UsersId PerfilR GET
/erro ErroR GET
/admin AdminR GET
/logout LogoutR GET
/quem QuemR GET
|]

--A função 'isAuthorized' determina os acessos por rota
instance Yesod Pagina where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _   = return Authorized
    isAuthorized ErroR _    = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _   = isAdmin
    isAuthorized _ _        = isUser


--Autenticação do Admin
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing      -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _       -> Unauthorized "Você precisa ser Admin para ter acesso a essa área!"


--A função isUser faz a autenticação do Usuário
isUser = do
    -- 'lookupSession' verifica se há session, e atribui à 'mu'
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _  -> Authorized

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{LoginR}>
                     ^{widget}
                     <input type="submit" value="Login">
           |]

--'Post' dos campos do login para a autenticação do usuário
--Obs.: Funções do Banco de Dados SEMPRE têm o runDB
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               --Caso seja Admin:
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               --Caso seja Usuário Comum:
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       --Caso o User venha 'vazio'            
                       Nothing -> redirect LoginR
                       --Caso o user seja retornado com sucesso, setamos a sessão e redirecionamos para a HomeR
                       --Abaixo: "pid" é o ID, e "u" contém todos os outros campos do registro
                       --A session é setada com o id do usuário
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)
               _ -> redirect ErroR --Em caso de erro, redirect para ErroR


--Renderiza o form para cadastro do Usuário
formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

--Abaixo, criamos o Form com uma Tupla de dois Text, pois queremos acessar apenas os campos Login e Senha de Users,
--Mas NÃO queremos o campo Nome (Senão bastaria usar o formUser acima) 
formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout [whamlet|
          <p><b> Pagina de #{usersNome user}
          <p><b> Login: #{usersLogin user}
      |]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
     <h1> Bem-vindo meu Rei!
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]


getQuemR :: Handler Html
getQuemR = do
     mu <- lookupSession "_ID"
     case mu of
        --Se em 'mu' houver sessão:
        Just sess -> do
            --Na Session é guardado um Text, mas só é possível converter de Text para String (unpack), e de String para Int (read)
            --O 'toSqlKey' converte de Int para Key(do BD)

            --(toSqlKey $ read $ unpack sess) <--- Transforma a Session de Text pra String, de String pra Inteiro e de Int pra chave
            uid <- return (toSqlKey $ read $ unpack sess) :: Handler (Key Users)
            user <- runDB $ get404 uid
            defaultLayout [whamlet|
               <h1> Quem sou? #{usersNome user}
            |]
        --Se não houver (é Nothing):
        Nothing -> redirect ErroR

connStr = "dbname=d7eaanmqf3fql5 host=ec2-23-21-165-183.compute-1.amazonaws.com user=qpgypdhkhkmvdx password=ABwQOGG99Ol98AixMF8l7Ta8Dn port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
