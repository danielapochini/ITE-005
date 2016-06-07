{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod -- 6 a 17, fundação.

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET POST 
/link LinkR GET
--rota, tipo , metodo de acesso à rota 
|]


-- O tipo HelloWorld representa sua pagina 
-- pois é uma INSTANCIA DE YESOD

instance Yesod HelloWorld 
--Yesod é um typeclass, todo site tem que ser instancia do typeclass YESOD, a pag é uma instancia.

-- SITE COMEÇA AQUI 

-- HomeR GET 
getHomeR :: Handler Html -- handler é algo que tem que renderizar na tela, tudo q vai mostrar na pagina 
getHomeR = defaultLayout $ [whamlet|
        <h1> Ola Mundo 
        <button onclick="teste()"> OK
{-- |] >> toWidget [lucius|
    h1 {
        color:red;
    } --} 
|] >> toWidget [cassius|
    h1
    color: red;
|] >> toWidgetHead [julius|
    function teste(){
        alert("Ola mundo");
    }
|]    
-- whamlet - HTML, lucius - CSS.
-- defaultLayout mostra na tela o HTML/CSS/JavaScript, renderiza os widgets.

postHomeR :: Handler Html 
postHomeR = defaultLayout $ [whamlet|
    <h2> Ola do metodo POST
    

getLinkR :: Handler Html
getLinkR = defaultLayout $ [whamlet|
        <a href=@{HomeR}> Pagina Inicial -- ROTA, NAO USAR /
----------------------------------------
main :: IO ()
main = warp 8080 HelloWorld --warp é o servidor, executa na porta 8080 a pag.

{--
para executar a aplicacao:
stack exec -- web

preview - preview running aplication

parar aplicacao:
ir no terminal e digitar ctrl + c
--}

{--
ver rota post
abrir outra bash
curl -v -x POST 

--}


{--

cd web/
stack setup
stack build
se der erro digitar stack build dnv
--}

