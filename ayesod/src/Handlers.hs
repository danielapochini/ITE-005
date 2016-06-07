{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2", --padrao yesod, de cima por ex é hident1
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing, -- Just "string"
                           fsAttrs=[("maxlength","2")]} Nothing --uma lista com o atributo
                           

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing --Combobox

dptos = do
        -- faz um selectlist ascendente na tabela departamento nome
        -- select * from departamento order by nome 
        -- entidades = 
        -- [Entity (Key 1) (Departamento "Sistemas p Internet" "SI"),
        -- [Entity (Key 2) (Departamento "Analise e Desenv." "AD"), 
        -- [Entity (Key 3) (Departamento "Gestao Portuaria" "GP")]
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades
       -- fmap vai jogar a função f ent = (departamentoSigla $ entityVal ent, entityKey ent)
       -- 1º ent = Entity (Key 1) (Departamento "Sistemas p Internet" "SI")
       -- f ent = ("SI", 1)
       -- 2º ent = Entity (Key 2) (Departamento "Analise e Desenv." "AD")
       -- f ent ("AD", 2)
       -- 3º ent = Entity (Key 3) (Departamento "Gestao Portuaria" "GP")
       -- f ent ("GP", 3)
       -- coloca dentro da monad certa com optionsPairs = [("SI", 1),("AD", 2),("GP", 3)]
       -- toda vez q clicar no combobox de SI, vai dar o id 1, e assim por diante...


-- usando a internacionalizacao
getHelloR :: Handler Html
getHelloR = defaultLayout [whamlet|
     <h1> _{MsgHello}
|]

-- Função para gerar formularios de uma maneira genérica
widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
-- x rota, enctype 
-- #{y} texto 
widgetForm x enctype widget y = [whamlet|
            <h1>
                Cadastro de #{y}
            <form method=post action=@{x} enctype=#{enctype}> 
                ^{widget}
                <input type="submit" value="Cadastrar">
|]

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ widgetForm CadastroR enctype widget "Pessoas" 
-- cadastroR @{x}, Texto em #{y}

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid 
             dpto <- runDB $ get404 (pessoaDeptoid pessoa)
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pessoaNome pessoa}
                 <p> Salario: #{pessoaSalario pessoa}
                 <p> Idade: #{pessoaIdade pessoa}
                 <p> Departamento: #{departamentoNome dpto}
             |]


getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout $ [whamlet|
                 <h1> Pessoas cadastradas:
                 $forall Entity pid pessoa <- listaP
                     <a href=@{PessoaR pid}> #{pessoaNome pessoa} 
                     <form method=post action=@{PessoaR pid}> 
                         <input type="submit" value="Deletar"><br>
             |] >> toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]

{--
 listaP <- runDB $ selectList [] [Asc PessoaNome]
listaP =
   [Entity (Key 1) (Pessoa "Vanessa" 31 900),
   [Entity (Key 2) (Pessoa "Eu" 25 800), 
   [Entity (Key 3) (Pessoa "André" 38 1100)]
   
   pra cada um do lista, ele coloca no pattern matching ex pid = Key 1, pessoa = "Vanessa"
   -- pra cada pessoa aparecera o nome, e um link, q vai pro perfil.
--}


postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet| 
                           <h1> #{pessoaNome pessoa} Inseridx com sucesso. 
                       |]
                    _ -> redirect CadastroR


getDeptoR :: Handler Html
getDeptoR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ widgetForm DeptoR enctype widget "Departamentos"

postDeptoR :: Handler Html
postDeptoR = do
                ((result, _), _) <- runFormPost formDepto
                case result of
                    FormSuccess depto -> do
                       runDB $ insert depto
                       defaultLayout [whamlet|
                           <h1> #{departamentoNome depto} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR
                    

getListarDeptoR :: Handler Html
getListarDeptoR = do
             listaD <- runDB $ selectList [] [Asc DepartamentoNome]
             defaultLayout $ [whamlet|
                 <h1> Departamentos Cadastrados:
                 $forall Entity pid departamento <- listaD
                     <a href=@{DeptoR pid}> #{departamentoNome departamento} 
                     <form method=post action=@{DeptoR pid}> 
                         <input type="submit" value="Deletar"><br>
             |] >> toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]

 

postPessoaR :: PessoaId -> Handler Html
postPessoaR pid = do
     runDB $ delete pid
     redirect ListarR
     
    
{--
EX. 0:
Listagem de departamentos com botao delete

EX. 1: 
toda pessoa possui um chefe, faça um cadastro de chefe
associe chefe a um departamento e mostre no perfil da pessoa 
o chefe dela e crie uma pagina de perfil para chefe e departamento.

crie tambem uma listagem de chefes

--}
       
{--
INTERPOLADORES:

^ - widgets
@ - rotas
# - comando haskell
$ - estruturas de comando (ex: maybe, forall, if, etc...)
_ - internacionalizacao
--}