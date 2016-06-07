{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
   / CadastroR GET POST
   /listar ListarR GET
   /listardpto ListarDeptoR GET
   /pessoa/#PessoaId PessoaR GET POST
   /depto DeptoR GET POST
   /hello HelloR GET
|]

--IMPORT : ROTAS
-- FOUNDATION: BD, INSTANCIA DE YESOD
-- HANDLERS: CODIGOS DA APLICACAO