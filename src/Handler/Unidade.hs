{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Unidade where

import Import
formUnidade :: Maybe Unidade -> Form Unidade
formUnidade unidade = renderDivs $ Unidade  
    <$> areq textField "Nome: " (fmap unidadeNome unidade)
    <*> areq textField "Endereço: " (fmap unidadeEndereco unidade)


auxUnidadeR :: Route App -> Maybe Unidade -> Handler Html
auxUnidadeR rt unidade = do
    (widget,_) <- generateFormPost (formUnidade unidade)
    defaultLayout $ do
        setTitle "Cadastro de Unidade"
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <a href=@{HomeR}>
                Página Inicial

            <h1>
                 Cadastro De Unidade
            
            <form action=@{rt} method=post style="margin: 10px 0px 0px 5px">
                ^{widget}
                <button type="submit" class="btn btn-primary" style="margin-top: 10px">Cadastrar
        |]

getUnidadeR :: Handler Html
getUnidadeR = auxUnidadeR UnidadeR Nothing
    
postUnidadeR :: Handler Html
postUnidadeR = do
    ((resp,_),_) <- runFormPost (formUnidade Nothing)
    case resp of 
         FormSuccess unidade -> do 
             _ <- runDB $ insert unidade
             redirect ListUnidadeR
         _ -> redirect HomeR

getListUnidadeR :: Handler Html
getListUnidadeR = do 
    -- unidades :: [Entity Unidade]
    unidades <- runDB $ selectList [] [Desc UnidadeNome]
    defaultLayout $ do
        setTitle "Lista de Unidades"
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
                <a href=@{HomeR}>
                    Página Inicial
                
                <table class=" table table-hover table-striped ">
                    <thead>
                        <tr>
                            <th> 
                                Nome da Unidade
                            
                            <th>
                                Endereço
                            
                            <th>
                            
                            <th>
                    <tbody>
                        $forall Entity uid unidade <- unidades
                            <tr>
                                <td>
                                    #{unidadeNome unidade}
                                
                                <td>
                                    #{unidadeEndereco unidade}
                                
                                <th>
                                    <a href=@{UpdUnidadeR uid}>
                                        Editar
                                <th>
                                    <form action=@{DelUnidadeR uid} method=post>
                                        <button type="submit" class="btn btn-primary">×
        |]

getUpdUnidadeR :: UnidadeId -> Handler Html
getUpdUnidadeR pid = do 
    antigo <- runDB $ get404 pid
    auxUnidadeR (UpdUnidadeR pid) (Just antigo)    
    
-- UPDATE unidade WHERE id = pid SET ...
postUpdUnidadeR :: UnidadeId -> Handler Html
postUpdUnidadeR pid = do
    ((resp,_),_) <- runFormPost (formUnidade Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace pid novo
            redirect ListUnidadeR 
         _ -> redirect HomeR

postDelUnidadeR :: UnidadeId -> Handler Html
postDelUnidadeR pid = do 
    _ <- runDB $ get404 pid 
    runDB $ delete pid 
    redirect ListUnidadeR



