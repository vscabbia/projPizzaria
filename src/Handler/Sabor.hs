{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Sabor where

import Import
--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formSabor :: Maybe Sabor -> Form Sabor
formSabor prod = renderDivs $ Sabor  
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap saborNome prod)
    <*> areq doubleField "Preco: " (fmap saborPreco prod)
    <*> areq textField "Descrição: " (fmap saborDescricao prod)


auxSaborR :: Route App -> Maybe Sabor -> Handler Html
auxSaborR rt sabor = do
    (widget,_) <- generateFormPost (formSabor sabor)
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <h1>
                 CADASTRO DE SABOR
            
            <form action=@{rt} method=post>
                ^{widget}
                <button type="submit" class="btn btn-primary">Cadastrar
        |]
    
getSaborR :: Handler Html
getSaborR = auxSaborR SaborR Nothing
    
postSaborR :: Handler Html
postSaborR = do
    ((resp,_),_) <- runFormPost (formSabor Nothing)
    case resp of 
         FormSuccess sabor -> do 
             pid <- runDB $ insert sabor
             redirect (DescR pid)
         _ -> redirect HomeR

-- SELECT * from sabor where id = pid 
getDescR :: SaborId -> Handler Html
getDescR pid = do 
    sabor <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Nome: #{saborNome sabor}
        
        <h2>
            Preco: #{saborPreco sabor}
    |]

getListProdR :: Handler Html
getListProdR = do 
    -- sabors :: [Entity Sabor]
    sabors <- runDB $ selectList [] [Desc SaborPreco]
    defaultLayout [whamlet|
            <table>
                <thead>
                    <tr>
                        <th> 
                            Nome
                        
                        <th>
                            Sabor
                        
                        <th>
                        
                        <th>
                <tbody>
                    $forall Entity pid prod <- sabors
                        <tr>
                            <td>
                                #{saborNome prod}
                            
                            <td>
                                #{saborPreco prod}
                            
                            <th>
                                <a href=@{UpdProdR pid}>
                                    Editar
                            <th>
                                <form action=@{DelProdR pid} method=post>
                                    <button type="submit" class="btn btn-primary">×
    |]

getUpdProdR :: SaborId -> Handler Html
getUpdProdR pid = do 
    antigo <- runDB $ get404 pid
    auxSaborR (UpdProdR pid) (Just antigo)    
    
-- UPDATE sabor WHERE id = pid SET ...
postUpdProdR :: SaborId -> Handler Html
postUpdProdR pid = do
    ((resp,_),_) <- runFormPost (formSabor Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace pid novo
            redirect (DescR pid) 
         _ -> redirect HomeR

postDelProdR :: SaborId -> Handler Html
postDelProdR pid = do 
    _ <- runDB $ get404 pid 
    runDB $ delete pid 
    redirect ListProdR



