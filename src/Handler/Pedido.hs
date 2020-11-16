{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Pedido where

import Import
import Database.Persist.Sql ( fromSqlKey, toSqlKey )

formPedidos :: Form Pedidos
formPedidos = renderDivs $ Pedidos
    <$> areq (selectFieldList [("Opçao 0" :: Text, toSqlKey 0),("Opçao 1" :: Text, toSqlKey 1)]) "Which value?" Nothing
    <*> areq (selectFieldList [("Opçao 0" :: Text, toSqlKey 0),("Opçao 1" :: Text, toSqlKey 1)]) "Which value?" Nothing
    <*> areq textField "Endereço " Nothing
    
postPedidoR :: Handler Html
postPedidoR = do
    ((resp,_),_) <- runFormPost formPedidos
    case resp of 
         FormSuccess sabor -> do 
             _ <- runDB $ insert sabor
             redirect HomeR
         _ -> redirect HomeR


-- listar todos os pedidos
getPedidoR :: Handler Html
getPedidoR = do 
    pedidos <- runDB $ selectList [] [Desc PedidosUnidadeId]
    defaultLayout [whamlet|
            <table>
                <thead>
                    <tr>
                        <th> 
                            Nome
                        <th>
                            Sabor
                        <th>
                            Endereço
                <tbody>
                    $forall Entity _ pedido <- pedidos
                        <tr>
                            <td>
                                #{fromSqlKey $ pedidosUnidadeId pedido}
                            
                            <td>
                                #{fromSqlKey $ pedidosSaborId pedido}
                            <td>
                                #{pedidosEndereco pedido}
    |]

getCrPedidoR :: Handler Html
getCrPedidoR = do 
    (widget,_) <- generateFormPost formPedidos
    defaultLayout $ do
        [whamlet|
            <h1>
                 FAZER PEDIDO
            
            <form action=@{PedidoR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]  
