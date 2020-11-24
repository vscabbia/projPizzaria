{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Pedido where

import Import
import Database.Persist.Sql ( fromSqlKey )

formPedidos :: [Entity Unidade] -> [Entity Sabor] -> Form Pedidos
formPedidos us ss = renderDivs $ Pedidos
    <$> areq (selectFieldList opcoesSabores) "Escolher sabor: " Nothing
    <*> areq (selectFieldList opcoesUnidades) "Escolher unidade: " Nothing
    <*> areq textField "Endereço: " Nothing
    where
        opcoesUnidades = fmap (\(Entity uid u) -> (unidadeNome u, uid)) us
        opcoesSabores = fmap (\(Entity sid s) -> (saborNome s, sid)) ss
    
postPedidoR :: Handler Html
postPedidoR = do
    unidades <- runDB $ selectList [] [Desc UnidadeId]
    sabores <- runDB $ selectList [] [Desc SaborId]
    ((resp,_),_) <- runFormPost $ formPedidos unidades sabores
    case resp of 
         FormSuccess sabor -> do 
             _ <- runDB $ insert sabor
             redirect PedidoR
         _ -> redirect HomeR


-- listar todos os pedidos
getPedidoR :: Handler Html
getPedidoR = do 
    pedidos <- runDB $ selectList [] [Desc PedidosUnidadeId]
    defaultLayout $ do
        setTitle "Lista de Pedidos"
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <a href=@{HomeR} class="btn btn-outline-primary btn-sm" style="margin: 5px 5px">
                Voltar
            
            <h1 style="margin: 5px 0px 10px 5px">Lista de Pedidos
            
            <table class=" table table-striped ">
                <thead>
                    <tr>
                        <th> 
                            Unidade
                        <th>
                            Sabor
                        <th>
                            Endereço de Entrega
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
    unidades <- runDB $ selectList [] [Desc UnidadeId]
    sabores <- runDB $ selectList [] [Desc SaborId]
    (widget,_) <- generateFormPost $ formPedidos unidades sabores
    defaultLayout $ do
        setTitle "Fazer Pedido"
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
            <a href=@{HomeR} class="btn btn-outline-primary btn-sm" style="margin: 5px 5px">
                Voltar

            <h1 style="margin: 5px 0px 0px 5px">
                 Fazer Pedido
            
            <form action=@{PedidoR} method=post style="margin: 10px 0px 0px 5px">
                ^{widget}
                <button type="submit" class="btn btn-success" style="margin-top: 10px">
                    <b>Cadastrar
        |]
