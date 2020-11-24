{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Import
   

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Página Inicial"
  addStylesheet (StaticR css_bootstrap_css)
  [whamlet|

        <h1 class="ml-3">
            Mega Pizza: Sistema Interno
        <div class="container-fluid" style="background-image: url(@{StaticR img_sabor_jpg});height:33em;background-size:cover">
            
            
                <div class="container">
                        <div class="row pt-3">
                            <div class="d-flex justify-content-center">

                                <a class="mx-2" class="btn btn-light" href=@{SaborR}>
                                    Cadastrar sabor
                            
                                <a class="btn btn-light mx-2" href=@{CardapioR}>
                                    Ver cardápio
                            
                                <a class="btn btn-light mx-2" href=@{CrPedidoR}>
                                    Fazer pedido

                                <a class="btn btn-light mx-2" href=@{PedidoR}>
                                    Ver pedidos

                                <a class="btn btn-light mx-2" href=@{UnidadeR}>
                                    Cadastrar unidades
                            
                                <a class="btn btn-light mx-2" href=@{ListUnidadeR}>
                                    Ver unidades
                            
                        


    |]

getCardapioR :: Handler Html
getCardapioR = do
  sabors <- runDB $ selectList [] [Desc SaborPreco]
  defaultLayout $ do
    setTitle "Cardápio"
    addStylesheet (StaticR css_bootstrap_css)
    [whamlet|
            <a href=@{HomeR} class="btn btn-outline-primary btn-sm" style="margin: 5px 5px">
                Voltar
                
            <h1 style="margin: 5px 0px 0px 5px">Cardápio
            
            <table class=" table table-striped ">
                    <thead>
                        <tr>
                            <th> 
                                Sabor
                            
                            <th>
                                Preço
                            
                            <th>
                                Descrição
                            <th>
                    <tbody>
                    $forall Entity _ sabor <- sabors   
                        <tr>
                                <td>
                                    #{saborNome sabor}
                                
                                <td>
                                    #{saborPreco sabor}
                                
                                <th>
                                    #{saborDescricao sabor}
                                        
        |]
