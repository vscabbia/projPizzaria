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
      
      <div class="container-fluid" style="background-image: url(@{StaticR img_sabor_jpg});height:40em;background-repeat: repeat-repeat;"> 
            <div class="container" style="padding-top: 20px">
                <div class="row pt-3">
                    <div class="d-flex justify-content-center">
                        <h2 style="margin-right: 15px; color: #ffffff"> Sabores:
                        <div class="btn-group" role="group" aria-label="Sabores">
                            <a class="btn btn-light btn-lg" href=@{SaborR}>
                                <b>Cadastrar Sabor
                            <a class="btn btn-light btn-lg" href=@{CardapioR}>
                                <b>Ver Cardápio
                                
                <div class="row pt-3" style="margin-top: 10px">
                    <div class="d-flex justify-content-center">
                        <h2 style="margin-right: 15px; color: #ffffff"> Pedidos:
                        <div class="btn-group" role="group" aria-label="Sabores">
                            <a class="btn btn-light btn-lg" href=@{CrPedidoR}>
                                <b>Cadastrar Pedido
                            <a class="btn btn-light btn-lg" href=@{PedidoR}>
                                <b>Ver Pedidos                 

                <div class="row pt-3" style="margin-top: 10px">
                    <div class="d-flex justify-content-center">
                        <h2 style="margin-right: 15px; color: #ffffff"> Unidades:
                        <div class="btn-group" role="group" aria-label="Sabores">
                            <a class="btn btn-light btn-lg" href=@{UnidadeR}>
                                <b>Cadastrar Unidade
                            <a class="btn btn-light btn-lg" href=@{ListUnidadeR}>
                                <b>Ver Unidades
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
