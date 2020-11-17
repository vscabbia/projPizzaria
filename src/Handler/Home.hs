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
  addStylesheet (StaticR css_bootstrap_css)
  [whamlet|

        <h1 class="ml-3">
            Pizzaria
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

                                <a class="btn btn-light mx-2" href=@{UnidadeR}>
                                    Cadastrar unidades
                            
                                <a class="btn btn-light mx-2" href=@{ListUnidadeR}>
                                    Ver unidades
                            
                        


    |]

getCardapioR :: Handler Html
getCardapioR = do
  sabors <- runDB $ selectList [] [Desc SaborPreco]
  defaultLayout $ do
    addStylesheet (StaticR css_bootstrap_css)
    [whamlet|
            <h1>Cardápio
            
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