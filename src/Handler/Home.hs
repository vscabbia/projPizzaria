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

        <h1>
            SISTEMA DE PRODUTO
        
        <img src=@{StaticR img_produto_jpg}>
        
        <ul>
            <li> 
                <a href=@{ProdutoR}>
                    CADASTRO
    |]
