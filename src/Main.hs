{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Applicative
import           Snap
import           Snap.Snaplet.AcidState
import           Snap.Util.FileServe
import           Control.Lens
import           Control.Lens.TH
import           BoardApp

data HboardApp = HboardApp { _board :: Snaplet BoardApp }
makeLenses ''HboardApp

type HHandler = Handler HboardApp HboardApp

uncachedHandler :: HHandler a -> HHandler a
uncachedHandler h = do
  modifyResponse $ setHeader "Cache-Control" "no-cache"
  h
  finishWith =<< getResponse

serveNotFound :: HHandler ()
serveNotFound = do
  sendFile "res/404.html"
  modifyResponse $ setResponseCode 404
  finishWith =<< getResponse

tryTop :: HHandler ()
tryTop = ifTop $ uncachedHandler $ serveFile "res/index.html"

hboardAppInit :: SnapletInit HboardApp HboardApp
hboardAppInit = makeSnaplet "hboard" "An image-board server!" Nothing $ do
  b <- nestSnaplet "board" board $ boardInit
  addRoutes [ ("/res/", serveDirectory "res")
            , ("post") ]
  wrapSite (\site -> tryTop <|> site <|> serveNotFound)
  return $ HboardApp b

main :: IO ()
main = do
  serveSnaplet defaultConfig hboardAppInit

postThreadHandler :: Snap ()
postThreadHandler = do
  ptext <- getPostParam "text"
  maybe (writeBS "Missing text field value!")
        writeBS ptext
