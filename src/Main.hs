{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Applicative
import           Snap
import           Snap.Util.FileUploads
import           Snap.Util.FileServe
import           Control.Lens
import           Control.Lens.TH
import           Data.UID (newUIDString)
import           System.Directory (renameFile, doesFileExist, removeFile)
import           Data.List.Split (splitOn)
import           Snap.Internal.Debug (debug)
import           Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BS
import           Snap.Snaplet.AcidState
import           BoardApp

data HboardApp = HboardApp { _board :: Snaplet BoardApp }
makeLenses ''HboardApp

type HHandler = Handler HboardApp HboardApp

instance HasAcid HboardApp Board where
  getAcidStore x = getAcidStore (x ^. board . snapletValue)

-- Helper functions

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

-- Content/functionality functions

hboardAppInit :: SnapletInit HboardApp HboardApp
hboardAppInit = makeSnaplet "hboard" "An image-board server!" Nothing $ do
  b <- nestSnaplet "board" board $ boardInit
  addRoutes [ ("/res/", serveDirectory "res")
            , ("post", method POST performPost) ]
  wrapSite (\site -> tryTop <|> site <|> serveNotFound)
  return $ HboardApp b


handleImageUpload :: MonadIO m =>
                     [(PartInfo, Either PolicyViolationException FilePath)]
                     -> m (Either BS.ByteString FilePath)
handleImageUpload [ (pain, Right fp) ] = do
  fe <- liftIO $ doesFileExist fp
  nfn <- newImage $ fileExtension fp
  liftIO $ putStrLn $ show nfn
  liftIO $ renameFile fp nfn
  return $ Right nfn

handleImageUpload [ (_, Left polex) ] = return $ Left $ BS.pack $ show polex

handleImageUpload [] = return $ Left "Invalid form."

handleImageUpload (_:_) = return $ Left "Multiple files submitted!"

fileExtension :: FilePath -> FilePath
fileExtension fp = (last $ splitOn "." fp)

newImage :: MonadIO m => FilePath -> m FilePath
newImage fe = do
  nuid <- liftIO newUIDString
  let nfilename = mkfn nuid
  return $ nfilename
  where
    mkfn x = "res/img/" ++ x

performPost :: HHandler ()
performPost = do
  -- Insert spam preventing code here
  img <- validateImage
  (validate img) =<< getPostParam "text"
  where
    validate (Left err) x = writeBS err
    validate (Right fp) x = maybe (writeBS "No text supplied")
                                  (formulateRepsonse fp)
                                  x
    formulateRepsonse fp t = do
      nid <- query NextEntryId
      update $ PostThread (ThreadPost (show nid) fp t [])
      writeBS $ BS.append (BS.pack $ "Image: " ++ fp ++ "\n") t

    validateImage = handleFileUploads "./tmp"
                                      defaultUploadPolicy
                                      (const $ allowWithMaximumSize 1572864)
                                      handleImageUpload


main :: IO ()
main = do
  serveSnaplet defaultConfig hboardAppInit

