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
--import           Control.Lens.TH
import           Data.UID (newUIDString)
import           System.Directory (renameFile)
import           Data.List.Split (splitOn)
import qualified Data.ByteString.Char8      as BS
import           Snap.Snaplet.AcidState
import           BoardApp
import           Data.Aeson (encode)
import           Numeric

data HboardApp = HboardApp { _board :: Snaplet BoardApp }
makeLenses ''HboardApp

type HHandler = Handler HboardApp HboardApp

instance HasAcid HboardApp Board where
  getAcidStore x = getAcidStore (x ^. board . snapletValue)

-- Helper functions
-- Because caching a dynamic site just doesn't work well
uncachedHandler :: HHandler a -> HHandler a
uncachedHandler h = do
  modifyResponse $ setHeader "Cache-Control" "no-cache"
  h
  finishWith =<< getResponse

withResponseCode :: Int -> HHandler () -> HHandler ()
withResponseCode c e = do
  e -- verybody do the flop.
  modifyResponse $ setResponseCode c
  finishWith =<< getResponse

withContentType :: BS.ByteString -> HHandler () -> HHandler()
withContentType c e = do
  e
  modifyResponse $ setContentType c
  finishWith =<< getResponse

-- The page you're looking for is in another castle.
serveNotFound :: HHandler ()
serveNotFound = withResponseCode 404 $ sendFile "res/404.html"

tryTop :: HHandler ()
tryTop = ifTop $ uncachedHandler $ serveFile "res/index.html"

-- Content/functionality functions

-- Some people just want to watch the world burn.
routes :: [ (BS.ByteString, HHandler ()) ]
routes = [ ("res/"  , serveDirectory "res")
         , ("post"   , method POST performPost)
         , ("getposts/:pidx/:pcnt", withContentType "text/json" fetchPosts)
         , ("getpost/:pid", withContentType "text/json" fetchPost)
         , ("dragons", dragons) ]

-- Shrek plz.
dragons :: HHandler ()
dragons = writeBS "Here be dragons."

hboardAppInit :: SnapletInit HboardApp HboardApp
hboardAppInit = makeSnaplet "hboard" "An image-board server!" Nothing $ do
  b <- nestSnaplet "board" board $ boardInit
  addRoutes routes
  wrapSite (\site -> tryTop <|> site <|> serveNotFound)
  return $ HboardApp b

fetchPost :: HHandler ()
fetchPost = do
  getParam "pid"
           >>= retPost
           >>= maybe (withResponseCode 404 $ writeBS "{}")
                     (writeLBS . encode)
  where
    retPost Nothing  = return Nothing
    retPost (Just x) = do
      n <- query (GetPost $ BS.unpack x)
      return n

readDecMaybe :: BS.ByteString -> Maybe Int
readDecMaybe s = case readDec $ BS.unpack s of
  [ (i, "") ] -> Just i
  _           -> Nothing

fetchPosts :: HHandler ()
fetchPosts = do
  pidx <- getParam "pidx"
  pcnt <- getParam "pcnt"
  case (pidx >>= readDecMaybe, pcnt >>= readDecMaybe) of
    (Just i, Just c) -> do
      psts <- query (GetPosts i c)
      writeLBS $ encode psts
    _                -> withResponseCode 404 $ writeBS "{}"

handleImageUpload :: MonadIO m =>
                     [(PartInfo, Either PolicyViolationException FilePath)]
                     -> m (Either BS.ByteString FilePath)
handleImageUpload [ (pain, Right fp) ] = do
  nfn <- newImage
  liftIO $ renameFile fp nfn
  return $ Right nfn

handleImageUpload [ (_, Left polex) ] = return $ Left $ BS.pack $ show polex

handleImageUpload [] = return $ Left "Invalid form."

handleImageUpload (_:_) = return $ Left "Multiple files submitted!"

fileExtension :: FilePath -> FilePath
fileExtension fp = (last $ splitOn "." fp)

newImage :: MonadIO m => m FilePath
newImage = do
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
    validate (Left err) _ = writeBS err
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

