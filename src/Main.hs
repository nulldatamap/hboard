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
import           System.Directory
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
routes = [ ("res/", serveDirectory "res")
         , ("/", serveDirectory "res")
         , ("post", method POST performPost)
         , ("reply", method POST performReply)
         , ("getposts/:pidx/:pcnt", withContentType "text/json" fetchPosts)
         , ("getpost/:pid", withContentType "text/json" fetchPost)
         , ("getpreview/:pid", withContentType "text/json" fetchPreview)
         , ("dragons", dragons) ]

-- Shrek plz.
dragons :: HHandler ()
dragons = writeBS "Here be dragons."

hboardAppInit :: SnapletInit HboardApp HboardApp
hboardAppInit = makeSnaplet "hboard" "An image-board server!" Nothing $ do
  b <- nestSnaplet "board" board $ boardInit
  addRoutes routes
  wrapSite (\site -> tryTop <|> site <|> serveNotFound)
  tmpExists <- liftIO $ doesDirectoryExist "./tmp"
  if not tmpExists
     then liftIO $ createDirectory "./tmp"
     else return ()
  return $ HboardApp b

fetchEntry getter = do
  getParam "pid"
           >>= retPost
           >>= maybe (withResponseCode 404 $ writeBS "{}")
                     (writeLBS . encode)
  where
    retPost Nothing  = return Nothing
    retPost (Just x) = do
      n <- query (getter $ BS.unpack x)
      return n

fetchPost :: HHandler ()
fetchPost = fetchEntry GetPost

fetchPreview :: HHandler ()
fetchPreview = fetchEntry GetPreview

readDecMaybe :: BS.ByteString -> Maybe Int
readDecMaybe s = case readDec $ BS.unpack s of
  [ (i, "") ] -> Just i
  _           -> Nothing


-- Sanitization for text that goes into the body of a html tag
-- This does not sanitize for attributes
-- Esacped chars: < > &
sanitizeText :: BS.ByteString -> BS.ByteString
sanitizeText "" = ""
sanitizeText te = BS.foldl (\acc x -> BS.append acc $ maybe (BS.singleton x) id (lookup x escapedChars)) "" te
  where
    escapedChars = [ ('<', "&lt;")
                   , ('>', "&gt;")
                   , ('&', "&amp;") ];

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
  nfn <- newImage $ partContentType pain
  case nfn of
    Nothing -> return $ Left $ if partFileName pain == Just ""
                                  then "No file supplied."
                                  else "File type either not valid or not supported."
    Just x  -> do
      liftIO $ renameFile fp x
      return $ Right x

handleImageUpload [ (_, Left polex) ] = return $ Left $ BS.pack $ show polex

handleImageUpload [] = return $ Left "Invalid form."

handleImageUpload (_:_) = return $ Left "Multiple files submitted!"

fileExtension :: BS.ByteString -> Maybe FilePath
fileExtension fp = lookup fp allowedMIMEs
  where
    allowedMIMEs = [ ( "image/png", ".png" )
                   , ( "image/jpeg", ".jpeg" )
                   , ( "image/gif", ".gif" )
                   , ( "image/webp", ".webp" )
                   , ( "video/wemb", ".webm" ) ];

newImage :: MonadIO m => BS.ByteString -> m (Maybe FilePath)
newImage ct = do
  nuid <- liftIO newUIDString
  return $ fext >>= Just . (mkfn nuid)
  where
    fext = fileExtension ct
    mkfn x y = "res/img/" ++ x ++ y

validateImage :: HHandler (Either BS.ByteString FilePath)
validateImage = handleFileUploads "./tmp"
                                  defaultUploadPolicy
                                  (const $ allowWithMaximumSize 1572864)
                                  handleImageUpload

maybeImage :: HHandler (Maybe (Either BS.ByteString FilePath)) 
maybeImage = do
  img <- validateImage
  return $ if img == Left "No file supplied."
              then Nothing
              else Just img

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
      update $ PostThread fp (sanitizeText t)
      writeBS $ BS.append (BS.pack $ "Image: " ++ fp ++ "\n") t

performReply :: HHandler ()
performReply = do
  -- Required form fields:
  pimg <- maybeImage
  getPostParam "to" >>= maybe (writeBS "Invalid form.")
                              (optionals pimg)
  where
    -- Optional form fields, the text field will be substituded 
    -- with an empty string if not found.
    optionals pimg post = do
      text <- getPostParam "text" >>= maybe (return "") (return . id)
      case pimg of
        Just (Left err)  -> writeBS err
        Just (Right img) -> doReply post text (Just img)
        Nothing          -> doReply post text Nothing
    doReply post text mimg = do
      update $ ReplyTo mimg (sanitizeText text) (BS.unpack post)
      writeBS "Replied!"


main :: IO ()
main = do
  serveSnaplet defaultConfig hboardAppInit

