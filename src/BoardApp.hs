{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BoardApp ( BoardApp
                , Board      (..)
                , ThreadPost (..)
                , ThreadReply(..)
                , NextEntryId(..)
                , PostThread (..)
                , ReplyTo    (..)
                , PreviewPost(..)
                , DeletePost (..)
                , PostOfReply(..)
                , GetPost    (..)
                , GetPosts   (..)
                , boardInit )
  where

import           Snap
import           Control.Lens.TH
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Data.Typeable
import qualified Data.List as List
import           Data.ByteString.Char8 hiding (take, reverse, elem, drop)
import           Data.SafeCopy
import           Snap.Snaplet.AcidState
import qualified Data.Map.Strict as Map
import           Data.Aeson
import           Data.Text.Encoding

type ImageRef = String
type PostID = String

-- A reply to a thread, might include an image, but always includes text
data ThreadReply = ThreadReply{ _replyId :: PostID
                              , _replyImage :: (Maybe ImageRef)
                              , _replyText :: ByteString } deriving (Typeable)

instance ToJSON ThreadReply where
  toJSON (ThreadReply _id _img _text) = object [ "id"   .= _id
                                               , "img"  .= _img
                                               , "text" .= decodeUtf8 _text ]

makeLenses ''ThreadReply

-- A thread posted on the image board, always has an image and text
-- All the replies to the thread is contained inside the thread itself.
data ThreadPost = ThreadPost{ _postId :: PostID
                            , _postImage :: ImageRef
                            , _postText :: ByteString
                            , _postReplies :: [ThreadReply] } deriving (Typeable)

instance ToJSON ThreadPost where
  toJSON (ThreadPost _id _img _text _replies ) = object [ "id"      .= _id
                                                        , "img"     .= _img
                                                        , "text"    .= decodeUtf8 _text
                                                        , "replies" .= _replies ]

makeLenses ''ThreadPost

-- The whole image board, with a post-count for generating post ids, a list 
-- of all the threads in relavance order, and a PostID to ThreadPost map of all the threads
data Board = Board{ _nextId :: Int
                  , _topPosts :: [PostID]
                  , _posts :: (Map.Map PostID ThreadPost) } deriving (Typeable)
makeLenses ''Board

-- Derive safe copy for all database structures
deriveSafeCopy 0 'base ''ThreadReply
deriveSafeCopy 0 'base ''ThreadPost
deriveSafeCopy 0 'base ''Board

-- Database structure queries and updates:
-- Actions:
-- Post thread        +
-- Reply to thread    +
-- Delete thread      
-- Delete reply       
-- Preview thread     +
-- Get thread         +
-- Get threads        +

incBoard :: Board -> Board
incBoard = over nextId (+ 1)

-- Moves the PostID to the top of the list
bump :: PostID -> [PostID] -> [PostID]
bump pid ps = pid : (List.delete pid ps)

nextEntryId :: Query Board Int
nextEntryId = do
  b <- ask
  return $ b ^. nextId

postThread :: ThreadPost -> Update Board ()
postThread post = do
  b <- get
  put $ over posts (Map.insert (post ^. postId) post)   -- Add the post to the board
      $ over topPosts ((:) (post ^. postId)) -- Add the post the the top posts
      $ incBoard b                         -- Increment the post id counter

replyTo :: ThreadReply -> PostID -> Update Board ()
replyTo rep pstid = do
  b <- get
  case Map.lookup pstid (b ^. posts) of -- If the post exists:
    Just _  -> put $ over topPosts (bump pstid) -- Bump it
                    -- And add the reply to the post
                   $ over posts (Map.adjust (over postReplies
                          (flip (++) [rep])) pstid)
                   $ incBoard b         -- Increment the post id counter
    Nothing -> put b -- Else do nothing

previewPost :: PostID -> Query Board (Maybe ThreadPost)
previewPost pid = do
  b <- ask
  -- Return the post with only the 3 latest replies if it exists
  return $ Map.lookup pid (b ^. posts) >>=
         (\x -> return $ over postReplies ((take 3) . reverse) x ) 

-- Deletes a thread from the board
deletePost :: PostID -> Update Board ()
deletePost pst = do
  b <- get
  put $ over posts (Map.delete pst) b 

postContainsReply :: PostID -> ThreadPost -> Bool
postContainsReply pid pst = elem pid $ List.map (\x -> x ^. replyId) (pst ^. postReplies)

postOfReply :: PostID -> Query Board (Maybe PostID)
postOfReply pid =
  let search :: PostID -> [(PostID, ThreadPost)] -> Maybe PostID
      search a ((k, x):xs)
             | postContainsReply a x = Just k
             | otherwise             = search a xs
      search _ [] = Nothing
  in do
    b <- ask
    return $ search pid $ Map.toList (b ^. posts)

getPost :: PostID -> Query Board (Maybe ThreadPost)
getPost pst = do
  b <- ask
  return $ Map.lookup pst (b ^. posts)

getPosts :: Int -> Int -> Query Board [PostID]
getPosts idx cnt = do
  b <- ask
  return $ take cnt $ drop idx $ b ^. topPosts

-- Make the database structures acidic
makeAcidic ''Board [ 'nextEntryId
                   , 'postThread
                   , 'replyTo
                   , 'previewPost
                   , 'deletePost
                   , 'postOfReply
                   , 'getPost
                   , 'getPosts ]


data BoardApp = BoardApp { _acid :: Snaplet (Acid Board) }
makeLenses ''BoardApp

instance HasAcid BoardApp Board where
  getAcidStore = view (acid.snapletValue)

boardInit :: SnapletInit b BoardApp
boardInit = makeSnaplet "boardapp" "A image board database-thing" Nothing $ do
  a <- nestSnaplet "acid" acid $ acidInit (Board 0 [] Map.empty)
  return $ BoardApp a

