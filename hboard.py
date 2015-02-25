#!/usr/bin/python
from flask import Flask, g, url_for, render_template, request
from flask import abort, redirect, after_this_request
import board
from counter import Counter
import os
import time
import random
import string
from parsepost import parse_post

FILE_EXTS = [ "jpeg", "jpg", "png", "gif", "webp", "webm" ]
UPLOAD_FOLDER = "static/img/"

app = Flask(__name__)
app.config['MAX_CONTENT_LENGTH'] = 16777216

imageCounter = None
boards = None
boards_desc = { "r": "Random (NSFW)"
              , "g": "Gaming"
              , "a": "Anime & Manga"
              , "t": "Tech & Science"
              , "c": "Coding & Computer Science"
              , "l": "Language" }

def init_boards():
  global imageCounter, boards
  imageCounter = Counter.try_load( "boards/imgcounter" )
  boards = {}
  for board in boards_desc:
    init_board( board )

def init_board( bn ):
  print "Opening board: " + str( bn )
  boards[bn] = board.BoardStorage( bn )

def is_valid_file( fn ):
  return '.' in fn and fn.split( '.' )[-1] in FILE_EXTS

def file_extension( fn ):
  return fn.split( '.' )[-1]

def img( ifn ):
  return url_for( 'static', filename="img/" + ifn )

def get_id( ip ):
  t = int( time.time() / 43200.0 )
  random.seed( ip + str( t ) )
  ch = string.ascii_letters + "-_!" + string.digits
  r = ""
  for i in xrange( 10 ):
    r += random.choice( ch )
  return r

def render_board( b, error=None ):
  return render_template( "boardview.html"
                        , title=b
                        , img=img
                        , boards_desc=boards_desc
                        , posts=boards[b].get_posts( 10, 0 )
                        , parse_post=parse_post
                        , error=error ) 
 
def render_post( b, pid, error=None ):
  return render_template( "postview.html"
                        , title=b
                        , url=b+"/"+pid
                        , img=img
                        , post=boards[b].get_post( pid )
                        , error=error ) 

def no_cahce( req ):
  req.headers['Cache-Control'] = 'no-cache'
  return req

def upload_image( file ):
  if not file:
    return ( False, "No file supplied" )
  if not is_valid_file( file.filename ):
    return ( False, "Invalid file." )
  imageCounter.inc()
  pfile = imageCounter.id() + "." + file_extension( file.filename ) 
  file.save( os.path.join( UPLOAD_FOLDER, pfile) )
  return ( True, pfile )

@app.route("/<b>/", methods=["GET", "POST"])
def diplay_board( b ):
  if not b in boards.keys():
    abort( 404 )
  if request.method == "POST":
    suc, rr = upload_image( request.files['file'] )
    if not suc:
      return render_board( b, rr )
    boards[b].post( rr
                  , request.form["text"] or ""
                  , get_id( request.remote_addr ) )
    # This is not very safe since in theory somebody could have posted
    # a new thread between the post() and get() call, changing the top
    pid = boards[b].get().fp.ordering[0]
    return redirect( "/" + b + "/" + pid + "/" )
  after_this_request( no_cahce )
  return render_board( b )

@app.route("/<b>/<p>/", methods=["GET", "POST"])
def display_post( b, p ):
  if not b in boards.keys():
    abort( 404 )
  post = boards[b].get_post( p )
  if not post:
    abort( 404 )
  if request.method == "POST":
    suc, rr = upload_image( request.files['file'] )
    if not suc:
      rr = None
    boards[b].reply( p
                   , rr
                   , request.form["text"] or ""
                   , get_id( request.remote_addr ) )
    return redirect( "/" + b + "/" + p + "/" ) # refresh
  after_this_request( no_cahce )
  return render_post( b, p )

@app.route("/")
def frontpage():
  after_this_request( no_cahce )
  return render_template( "index.html"
                        , title="hboard"
                        , boards=boards_desc )


if __name__ == "__main__":
  init_boards()
  app.debug = True
  app.run( host="0.0.0.0", port=80 )

