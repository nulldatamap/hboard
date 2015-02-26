#!/usr/bin/python
from flask import Flask, g, url_for, render_template, request
from flask import abort, redirect, after_this_request
import board
from counter import Counter
import os
import time
import random
import string
import json
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
              , "l": "Language"
              , "arks_is_a_shit": "Arks is a shit"
              , "d": "Design & Art" }

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

@app.route("/boards", methods=["GET"])
def fetch_boards():
  return json.dumps( boards_desc )

@app.route("/boards/<b>", methods=["GET"])
def fetch_board( b ):
  if not b in boards.keys():
    abort( 404 )
  return json.dumps( boards[b].get().save() )

init_boards()

if __name__ == "__main__":
  app.debug = True
  app.run( host="0.0.0.0", port=80 )

