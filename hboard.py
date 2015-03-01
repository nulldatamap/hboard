#!/usr/bin/python
from flask import Flask, g, url_for, render_template, request
from flask import abort, redirect, after_this_request, send_from_directory
import os
import time
import random
import string
import json
from parsepost import parse_post
import redis

FILE_EXTS = [ "jpeg", "jpg", "png", "gif", "webp", "webm" ]
UPLOAD_FOLDER = "static/img/"

app = Flask(__name__)
app.config['MAX_CONTENT_LENGTH'] = 16777216

db = redis.StrictRedis( host='localhost', port=6379, db=0 )

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

def upload_image( file, b ):
  global db

  if not file:
    return ( False, "No file supplied" )

  if not is_valid_file( file.filename ):
    return ( False, "Invalid file." )

  image_id = db.incr( "imagecounter" )

  pfile = hex( image_id )[2:] + "." + file_extension( file.filename ) 

  db.lpush( "gallery", pfile )
  db.lpush( "gallery:" + b, pfile )

  file.save( os.path.join( UPLOAD_FOLDER, pfile) )

  return ( True, pfile )

@app.route("/api/boards/", methods=["GET"])
@crossdomain(origin='*')
def boards_api():
  global db
  return json.dumps( db.hgetall( "boards" ) )

@app.route("/api/boards/<b>/", methods=["GET", "POST"])
@crossdomain(origin='*')
def board_api( b ):
  global db

  boardkey = "board:" + b

  if not db.hexists( "boards", b ):
    abort( 404 )
  
  if request.method == "POST":
    suc, val = upload_image( request.files.get( "file" ), b )
    if suc:
      if request.form.has_key( "text" ):
        post_id = db.incr( boardkey + ":counter" )
        db.hmset( boardkey + ":" + str( post_id )
                , { "poster_id" : get_id( request.remote_addr )
                  , "text" : request.form["text"]
                  , "image" : val
                  , "is_post" : 1 } )
        top_score = db.incr( boardkey + ":top_score" )
        db.zadd( boardkey + ":posts", top_score, post_id )
        return json.dumps( { "post_id" : post_id } )
      else:
        val = "No post text supplied"
    
    return json.dumps( { "error": val } ), 306

  try:
    start = int( request.args.get( "start", 0 ) )
    end = int( request.args.get( "end", -1 ) )
  except:
    return json.dumps( { "error": "Query parameters must be integers" } ), 306

  posts = db.zrange( boardkey + ":posts", start, end )
  
  return json.dumps( posts )

@app.route("/api/boards/<b>/<p>/", methods=["GET", "POST"])
@crossdomain(origin='*')
def post_api( b, p ):
  global db
  
  boardkey = "board:" + b
  postkey = boardkey + ":" + p
  
  if not db.exists( postkey ):
    abort( 404 )

  # Reply to a post
  if request.method == "POST":
    suc, val = (True, None)
    
    if int( db.hget( postkey, "post_id" ) ) == 0:
      return json.dumps( { "error": "Can only reply to posts." } ), 306

    if request.files.has_key( "file" ):
      suc, val = upload_image( request.files["file"], b )

    if not request.form.has_key( "text" ):
      suc, val = (False, "No reply text supplied")
    
    if suc:
      reply_id = db.incr( boardkey + ":counter" )
      db.hmset( boardkey + ":" + str( reply_id )
              , { "poster_id" : get_id( request.remote_addr )
                , "text" : request.form["text"]
                , "image" : val
                , "is_post" : 0 } )
      db.lpush( postkey + ":replies" , reply_id )
      top_score = db.incr( boardkey + ":top_score" )
      db.zadd( boardkey + ":posts", top_score, p )
      return json.dumps( { "reply_id" : reply_id } )
    else:
      return json.dumps( { "error" : val } ), 306

  # View a post
  post = { "poster_id" : db.hget( postkey, "poster_id" )
         , "text" : db.hget( postkey, "text" )
         , "image" : db.hget( postkey, "image" ) }
  
  # Only posts have replies
  if int( db.hget( postkey, "is_post" ) ) == 1:
    post["replies"] = []

    try:
      start = int( request.args.get( "start", 0 ) )
      end = int( request.args.get( "end", -1 ) )
    except:
      return json.dumps( { "error": "Query parameters must be integers" } ), 306

    # Replies are stored as references ( by id )
    reply_ids = db.lrange( postkey + ":replies" , start, end )
    
    # But when calling we want all the replies in-structure and in order
    for reply_id in reply_ids:
      reply = db.hgetall( "board:" + b + ":" + reply_id )
      del reply["is_post"]
      # But we still want to know the id of each post
      reply["id"] = reply_id
      
      post["replies"].append( reply )
  
  return json.dumps( post )

@app.route("/api/gallery/<b>/")
@crossdomain(origin='*')
def api_gallery( b ):
  global db
  try:
    start = int( request.args.get( "start", 0 ) )
    end = int( request.args.get( "end", -1 ) )
  except:
    return json.dumps( { "error": "Query parameters must be integers" } ), 306

  images = db.lrange( "gallery:" + b, start, end )
  return json.dumps( images )

@app.route("/api/gallery/")
@crossdomain(origin='*')
def api_board_gallery():
  global db
  try:
    start = int( request.args.get( "start", 0 ) )
    end = int( request.args.get( "end", -1 ) )
  except:
    return json.dumps( { "error": "Query parameters must be integers" } ), 306

  images = db.lrange( "gallery", start, end )
  
  return json.dumps( images )

@app.route( "/<b>/" )
def board_view( b ):
  if not db.hexists( "boards", b ):
    abort( 404 )
  return send_from_directory( "static", "boardview.html" )

@app.route( "/<b>/<p>" )
def post_view( b, p ):
  if not db.exists( "board:" + b + ":" + p ):
    abort( 404 )
  return send_from_directory( "static", "postview.html" )

@app.route( "/" )
def index():
  return send_from_directory( "static", "index.html" )

if __name__ == "__main__":
  app.debug = True
  app.run( host="0.0.0.0", port=80 )

