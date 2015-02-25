#board.py
import os
import json
from storage import Storage

def map_dict( f, d ):
  nd = {}
  for k in d.keys():
    nd[k] = f( d[k] )
  return nd

def dict_insert( d, k, v ):
  nd = dict( d )
  nd[k] = v
  return nd

def dict_modify( d, k, f ):
  nd = dict( d )
  nd[k] = f( nd[k] )
  return nd

class BoardReply(object):
  def __init__( self, img, txt, pid, op ):
    self.img = img
    self.txt = txt
    self.pid = pid
    self.op = op


  @staticmethod
  def load( js ):
    return BoardReply( js["img"], js["txt"], js["pid"], js["op"] )

  def save( self ):
    return { "img": self.img, "txt": self.txt, "pid": self.pid, "op": self.op }

class BoardPost(object):
  def __init__( self, img, txt, pid, op, reps ):
    self.pid = pid
    self.img = img
    self.txt = txt
    self.op = op
    self.reps = reps

  @staticmethod
  def load( js ):
    return BoardPost( js["img"]
                    , js["txt"]
                    , js["pid"]
                    , js["op"]
                    , tuple( BoardReply.load( x ) for x in js["reps"] ) )

  def save( self ):
    return { "img": self.img
           , "txt": self.txt
           , "pid": self.pid
           , "op": self.op
           , "reps": [ x.save() for x in self.reps ] }

  def reply( self, img, txt, pid, op ):
    return BoardPost( self.img
                    , self.txt
                    , self.pid
                    , self.op
                    , self.reps + (BoardReply( img, txt, pid, op ),) )

class FrontPage(object):
  def __init__( self, val=() ):
    self.ordering = val

  def new_post( self, pid ):
    return FrontPage( (pid,) + self.ordering )

  def bump( self, pid ):
    return FrontPage( (pid,) + tuple( x for x in self.ordering if x != pid ) )

class ImageBoard(object):
  def __init__( self, fp, pstn, psts ):
    self.fp = fp
    self.pstn = pstn
    self.psts = psts

  @staticmethod
  def load( loc ):
    o = {}
    with open( loc, 'r' ) as f:
      o = json.load( f )
    return ImageBoard( FrontPage( tuple( o["fp"] ) )
                     , o["pstn"]
                     , map_dict( lambda x: BoardPost.load( x ), o["psts"] ) )

  def save( self ):
    return { "fp": self.fp.ordering
           , "pstn": self.pstn
           , "psts": map_dict( lambda x: x.save(), self.psts ) }

class BoardStorage(Storage):
  def __init__( self, boardName ):
    self.boardName = boardName
    self.path = os.path.join( "boards", boardName )
    if not os.path.exists( "boards" ):
      os.mkdir( "boards" )
    if not os.path.exists( self.path ):
      Storage.__init__( self, ImageBoard( FrontPage(), 0, {} ) )
    else:
      Storage.__init__( self, ImageBoard.load( self.path ) )

  def save( self ):
    with open( self.path, 'w' ) as wf:
      json.dump( self.obj.save(), wf )

  # mod
  def post( self, img, txt, op ):
    def post_mod( obj ):
      pid  = str( obj.pstn )
      fp   = obj.fp.new_post( pid )
      pstn = obj.pstn + 1
      psts = dict_insert( obj.psts, pid, BoardPost( img, txt, pid, op, () ) )
      return ImageBoard( fp, pstn, psts )
    self.mod( post_mod )

  # mod
  def reply( self, pid, img, txt, op ):
    def reply_mod( obj ):
      if not pid in obj.fp.ordering:
        return obj
      npid = str( obj.pstn )
      fp = obj.fp.bump( pid )
      pstn = obj.pstn + 1
      psts = dict_modify( obj.psts, pid, lambda x: x.reply( img, txt, npid, op ) )
      return ImageBoard( fp, pstn, psts )
    self.mod( reply_mod )

  # mod
  def delete( self, pid ):
    pass
  
  # get
  def get_post( self, pid ):
    obj = self.get()
    if obj.psts.has_key( pid ):
      return obj.psts[pid]
    else:
      return None
  
  # get
  def get_posts( self, cnt, pge ):
    obj = self.get()
    pstart = pge * cnt
    if pstart > len( obj.fp.ordering ):
      return []
    else:
      r = []
      for pid in obj.fp.ordering[pstart:pstart+cnt]:
        if obj.psts.has_key( pid ):
          n = obj.psts[pid]
          r.append( BoardPost( n.img, n.txt, n.pid, n.op, n.reps[-3:] ) )
      return r





