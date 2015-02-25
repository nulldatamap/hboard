#counter.py
from storage import Storage
import json
import os

class Counter(Storage):
  def __init__( self, val=0, loc=None ):
    self.loc = loc
    Storage.__init__( self, val )

  def id( self ):
    return hex( self.get() )[2:] # remove the '0x'

  def inc( self ):
    self.mod( lambda x: x + 1 )

  def save( self ):
    if self.loc is None:
      print "Counter can't be saved."
      return
    with open( self.loc, 'w' ) as f:
      f.write( str( self.obj ) )

  @staticmethod
  def try_load( loc ):
    if not os.path.exists( loc ):
      return Counter( 0, loc )
    else:
      return Counter.load( loc )

  @staticmethod
  def load( loc ):
    with open( loc, 'r' ) as f:
      return Counter( int( f.read() ), loc )
