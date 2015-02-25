#storage.py
import Queue
import copy
import threading
import time

class Storage(object):
  def __init__( self, obj ):
    self.imm = True
    self.obj = obj
    self.shadow = obj
    self.queue = Queue.Queue()

  def get( self ):
    if self.imm:
      return self.obj
    else:
      r = self.shadow
      if self.imm:
        r = self.obj
        print "WRRRRRRRRRRUUNGGG!"
      return r

  def mod( self, fn ):
    if not self.imm:
      self.queue.put_nowait( fn )
      return
    self.imm = False
    self.obj = fn( self.obj )
    while self.queue.qsize() > 0:
      self.obj = self.queue.get()( self.obj )
      self.queue.task_done()
    self.imm = True
    self.shadow = copy.copy( self.obj )
    if not getattr( self, "save", None ) is None:
      self.save()
