import Std.Sync.Mutex

import Concurrency.MVar.State

namespace Concurrency

structure MVar (a : Type) where
  private mk ::
  taking : Std.Condvar
  putting : Std.Condvar
  state : Std.Mutex (MVar.State a)

namespace MVar

variable {a : Type} [Inhabited a]

def new (value : a) : BaseIO (MVar a) := do
  MVar.mk <$> Std.Condvar.new <*> Std.Condvar.new <*> Std.Mutex.new (.full value)

def empty : BaseIO (MVar a) := do
  MVar.mk <$> Std.Condvar.new <*> Std.Condvar.new <*> Std.Mutex.new .empty

def isEmpty [Monad m] [MonadState (State a) m] : m Bool := do
  State.isEmpty <$> get

def isFull [Monad m] [MonadState (State a) m] : m Bool := do
  State.isFull <$> get

/--
Returns the content of the MVar. If the MVar is currently empty, take will wait until it is fill.
After a take, the MVar is left empty.

Single-wakeup
LIFO order
-/
def take (self : MVar a) : BaseIO a := do
  self.state.atomicallyOnce self.taking isFull do
    let .full result <- get
      | unreachable!
    set (State.empty : State a)
    self.putting.notifyOne
    return result

/--
Put a value into the MVar.  If the MVar is currently full, put will wait until it becomes empty.

Single-wakeup
FIFO order
-/
def put (self : MVar a) (value : a) : BaseIO Unit := do
  self.state.atomicallyOnce self.putting isEmpty do
    set $ State.full value
    self.taking.notifyOne

def read (self : MVar a) : IO a := do
  let result <- self.take
  self.put result
  return result

def read? (self : MVar a) : BaseIO (Option a) := do
  self.state.atomicallyOnce self.taking (pure true) do
    let result? <- State.get? <$> get
    let _ <- self.taking.notifyOne
    return result?

def replace (self : MVar a) (value : a) : BaseIO Unit :=
  self.take *> self.put value

end MVar
