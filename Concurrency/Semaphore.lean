import Std.Sync.Mutex

namespace Concurrency

abbrev Semaphore.Count := Nat

structure Semaphore where
  private mk ::
  condition : Std.Condvar
  count : Std.Mutex Nat

namespace Semaphore

def new (n : Nat := 0) : BaseIO Semaphore :=
  Semaphore.mk <$> Std.Condvar.new <*> Std.Mutex.new n

def isAvailable [Monad m] [MonadState Nat m] : m Bool := do
  let count <- get
  return count > 0

def isUnavailable [Monad m] [MonadState Nat m] : m Bool :=
  not <$> isAvailable

def wait (self : Semaphore) : BaseIO Unit := do
  self.count.atomicallyOnce self.condition isAvailable $ do
    modify Nat.pred

def signal (self : Semaphore) : BaseIO Unit := do
  self.count.atomically $ do
    modify Nat.succ
    self.condition.notifyOne

def bracket (self : Semaphore) (action : IO a) : IO a := do
  try
    self.wait
    action
  finally
    self.signal

end Semaphore
