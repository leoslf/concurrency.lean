import Std.Data.HashMap
import Std.Data.HashSet

import Concurrency

open Concurrency

instance : MonadLift BaseIO BaseIO where
  monadLift := id

instance {a : Type u} [Hashable a] [BEq a] : HSub (Std.HashSet a) (Std.HashSet a) (Std.HashSet a) where
  hSub a b := b.fold Std.HashSet.erase a

def test_MVar : IO Unit := do
  let mVar : MVar Nat <- MVar.empty

  let mut value <- mVar.read?
  assert! value == .none

  mVar.put 1

  value <- mVar.read?
  assert! value == .some 1

  value <- mVar.take
  assert! value == .some 1

  value <- mVar.read?
  assert! value == .none

def test_Semaphore : IO Unit := do
  -- make sure that there's at most `concurrency` workers running around in the critical section
  let aliveWorkers : Std.Mutex (Std.HashSet Nat) <- Std.Mutex.new {}

  let online (id : Nat) : BaseIO Unit := aliveWorkers.atomically $ modify (·.insert id)
  let offline (id : Nat) : BaseIO Unit := aliveWorkers.atomically $ modify (·.erase id)

  let mut workers : Std.HashMap Nat (Task Unit) := {}

  let concurrency : Nat := 4
  let sem <- Semaphore.new concurrency

  let worker (id : Nat) : BaseIO Unit := sem.bracket $ do
    online id
    try
      let mut iterations := 0
      while not (<- IO.checkCanceled) do
        -- pretend that we're busy
        iterations := iterations + 1
    finally
      offline id

  -- spin up the maximum concurrency
  for id in [0:concurrency] do
    workers := workers.insert id (<- BaseIO.asTask $ worker id)

  let mut ids <- aliveWorkers.atomically get
  assert! ids - Std.HashSet.ofList (List.range' 0 concurrency) |>.isEmpty

  let n := 2
  assert! n < concurrency
  -- create extra workers that cannot step into the critical section yet
  for id in [concurrency:concurrency + n] do
    workers := workers.insert id (<- BaseIO.asTask $ worker id)

  -- make sure we don't have the newly created workers (more than concurrency currently allowed) running around in the critical section
  ids <- aliveWorkers.atomically do get
  assert! ids - Std.HashSet.ofList (List.range' 0 concurrency) |>.isEmpty

  -- signal cancel to the first n workers
  for id in [0, n] do
    IO.cancel workers[id]!

  -- now we should have the newly created workers joining the pool
  ids <- aliveWorkers.atomically do get
  assert! ids - Std.HashSet.ofList (List.range' n (concurrency + n)) |>.isEmpty

  -- teardown
  for id in ids do
    IO.cancel workers[id]!

def main : IO Unit := do
  test_MVar
  test_Semaphore
