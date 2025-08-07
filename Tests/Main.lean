import Concurrency

open Concurrency

def test_MVar : IO Unit := do
  let mVar : MVar Nat <- MVar.empty

  IO.println s!"mVar: {<- mVar.read?}"

  mVar.put 1

  IO.println s!"mVar: {<- mVar.read?}"

  let value <- mVar.take
  IO.println s!"received value: {value}"

  IO.println s!"mVar: {<- mVar.read?}"

  IO.println s!"Hello, World!"

def test_Semaphore : IO Unit := do
  let ref : IO.Ref (List Char) <- IO.mkRef []
  let sem <- Semaphore.new 1

  -- let task_a := IO.asTask $ sem.bracket $ do
  --   -- IO.checkCanceled
  --   ref.modify λxs => xs.concat 'a'

  -- let task_b := IO.asTask $ sem.bracket $ do
  --   -- IO.checkCanceled
  --   ref.modify λxs => xs.concat 'b'

def main : IO Unit := do
  test_MVar
  test_Semaphore
