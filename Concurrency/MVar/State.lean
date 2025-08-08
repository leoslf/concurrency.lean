namespace Concurrency.MVar

inductive State (a : Type) where
  | empty : State a
  | full : a -> State a
deriving Repr, DecidableEq, BEq

namespace State

def isEmpty : State a -> Bool
  | .empty => true
  | .full _ => false

@[simp, grind] theorem isEmpty_empty : @isEmpty a empty = true := rfl
@[simp, grind] theorem isEmpty_full : isEmpty (full a) = false := rfl

def isFull : State a -> Bool
  | .empty => false
  | .full _ => true

@[simp, grind] theorem isFull_empty : @isFull a empty = false := rfl
@[simp, grind] theorem isFull_full : isFull (full a) = true := rfl

def get? : (self : State a) -> Option a
  | .empty => .none
  | .full value => .some value

def get : (self : State a) -> isFull self -> a
  | .full x, _ => x
  | .empty, h => nomatch h

end State
