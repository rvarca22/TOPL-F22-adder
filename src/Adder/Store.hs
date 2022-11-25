{--
 -  Adder is a small but usable subset of the Python language. It is named
 -  for the Blackadder comedy series, much as the Python language is named
 -  for Monty Python.
 -
 -  This module provides the "world's dumbest model of the store", as
 -  described by Mitchell and Wand: "the store is a list and a reference is
 -  a number which denotes a position in the list."
 -}
module Adder.Store
  ( Store,
    emptyStore,
    newref,
    deref,
    setref,
  )
where

import Adder.DataStructures (StoVal)
import Adder.Defs (Reference)

-- Free store represented as a list
-- TODO Implement the rudimentary garbage collection

type Store = [StoVal]

emptyStore :: Store
emptyStore = []

newref :: StoVal -> Store -> (Reference, Store)
newref val store = (length store, store ++ [val])

deref :: Reference -> Store -> StoVal
deref ref store = store !! ref

setref :: Reference -> StoVal -> Store -> Store
setref ref val store = take ref store ++ (val : drop (ref + 1) store)
