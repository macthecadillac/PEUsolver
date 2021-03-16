module AStar : PairingHeap.ORDERING
  with type t = float * (int * Grammar.t) list

module FAStar (E : Search.ENV) : PairingHeap.ORDERING
  with type t = float * (int * Grammar.t) list
