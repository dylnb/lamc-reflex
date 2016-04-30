module Lexicon where

import DbExp

lexicon :: [(String, Exp String)]
lexicon =
  [ ("false",  "y" ! "n" ! V"n")
  , ("true",   "y" ! "n" ! V"y")
  , ("if",     "b" ! "y" ! "n" ! V"b" :@ V"y" :@ V"n")
  , ("and",    "u" ! "v" ! V"u" :@ V"v" :@ V"false")
  , ("or",     "u" ! "v" ! V"u" :@ V"true" :@ V"v")
  , ("not",    "u" ! "y" ! "n" ! V"u" :@ V"n" :@ V"y")
  , ("zero",   "s" ! "z" ! V"z")
  , ("succ",   "n" ! "s" ! "z" ! V"s" :@ (V"n" :@ V"s" :@ V"z"))
  , ("pred",   "n" ! V"fst" :@ (V"n" :@ ("p" ! V"pair" :@ (V"snd" :@ V"p") :@ (V"add" :@ V"one" :@ (V"snd" :@ V"p"))) :@ (V"pair" :@ V"zero" :@ V"zero")))
  , ("one",    V"succ" :@ V"zero")
  , ("two",    V"succ" :@ V"one")
  , ("three",  V"succ" :@ V"two")
  , ("isZero", "n" ! V"n" :@ ("m" ! V"false") :@ V"true")
  , ("const",  "x" ! "y" ! V"x")
  , ("pair",   "a" ! "b" ! "k" ! V"k" :@ V"a" :@ V"b")
  , ("fst",    "ab" ! V"ab" :@ ("a" ! "b" ! V"a"))
  , ("snd",    "ab" ! V"ab" :@ ("a" ! "b" ! V"b"))
  -- we have a lambda calculus extended with recursive bindings, so we don't need to use fix
  , ("add",    "m" ! "n" ! "s" ! "z" ! V"m" :@ V"s" :@ (V"n" :@ V"s" :@ V"z"))
  , ("sub",    "m" ! "n" ! V"m" :@ V"pred" :@ V"n")
  , ("mul",    "m" ! "n" ! "s" ! "z" ! V"m" :@ (V"n" :@ V"s") :@ V"z")
  , ("fac",    "m" ! "s" ! V"m" :@ ("x" ! "n" ! V"n" :@ (V"x" :@ ("a" ! "b" ! V"n" :@ V"a" :@ (V"a" :@ V"b")))) :@ ("y" ! V"s") :@ ("w" ! V"w"))
  , ("sumto",  "m" ! V"fst" :@ (V"m" :@ ("p" ! V"pair" :@ (V"add" :@ V"one" :@ (V"add" :@ (V"fst" :@ V"p") :@ (V"snd" :@ V"p"))) :@ (V"add" :@ V"one" :@ (V"snd" :@ V"p"))) :@ (V"pair" :@ V"zero" :@ V"zero")))
  , ("equal",  "m" ! "n" ! V"and" :@ (V"isZero" :@ (V"sub" :@ V"m" :@ V"n")) :@ (V"isZero" :@ (V"sub" :@ V"n" :@ V"m")))
  , ("n5",     V"add" :@ V"two" :@ V"three")
  , ("n6",     V"add" :@ V"three" :@ V"three")
  , ("n17",    V"add" :@ V"n6" :@ (V"add" :@ V"n6" :@ V"n5"))
  , ("n37",    V"succ" :@ (V"mul" :@ V"n6" :@ V"n6"))
  , ("n703",   V"sumto" :@ V"n37")
  , ("n720",   V"fac" :@ V"n6")
  ]
