module Benchmarks exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram)
import Benchmark exposing
  ( Benchmark, benchmark, describe )

import AssocList
import Util exposing
  ( aspect
  , removeFirstWithResult, ListOperationResult(..)
  )

import KeysDict.Uniqueness exposing (door)
import KeysDict exposing (KeysDict, houses, putUp)


main: BenchmarkProgram
main=
  Benchmark.Runner.program suite

suite: Benchmark
suite=
  describe "keys-dict"
    [ equal
    , fold
    ]

type alias CasedLetter=
  { lowercase: Char
  , uppercase: Char
  }

{-|
upperCaseLatin= range 65 90
lowerCaseLatin= range 97 122
-}
casedLetterList: List CasedLetter
casedLetterList=
  List.range 65 90
  |>List.map
      (\upperCaseLatin->
        { uppercase=
            Char.fromCode upperCaseLatin
        , lowercase=
            Char.fromCode (upperCaseLatin +32)
        }
      )

keysDict: KeysDict CasedLetter
keysDict=
  casedLetterList
  |>List.foldl putUp
      (KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
      )
    


fold: Benchmark
fold=
  Benchmark.compare "fold"
    "foldHouses"
    (\()-> KeysDict.foldHouses (::) [] keysDict)
    "foldr"
    (\()-> foldr (::) [] keysDict)

foldr reduce initial=
  houses
  >>List.foldr reduce initial


keysDictReversed: KeysDict CasedLetter
keysDictReversed=
  casedLetterList |>List.reverse
  |>List.foldl putUp
      (KeysDict.enterableBy
        [ door .uppercase, door .lowercase ]
      )

equal: Benchmark
equal=
  Benchmark.compare "equal <?> alternative equals"
    "equal"
    (\()->
      KeysDict.equal keysDict keysDictReversed
    )
    "altEqual"
    (\()->
      altEqual keysDict keysDictReversed
    )


altEqual:
  KeysDict CasedLetter ->KeysDict CasedLetter
  ->Bool
altEqual=
  (aspect houses) equalLists

equalLists: List el ->List el ->Bool
equalLists aList bList=
  case aList of
    head ::aNextValues->
      case removeFirstWithResult head bList of
        ChangedList aWithout->
          equalLists aNextValues aWithout
        
        AsBeforeList _->
          False

    []->
      List.isEmpty bList

