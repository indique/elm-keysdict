module Benchmarks exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram)
import Benchmark exposing
  ( Benchmark, benchmark, describe )

import AssocList as AssocDict

import MultiDict exposing (MultiDict, unique)


main: BenchmarkProgram
main=
  Benchmark.Runner.program suite

suite: Benchmark
suite=
  describe "multi-dict"
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

multiDict: MultiDict CasedLetter
multiDict=
  MultiDict.fromItems
    [ unique .lowercase, unique .uppercase ]
    casedLetterList


fold: Benchmark
fold=
  Benchmark.compare "fold"
    "MultiDict.fold"
    (\()-> MultiDict.fold (::) [] multiDict)
    "foldr"
    (\()-> foldr (::) [] multiDict)

foldr reduce initial=
  MultiDict.items
  >>List.foldr reduce initial


multiDictReversed: MultiDict CasedLetter
multiDictReversed=
  MultiDict.fromItems
    [ unique .uppercase, unique .lowercase ]
    (casedLetterList |>List.reverse)

equal: Benchmark
equal=
  Benchmark.compare "equal <?> alternative equals"
    "equal"
    (\()->
      MultiDict.equal multiDict multiDictReversed
    )
    "altEqual"
    (\()->
      altEqual multiDict multiDictReversed
    )


altEqual:
  MultiDict CasedLetter ->MultiDict CasedLetter
  ->Bool
altEqual aMultiDict bMultiDict=
  equalLists
    (MultiDict.items aMultiDict)
    (MultiDict.items bMultiDict)

equalLists: List el ->List el ->Bool
equalLists aList bList=
  case aList of
    head ::aNextValues->
      case removeWithResult head bList of
        ChangedList aWithout->
          equalLists aNextValues aWithout
        
        AsBeforeList _->
          False

    []->
      List.isEmpty bList
          

{-| Remove the first element (left to right) in a `List` where `toRemove` is equal.
-}
removeWithResult:
  el ->List el ->ListOperationResult el
removeWithResult toRemove=
  removeWithResultHelp toRemove []

removeWithResultHelp:
  el ->List el ->List el
  ->ListOperationResult el
removeWithResultHelp toRemove notToRemove list=
  case list of
    []-> AsBeforeList []

    head ::after->
      case (==) toRemove head of
        True->
          ChangedList
            ((notToRemove |>List.reverse) ++after)

        False->
          removeWithResultHelp
            toRemove (head ::notToRemove) after

type ListOperationResult el=
  AsBeforeList (List el)
  | ChangedList (List el)

