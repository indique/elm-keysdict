module Tests exposing (suite)


import Test exposing (Test, test, describe)
import Expect

import MultiDict exposing (KeysDict, Uniqueness, empty, unique)
import AssocList as AssocDict exposing (Dict)

import Json.Encode as Encode
import Json.Decode as Decode


suite: Test
suite=
  describe "pair-dict"
    [ createTest
    , scanScanTest
    , inTest
    , outTest
    , shapeTest
    , readmeExamplesTest
    ]


type alias CharWithCode=
  { code: Int, char: Char }

at0: CharWithCode
at0= { code= 0, char= 'A' }

at1: CharWithCode
at1= { code= 1, char= 'B' }


with2: KeysDict CharWithCode
with2=
  empty [ unique .code, unique .char ]
  |>KeysDict.putIn at0
  |>KeysDict.putIn at1

listOf2: List CharWithCode
listOf2=
  [ at0, at1 ]

type alias BracketMatch=
  { open: Char, closed: Char }

brackets: KeysDict BracketMatch
brackets=
  KeysDict.fromValues [ unique .open, unique .closed ]
    [ { open= '(', closed= ')' }
    , { open= '{', closed= '}' }
    ]

type alias CasedLetter=
  { lowercase: Char
  , uppercase: Char
  }

createTest: Test
createTest=
  describe "create"
    [ describe "fromValues is the same as empty |>inserting"
        [ test "empty  is  fromValues []"
          <|\()->
              KeysDict.equal
                (empty [ unique .code, unique .char ])
                (KeysDict.fromValues [ unique .code, unique .char ] [])
              |>Expect.true "empty equal to fromValues []"
        , test "fromValues gives same result as inserting"
          <|\()->
              KeysDict.equal
                with2
                (KeysDict.fromValues [ unique .code, unique .char ]
                  listOf2
                )
              |>Expect.true "fromValues gives same result as inserting"
        , test "fromValues ignores duplicates as in example"
          <|\()->
              let
                badList=
                  [ { lowercase= 'b', uppercase= 'B' } --put in
                  , { lowercase= 'a', uppercase= 'A' } --put in
                  , { lowercase= 'b', uppercase= 'C' } --ignored, as the left value already exists
                  , { lowercase= 'c', uppercase= 'A' } --ignored, as the right value already exists
                  , { lowercase= 'c', uppercase= 'C' } --put in
                  ]
              in
              KeysDict.size
                (KeysDict.fromValues [ unique .lowercase, unique .uppercase ]
                  badList
                )
              |>Expect.equal 3
        , test "fromDict dictFromLeft returns an equal dict"
          <|\()->
              let
                lowerToUpperLetters=
                  AssocDict.empty
                  |>AssocDict.insert 'a' 'A'
                  |>AssocDict.insert 'b' 'B'

                lowerUpperLetters=
                  KeysDict.fromDict
                    (\k v-> { lowercase= k, uppercase= v })
                    [ unique .lowercase, unique .uppercase ]
                    lowerToUpperLetters
              in
              Expect.true "expected fromDict toDict returns an equal dict"
                (AssocDict.eq
                  lowerToUpperLetters
                  (KeysDict.toDict .lowercase .uppercase
                    lowerUpperLetters
                  )
                )
        ]
    ]

scanScanTest: Test
scanScanTest=
  describe "scan"
    [ describe "size"
        [ test "size of empty is 0"
          <|\()->
              KeysDict.size (empty [ unique .code, unique .char ])
              |>Expect.equal 0
        , test "dict has the same size as a list of unique values"
          <|\()->
              Expect.equal 42
                (KeysDict.size
                  (KeysDict.fromValues
                    [ unique Tuple.first, unique Tuple.second ]
                    (List.range 0 41
                    |>List.map (\i-> ( i, i ))
                    )
                  )
                )
        ]
    , describe "access"
        (let
          casedLetters=
            KeysDict.empty [ unique .lowercase, unique .uppercase ]
            |>KeysDict.putIn { lowercase= 'a', uppercase= 'A' }
            |>KeysDict.putIn { lowercase= 'b', uppercase= 'B' }

          lowercase char=
            KeysDict.access .uppercase char
              casedLetters
            |>Maybe.map .lowercase
          uppercase char=
            KeysDict.access .lowercase char
              casedLetters
            |>Maybe.map .uppercase
        in
        [ test "finds lowercase"
          <|\()->
              List.map lowercase [ 'a', 'B' ]
              |>Expect.equal
                  [ Nothing, Just 'b' ]
        , test "finds uppercase"
          <|\()->
              List.map uppercase [ 'b', 'A' ]
              |>Expect.equal
                  [ Just 'B', Nothing ]
        ]
        )
    , test "equal example works"
      <|\()->
          let
            letterCodes=
              KeysDict.fromValues
                [ unique .letter, unique .code ]
                [ { letter= 'a', code= 97 }
                , { letter= 'b', code= 98 }
                ]
            fancyCompetingLetterCodes=
              empty [ unique .code, unique .letter ]
              |>KeysDict.putIn { code= 98, letter= 'b' }
              |>KeysDict.putIn { code= 97, letter= 'a' }
          in
          KeysDict.equal
            letterCodes
            fancyCompetingLetterCodes
          |>Expect.true "reversed list with switched left right fromValues equal to fromValues"
    , describe "emptyOrMore examples work"
        [ test "isEmpty"
          <|\()->
            let
              isEmpty=
                KeysDict.emptyOrMore
                  { ifEmpty= True
                  , ifMore= \_ _-> False
                  }
            in
            Expect.true "isEmpty for filled False, ifEmpty True"
              ((&&)
                (isEmpty (empty [ unique .code, unique .char ]))
                (not (isEmpty with2))
              )
        , test "most recently inserted"
          <|\()->
            let
              mostRecentlyInserted=
                KeysDict.emptyOrMore
                  { ifMore= \pair _-> Just pair
                  , ifEmpty= Nothing
                  }
            in
            mostRecentlyInserted
              (KeysDict.fromValues
                [ unique .lowercase, unique .uppercase ]
                [ { lowercase= 'a', uppercase= 'A' },
                  { lowercase= 'b', uppercase= 'B' }
                ]
              )
            |>Expect.equal
                (Just { lowercase= 'b', uppercase= 'B' })
        ]
    ]

inTest: Test
inTest=
  describe "in"
    [ describe "insert"
        [ test "insert is ignored for duplicates"
          <|\()->
              KeysDict.size
                (with2
                |>KeysDict.putIn at0
                |>KeysDict.putIn at1
                )
              |>Expect.equal 2
        , test "access code is Just letter of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>KeysDict.putIn at1
              |>KeysDict.access .code (.code at1)
              |>Maybe.map .char
                |>Expect.equal (Just (.char at1))
        , test "access code is Nothing if not of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>KeysDict.putIn at1
              |>KeysDict.access .code (.code at0)
              |>Maybe.map .char
                |>Expect.equal Nothing
        , test "access char is Just left of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>KeysDict.putIn at1
              |>KeysDict.access .char (.char at1)
              |>Maybe.map .code
                |>Expect.equal (Just (.code at1))
        , test "access char is Nothing if not of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>KeysDict.putIn at1
              |>KeysDict.access .char (.char at0)
              |>Maybe.map .code
                |>Expect.equal Nothing
        , test "put in example"
          <|\()->
              let
                result=
                  empty [ unique .lowercase, unique .uppercase ]
                    --lowercase and uppercase are unique keys across each value
                  |>KeysDict.putIn
                      { lowercase= 'b', uppercase= 'B', rating= 0.5 }
                      --put in 
                  |>KeysDict.putIn
                      { lowercase= 'a', uppercase= 'A', rating= 0.5 }
                      --put in, because rating is not a key
                  |>KeysDict.putIn
                      { lowercase= 'b', uppercase= 'C', rating= 0 }
                      --ignored, the left value already exists
                  |>KeysDict.putIn
                      { lowercase= 'c', uppercase= 'A', rating= 0 }
                      --ignored, the right value already exists
                  |>KeysDict.putIn
                      { lowercase= 'c', uppercase= 'C', rating= 0.6 }
                      --put in
              in
              KeysDict.equal
                result
                (KeysDict.fromValues
                  [ unique .lowercase, unique .uppercase ]
                  [ { lowercase= 'c', uppercase= 'C', rating= 0.6 }
                  , { lowercase= 'b', uppercase= 'B', rating= 0.5 }
                  , { lowercase= 'a', uppercase= 'A', rating= 0.5 }
                  ]
                )
              |>Expect.true "keys are unique, others aren't"
        ]
    , let
        numberNamedOperators=
          [ { operator= '+', name= "plus" }
          , { operator= '-', name= "minus" }
          ]
        custumNamedOperators=
          [ { operator= '∧', name= "and" }
          , { operator= '∨', name= "or" }
          , { operator= '-', name= "negate" }
          ]
        validNamedOperators=
          KeysDict.union
            (KeysDict.fromValues
              [ unique .operator, unique .name ]
              custumNamedOperators
            )
            (KeysDict.fromValues
              [ unique .name, unique .operator ]
              numberNamedOperators
            )
        fromValuesOfConcatenated=
          KeysDict.fromValues
            [ unique .name, unique .operator ]
            (numberNamedOperators
            ++custumNamedOperators
            )
        encode=
          KeysDict.encode
            (\{ operator, name }->
              Encode.object
                [ ( "operator"
                  , Encode.string (String.fromChar operator)
                  )
                , ( "name", Encode.string name )
                ]
            )
          >>Encode.encode 2
      in
      test "union example"
      <|\()->
          Expect.true
            ("union of fromValues dicts ("
            ++(encode validNamedOperators)
            ++"equal to fromValues of concatenated lists ("
            ++(encode fromValuesOfConcatenated)
            ++")"
            )
            (KeysDict.equal
              validNamedOperators
              fromValuesOfConcatenated
            )
    ]

outTest: Test
outTest=
  describe "out"
    [ test "add and remove code leaves it unchanged"
      <|\()->
          with2
          |>KeysDict.putIn { code= 2, char= 'C' }
          |>KeysDict.remove .code 2
          |>Expect.equal with2
    , test "add and remove char leaves it unchanged"
      <|\()->
          with2
          |>KeysDict.putIn { code= 2, char= 'C' }
          |>KeysDict.remove .char 'C'
          |>Expect.equal with2
    ]


shapeTest: Test
shapeTest=
  describe "shape"
    [ test "fold works as in the example"
      <|\()->
          let
            openingAndClosing=
              brackets
              |>KeysDict.fold
                  (\{ open, closed } acc->
                    acc ++[ String.fromValues [ open, closed ] ]
                  )
                  []
          in
          Expect.equal
            openingAndClosing [ "{}", "()" ]
    , test "map works as in the example"
      <|\()->
          let
            digitNames=
              KeysDict.empty [ unique .number, unique .name ]
              |>KeysDict.putIn { number= 0, name= "zero" }
              |>KeysDict.putIn { number= 1, name= "one" }

            mathSymbolNames=
              digitNames
              |>KeysDict.map
                  (\{ number, name }->
                    { symbol= String.fromInt number, name= name }
                  )
                  [ unique .symbol, unique .name ]
              |>KeysDict.putIn { symbol= "+", name= "plus" }
          in
          Expect.true "mapped KeysDict equal to fromValues"
            (KeysDict.equal
              mathSymbolNames
              (KeysDict.fromValues
                [ unique .symbol, unique .name ]
                [ { symbol= "0", name= "zero" }
                , { symbol= "1", name= "one" }
                , { symbol= "+", name= "plus" }
                ]
              )
            )
    , test "toDict example works"
      <|\()->
          let
            casedLetters=
              KeysDict.fromValues
                [ unique .lowercase, unique .uppercase ]
                [ { uppercase= 'A', lowercase= 'a' }
                , { uppercase= 'B', lowercase= 'b' }
                ]
            lowerFromUppercase=
              KeysDict.toDict .uppercase .lowercase
                casedLetters
          in
          Expect.true "KeysDict.fromValues toDict equal to AssocDict.fromValues"
            (AssocDict.eq
              lowerFromUppercase
              (AssocDict.fromValues [ ( 'A', 'a' ), ( 'B', 'b' ) ])
            )
    , encodeDecodeTest
    ]

encodeDecodeTest: Test
encodeDecodeTest=
  test "encoded & decoded KeysDict is the same"
  <|\()->
      let
        encoded=
          with2
          |>KeysDict.encode
              (\{ code, char }->
                Encode.object
                  [ ( "code", Encode.int code )
                  , ( "char", Encode.int (Char.toCode char) )
                  ]
              )
        encodedDecoded=
          encoded
          |>Decode.decodeValue
              (KeysDict.decode [ unique .code, unique .char ]
                (Decode.map2 CharWithCode
                  (Decode.field "code" Decode.int)
                  (Decode.field "char"
                    (Decode.map Char.fromCode Decode.int)
                  )
                )
              )
      in
      case encodedDecoded of
        Ok decoded->
          Expect.true "encoded |>decoded equal to before"
            (KeysDict.equal decoded with2)
        
        Err err->
          Expect.fail (Decode.errorToString err)


type Element=
  Hydrogen
  | Helium
type alias ProtonCount= Int

readmeExamplesTest: Test
readmeExamplesTest=
  describe "readme examples work"
    [ test "braces"
      <|\()->
          let
            typeChar character=
              brackets
              |>KeysDict.access .open character
              |>Maybe.map
                  (\{ closed }->
                    String.fromValues [ character, closed ]
                  )
              |>Maybe.withDefault
                  (brackets
                  |>KeysDict.access .closed character
                  |>Maybe.map
                      (\{ open }->
                        String.fromValues [ open, character ]
                      )
                  |>Maybe.withDefault
                      (String.fromChar character)
                  )
            in
            Expect.equal
              ([ '(', '}' ] |>List.map typeChar)
              [ "()", "{}" ]
    , test "cased letters"
      <|\()->
          let
            lowerUppercaseLetters=
              KeysDict.empty [ unique .lowercase, unique .uppercase ]
              |>KeysDict.putIn { lowercase= 'a', uppercase= 'A' }
              |>KeysDict.putIn { lowercase= 'b', uppercase= 'B' }
              |>KeysDict.putIn { lowercase= 'c', uppercase= 'C' }

            upperCase char=
              KeysDict.access .lowercase char lowerUppercaseLetters
              |>Maybe.map .uppercase
          in
          Expect.equal
            ([ 'c', 'a', 'x' ] |>List.map upperCase)
            ([ Just 'C', Just 'A', Nothing ])
    , test "periodic table"
      <|\()->
          let
            elementAtomicNumberPairdict=
              KeysDict.fromValues
                [ unique .element, unique .atomicNumber ]
                [ { element= Hydrogen, atomicNumber= 1 }
                , { element= Helium, atomicNumber= 2 }
                ]
            atomicNumberByElement=
              KeysDict.toDict .element .atomicNumber
                elementAtomicNumberPairdict
          in
          Expect.equal
            [ Just 2, Just 1 ]
            [ AssocDict.get Helium atomicNumberByElement
            , AssocDict.get Hydrogen atomicNumberByElement
            ]
    ]

