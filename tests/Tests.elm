module Tests exposing (suite)


import Test exposing (Test, test, describe)
import Expect

import MultiDict exposing (MultiDict, Uniqueness, empty, unique)
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


with2: MultiDict CharWithCode
with2=
  empty [ unique .code, unique .char ]
  |>MultiDict.insert at0
  |>MultiDict.insert at1

listOf2: List CharWithCode
listOf2=
  [ at0, at1 ]

type alias BracketMatch=
  { open: Char, closed: Char }

brackets: MultiDict BracketMatch
brackets=
  MultiDict.fromItems [ unique .open, unique .closed ]
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
    [ describe "fromItems is the same as empty |>inserting"
        [ test "empty  is  fromItems []"
          <|\()->
              MultiDict.equal
                (empty [ unique .code, unique .char ])
                (MultiDict.fromItems [ unique .code, unique .char ] [])
              |>Expect.true "empty equal to fromItems []"
        , test "fromItems gives same result as inserting"
          <|\()->
              MultiDict.equal
                with2
                (MultiDict.fromItems [ unique .code, unique .char ]
                  listOf2
                )
              |>Expect.true "fromItems gives same result as inserting"
        , test "fromItems ignores duplicates as in example"
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
              MultiDict.size
                (MultiDict.fromItems [ unique .lowercase, unique .uppercase ]
                  badList
                )
              |>Expect.equal 3
        ]
    ]

scanScanTest: Test
scanScanTest=
  describe "scan"
    [ describe "size"
        [ test "size of empty is 0"
          <|\()->
              MultiDict.size (empty [ unique .code, unique .char ])
              |>Expect.equal 0
        , test "dict has the same size as a list of unique values"
          <|\()->
              Expect.equal 42
                (MultiDict.size
                  (MultiDict.fromItems
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
            MultiDict.empty [ unique .lowercase, unique .uppercase ]
            |>MultiDict.insert { lowercase= 'a', uppercase= 'A' }
            |>MultiDict.insert { lowercase= 'b', uppercase= 'B' }

          lowercase char=
            MultiDict.access .uppercase char
              casedLetters
            |>Maybe.map .lowercase
          uppercase char=
            MultiDict.access .lowercase char
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
              MultiDict.fromItems
                [ unique .letter, unique .code ]
                [ { letter= 'a', code= 97 }
                , { letter= 'b', code= 98 }
                ]
            fancyCompetingLetterCodes=
              empty [ unique .code, unique .letter ]
              |>MultiDict.insert { code= 98, letter= 'b' }
              |>MultiDict.insert { code= 97, letter= 'a' }
          in
          MultiDict.equal
            letterCodes
            fancyCompetingLetterCodes
          |>Expect.true "reversed list with switched left right fromItems equal to fromItems"
    , describe "emptyOrMore examples work"
        [ test "isEmpty"
          <|\()->
            let
              isEmpty=
                MultiDict.emptyOrMore
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
                MultiDict.emptyOrMore
                  { ifMore= \pair _-> Just pair
                  , ifEmpty= Nothing
                  }
            in
            mostRecentlyInserted
              (MultiDict.fromItems
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
              MultiDict.size
                (with2
                |>MultiDict.insert at0
                |>MultiDict.insert at1
                )
              |>Expect.equal 2
        , test "access code is Just letter of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>MultiDict.insert at1
              |>MultiDict.access .code (.code at1)
              |>Maybe.map .char
                |>Expect.equal (Just (.char at1))
        , test "access code is Nothing if not of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>MultiDict.insert at1
              |>MultiDict.access .code (.code at0)
              |>Maybe.map .char
                |>Expect.equal Nothing
        , test "access char is Just left of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>MultiDict.insert at1
              |>MultiDict.access .char (.char at1)
              |>Maybe.map .code
                |>Expect.equal (Just (.code at1))
        , test "access char is Nothing if not of inserted pair"
          <|\()->
              empty [ unique .code, unique .char ]
              |>MultiDict.insert at1
              |>MultiDict.access .char (.char at0)
              |>Maybe.map .code
                |>Expect.equal Nothing
        , test "put in example"
          <|\()->
              let
                result=
                  empty [ unique .lowercase, unique .uppercase ]
                    --lowercase and uppercase are unique keys across each value
                  |>MultiDict.insert
                      { lowercase= 'b', uppercase= 'B', rating= 0.5 }
                      --put in 
                  |>MultiDict.insert
                      { lowercase= 'a', uppercase= 'A', rating= 0.5 }
                      --put in, because rating is not a key
                  |>MultiDict.insert
                      { lowercase= 'b', uppercase= 'C', rating= 0 }
                      --ignored, the left value already exists
                  |>MultiDict.insert
                      { lowercase= 'c', uppercase= 'A', rating= 0 }
                      --ignored, the right value already exists
                  |>MultiDict.insert
                      { lowercase= 'c', uppercase= 'C', rating= 0.6 }
                      --put in
              in
              MultiDict.equal
                result
                (MultiDict.fromItems
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
          MultiDict.union
            (MultiDict.fromItems
              [ unique .operator, unique .name ]
              custumNamedOperators
            )
            (MultiDict.fromItems
              [ unique .name, unique .operator ]
              numberNamedOperators
            )
        fromItemsOfConcatenated=
          MultiDict.fromItems
            [ unique .name, unique .operator ]
            (numberNamedOperators
            ++custumNamedOperators
            )
        encode=
          MultiDict.encode
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
            ("union of fromItems dicts ("
            ++(encode validNamedOperators)
            ++"equal to fromItems of concatenated lists ("
            ++(encode fromItemsOfConcatenated)
            ++")"
            )
            (MultiDict.equal
              validNamedOperators
              fromItemsOfConcatenated
            )
    ]

outTest: Test
outTest=
  describe "out"
    [ test "add and remove code leaves it unchanged"
      <|\()->
          with2
          |>MultiDict.insert { code= 2, char= 'C' }
          |>MultiDict.remove .code 2
          |>Expect.equal with2
    , test "add and remove char leaves it unchanged"
      <|\()->
          with2
          |>MultiDict.insert { code= 2, char= 'C' }
          |>MultiDict.remove .char 'C'
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
              |>MultiDict.fold
                  (\{ open, closed }->
                    (::) (String.fromList [ open, closed ])
                  )
                  []
          in
          Expect.equal
            openingAndClosing [ "()", "{}" ]
    , test "map works as in the example"
      <|\()->
          let
            digitNames=
              MultiDict.empty [ unique .number, unique .name ]
              |>MultiDict.insert { number= 0, name= "zero" }
              |>MultiDict.insert { number= 1, name= "one" }

            mathSymbolNames=
              digitNames
              |>MultiDict.map
                  (\{ number, name }->
                    { symbol= String.fromInt number, name= name }
                  )
                  [ unique .symbol, unique .name ]
              |>MultiDict.insert { symbol= "+", name= "plus" }
          in
          Expect.true "mapped MultiDict equal to fromItems"
            (MultiDict.equal
              mathSymbolNames
              (MultiDict.fromItems
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
              MultiDict.fromItems
                [ unique .lowercase, unique .uppercase ]
                [ { uppercase= 'A', lowercase= 'a' }
                , { uppercase= 'B', lowercase= 'b' }
                ]
            lowerFromUppercase=
              MultiDict.toDict .uppercase .lowercase
                casedLetters
          in
          Expect.true "MultiDict.fromItems toDict equal to AssocDict.fromList"
            (AssocDict.eq
              lowerFromUppercase
              (AssocDict.fromList [ ( 'A', 'a' ), ( 'B', 'b' ) ])
            )
    , encodeDecodeTest
    ]

encodeDecodeTest: Test
encodeDecodeTest=
  test "encoded & decoded MultiDict is the same"
  <|\()->
      let
        encoded=
          with2
          |>MultiDict.encode
              (\{ code, char }->
                Encode.object
                  [ ( "code", Encode.int code )
                  , ( "char", Encode.int (Char.toCode char) )
                  ]
              )
        encodedDecoded=
          encoded
          |>Decode.decodeValue
              (MultiDict.decode [ unique .code, unique .char ]
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
            (MultiDict.equal decoded with2)
        
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
              |>MultiDict.access .open character
              |>Maybe.map
                  (\{ closed }->
                    String.fromList [ character, closed ]
                  )
              |>Maybe.withDefault
                  (brackets
                  |>MultiDict.access .closed character
                  |>Maybe.map
                      (\{ open }->
                        String.fromList [ open, character ]
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
              MultiDict.empty [ unique .lowercase, unique .uppercase ]
              |>MultiDict.insert { lowercase= 'a', uppercase= 'A' }
              |>MultiDict.insert { lowercase= 'b', uppercase= 'B' }
              |>MultiDict.insert { lowercase= 'c', uppercase= 'C' }

            upperCase char=
              MultiDict.access .lowercase char lowerUppercaseLetters
              |>Maybe.map .uppercase
          in
          Expect.equal
            ([ 'c', 'a', 'x' ] |>List.map upperCase)
            ([ Just 'C', Just 'A', Nothing ])
    , test "periodic table"
      <|\()->
          let
            elementAtomicNumberPairdict=
              MultiDict.fromItems
                [ unique .element, unique .atomicNumber ]
                [ { element= Hydrogen, atomicNumber= 1 }
                , { element= Helium, atomicNumber= 2 }
                ]
            atomicNumberByElement=
              MultiDict.toDict .element .atomicNumber
                elementAtomicNumberPairdict
          in
          Expect.equal
            [ Just 2, Just 1 ]
            [ AssocDict.get Helium atomicNumberByElement
            , AssocDict.get Hydrogen atomicNumberByElement
            ]
    ]

