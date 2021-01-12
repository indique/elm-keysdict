module KeysDictTest exposing (suite)


import Test exposing (Test, test, describe)
import Expect

import KeysDict exposing 
  ( KeysDict
  , houses, countHouses, enterBy
  , putUp, foldHouses
  )
import KeysDict.Uniqueness as Uniqueness exposing (door)
import AssocList as AssocDict

import Json.Encode as Encode
import Json.Decode as Decode
import Serialize


suite: Test
suite=
  describe "KeysDict"
    [ scanTest
    , inTest
    , outTest
    , shapeTest
    , readmeExamplesTest
    ]


type alias CharWithCode=
  { char: Char, code: Int }

at0: CharWithCode
at0= { code= 0, char= 'A' }

at1: CharWithCode
at1= { code= 1, char= 'B' }


with2: KeysDict CharWithCode
with2=
  KeysDict.enterableBy
    [ door .code, door .char ]
  |>putUp at0
  |>putUp at1

listOf2: List CharWithCode
listOf2=
  [ at0, at1 ]

type alias BracketMatch=
  { open: Char, closed: Char }

brackets: KeysDict BracketMatch
brackets=
  KeysDict.enterableBy
    [ door .open, door .closed ]
  |>putUp { open= '(', closed= ')' }
  |>putUp { open= '{', closed= '}' }

type alias CasedLetter=
  { lowercase: Char
  , uppercase: Char
  }

scanTest: Test
scanTest=
  describe "scan"
    [ describe "countHouses"
        [ test "countHouses of empty is 0"
          <|\()->
              countHouses
                (KeysDict.enterableBy
                  [ door .code, door .char ]
                )
              |>Expect.equal 0
        , test "keysDict has the same size as a list of unique houses"
          <|\()->
              List.range 0 41
                |>List.map (\i-> ( i, i ))
              |>List.foldl putUp
                  (KeysDict.enterableBy
                    [ door Tuple.first, door Tuple.second ]
                  )
              |>countHouses
              |>Expect.equal 42
        ]
    , describe "enter by"
        (let
          casedLetters=
            KeysDict.enterableBy
              [ door .lowercase, door .uppercase ]
            |>putUp { lowercase= 'a', uppercase= 'A' }
            |>putUp { lowercase= 'b', uppercase= 'B' }

          lowercase char=
            enterBy { door= .uppercase, key= char }
              casedLetters
            |>Maybe.map .lowercase
          uppercase char=
            enterBy { door= .lowercase, key= char }
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
              [ { letter= 'a', code= 97 }
              , { letter= 'b', code= 98 }
              ]
              |>List.foldl putUp
                  (KeysDict.enterableBy
                    [ door .letter, door .code ]
                  )
                  
            fancyCompetingLetterCodes=
              KeysDict.enterableBy
                [ door .code, door .letter ]
              |>putUp { code= 98, letter= 'b' }
              |>putUp { code= 97, letter= 'a' }
          in
          KeysDict.equal
            letterCodes
            fancyCompetingLetterCodes
          |>Expect.true "from reversed list equal to before"
    , describe "houses examples work"
        [ test "isEmpty"
          <|\()->
            let
              isEmpty=
                List.isEmpty <<houses
            in
            Expect.true "isEmpty for filled False, ifEmpty True"
              ((&&)
                (isEmpty
                  (KeysDict.enterableBy
                    [ door .code, door .char ]
                  )
                )
                (not (isEmpty with2))
              )
        , test "most recently inserted"
          <|\()->
            let
              mostRecentlyInserted=
                List.head <<houses
            in
            mostRecentlyInserted
              (KeysDict.enterableBy
                [ door .lowercase, door .uppercase ]
              |>putUp { lowercase= 'a', uppercase= 'A' }
              |>putUp { lowercase= 'b', uppercase= 'B' }
              )
            |>Expect.equal
                (Just { lowercase= 'b', uppercase= 'B' })
        ]
    ]

inTest: Test
inTest=
  describe "in"
    [ describe "put up"
        [ test "put up is ignored for duplicates"
          <|\()->
              KeysDict.countHouses
                (with2
                |>putUp at0
                |>putUp at1
                )
              |>Expect.equal 2
        , test "access code is Just letter of inserted pair"
          <|\()->
              KeysDict.enterableBy
                [ door .code, door .char ]
              |>putUp at1
              |>enterBy { door= .code, key= .code at1 }
              |>Maybe.map .char
                |>Expect.equal (Just (.char at1))
        , test "enter by code is Nothing if not of inserted pair"
          <|\()->
              KeysDict.enterableBy
                [ door .code, door .char ]
              |>putUp at1
              |>enterBy { door= .code, key= .code at0 }
              |>Maybe.map .char
                |>Expect.equal Nothing
        , test "enter by char is Just left of inserted pair"
          <|\()->
              KeysDict.enterableBy
                [ door .code, door .char ]
              |>putUp at1
              |>enterBy { door= .char, key= .char at1 }
              |>Maybe.map .code
                |>Expect.equal (Just (.code at1))
        , test "enter by char is Nothing if not of inserted pair"
          <|\()->
              KeysDict.enterableBy
                [ door .code, door .char ]
              |>putUp at1
              |>enterBy { door= .char, key= .char at0 }
              |>Maybe.map .code
                |>Expect.equal Nothing
        , test "put in example"
          <|\()->
              let
                result=
                  KeysDict.enterableBy
                    [ door .lowercase, door .uppercase ]
                    --lowercase and uppercase are unique keys across each value
                  |>putUp { lowercase= 'b', uppercase= 'B', rating= 0.5 }
                      --put in 
                  |>putUp { lowercase= 'a', uppercase= 'A', rating= 0.5 }
                      --put in, because rating is not a key
                  |>putUp { lowercase= 'b', uppercase= 'C', rating= 0 }
                      --ignored, the left value already exists
                  |>putUp { lowercase= 'c', uppercase= 'A', rating= 0 }
                      --ignored, the right value already exists
                  |>putUp { lowercase= 'c', uppercase= 'C', rating= 0.6 }
                      --put in
              in
              KeysDict.equal
                result
                (KeysDict.enterableBy [ door .lowercase, door .uppercase ]
                |>putUp { lowercase= 'c', uppercase= 'C', rating= 0.6 }
                |>putUp { lowercase= 'b', uppercase= 'B', rating= 0.5 }
                |>putUp{ lowercase= 'a', uppercase= 'A', rating= 0.5 }
                )
              |>Expect.true "keys are unique, others aren't"
        ]
    ]

outTest: Test
outTest=
  describe "out"
    [ test "put up and tear down code leaves it unchanged"
      <|\()->
          with2
          |>putUp { code= 2, char= 'C' }
          |>KeysDict.tearDown { door= .code, key= 2 }
          |>Expect.equal with2
    , test "put up and tear down char leaves it unchanged"
      <|\()->
          with2
          |>putUp { code= 2, char= 'C' }
          |>KeysDict.tearDown { door= .char, key= 'C' }
          |>Expect.equal with2
    ]


shapeTest: Test
shapeTest=
  describe "shape"
    [ test "foldHouses works as in the example"
      <|\()->
          let
            openingAndClosing=
              brackets
              |>foldHouses
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
              KeysDict.enterableBy 
                [ door .number, door .name ]
              |>putUp { number= 0, name= "zero" }
              |>putUp { number= 1, name= "one" }

            mathSymbolNames=
              digitNames
              |>KeysDict.map
                  (\{ number, name }->
                    { symbol= String.fromInt number, name= name }
                  )
                  [ door .symbol, door .name ]
              |>putUp { symbol= "+", name= "plus" }
          in
          Expect.true "mapped KeysDict equal to put up"
            (KeysDict.equal
              mathSymbolNames
              (KeysDict.enterableBy
                [ door .symbol, door .name ]
              |>putUp { symbol= "0", name= "zero" }
              |>putUp { symbol= "1", name= "one" }
              |>putUp { symbol= "+", name= "plus" }
              )
            )
    , test "toDict example works"
      <|\()->
          let
            casedLetters=
              KeysDict.enterableBy
                [ door .lowercase, door .uppercase ]
              |>putUp { uppercase= 'A', lowercase= 'a' }
              |>putUp { uppercase= 'B', lowercase= 'b' }
            lowerFromUppercase=
              casedLetters
              |>KeysDict.toDict { key= .uppercase, value= .lowercase }
          in
          Expect.true "putUp |> toDict equal to AssocList.fromList"
            (AssocDict.eq
              lowerFromUppercase
              (AssocDict.fromList [ ( 'A', 'a' ), ( 'B', 'b' ) ])
            )
    , serializeTest
    ]

serializeTest: Test
serializeTest=
  test "json encoded & decoded KeysDict is the same"
  <|\()->
      let
        serializeCharWithCode=
          Serialize.record CharWithCode
          |>(Serialize.field .char
              (Serialize.map Char.fromCode Char.toCode
                Serialize.int
              )
            )
          |>Serialize.field .code Serialize.int
          |>Serialize.finishRecord

        serializeCharWithCodeKeysDict=
          KeysDict.serialize serializeCharWithCode
            [ door .code, door .char ]
            
        encodedDecoded=
          Serialize.encodeToJson
            serializeCharWithCodeKeysDict
            with2
          |>Serialize.decodeFromJson
              serializeCharWithCodeKeysDict
      in
      case encodedDecoded of
        Ok decoded->
          Expect.true "encoded |>decoded equal to before"
            (KeysDict.equal decoded with2)
        
        Err err->
          Expect.fail (Debug.toString err)--too lazy to case on import Serialize exposing (Error(..))


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
              |>enterBy { door= .open, key= character }
              |>Maybe.map
                  (\{ closed }->
                    String.fromList [ character, closed ]
                  )
              |>Maybe.withDefault
                  (brackets
                  |>enterBy { door= .closed, key= character }
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
              KeysDict.enterableBy
                [ door .lowercase, door .uppercase ]
              |>putUp { lowercase= 'a', uppercase= 'A' }
              |>putUp { lowercase= 'b', uppercase= 'B' }
              |>putUp { lowercase= 'c', uppercase= 'C' }

            upperCase char=
              lowerUppercaseLetters
              |>enterBy { door= .lowercase, key= char }
              |>Maybe.map .uppercase
          in
          Expect.equal
            ([ 'c', 'a', 'x' ] |>List.map upperCase)
            ([ Just 'C', Just 'A', Nothing ])
    , test "periodic table"
      <|\()->
          let
            elementAtomicNumberPairdict=
              KeysDict.enterableBy [ door .element, door .atomicNumber ]
              |>putUp { element= Hydrogen, atomicNumber= 1 }
              |>putUp { element= Helium, atomicNumber= 2 }
              
            atomicNumberByElement=
              KeysDict.toDict { key= .element, value= .atomicNumber }
                elementAtomicNumberPairdict
          in
          [ AssocDict.get Helium atomicNumberByElement
          , AssocDict.get Hydrogen atomicNumberByElement
          ]
          |>Expect.equal [ Just 2, Just 1 ] 
    ]

