module KeysDict exposing
  ( KeysDict
  , Uniqueness, unique
  , equal
  , empty, fromDict, fromList
  , access, emptyOrMore, size
  , putIn, map
  , remove
  , fold
  , union
  , toDict
  , encode, decode
  )
{-|
@docs KeysDict

@docs Uniqueness, unique

## create
@docs empty, fromDict, fromList, decode

## scan
@docs equal, access, emptyOrMore, size

## in
@docs putIn, union

## out
@docs remove

## shape
@docs fold, map, toDict, encode
-}

import AssocList as AssocDict

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode exposing (value)


{-| Want to look up values from many directions?

> You want the pair where `🗝️` is `1` and the pair where `🔑` is `0`?

      → < 🔑= 0, 🏠= 🍐, 🗝️= 2 >
        < 🔑= 2, 🏠= 🌳, 🗝️= 0 >
        < 🔑= 1, 🏠= 🍐, 🗝️= 1 > ←

        (🔑 & 🗝️ are unique)

> Going through while checking every pair, if `🗝️` is equal, then, if `🔑` is equal.

        🔑= 1, 🗝️= 1 where 🗝️ is 1    🔑= 0, 🗝️= 2 where 🔑 is 0

Like [assoc-list](https://github.com/pzp1997/assoc-list), a `KeysDict` allows union types.

## Example: cased letters
    type alias CasedLetter=
      { lowercase: Char
      , uppercase: Char
      }

    casedLetters: KeysDict CasedLetter
    casedLetters=
      KeysDict.empty [ unique .lowercase, unique .uppercase ]
      |>KeysDict.putIn { lowercase= 'a', uppercase= 'A' }
      |>KeysDict.putIn { lowercase= 'b', uppercase= 'B' }
      |>KeysDict.putIn { lowercase= 'c', uppercase= 'C' }

    uppercase char=
      KeysDict.access .lowercase char lowerUppercaseLetters
      |>Maybe.map .uppercase
-}
type KeysDict value=
  ValuesWithPromises
    (List (Uniqueness value)) (List value)

{-| Ensures, that there is not more than one value in the `KeysDict`, where a given condition is `True` (e.g. a given key being equal to a value)

That sounds a bit complicated, so let's look at some examples.

    listWithoutKeys= KeysDict.empty []--no Uniqueness

    uniqueInCasedLetter=
      []
      |>unique .inAlphabet
      |>unique .lowercase
      |>unique .uppercase
    
    letters=
      KeysDict.fromList uniqueInCasedLetter
        [ { inAlphabet= 0, lowercase= 'a', uppercase= 'A' }
        ]
-}
type Uniqueness value=
  TrueAtMostOnce
    (value ->value ->Bool)

{-| `accessKey` returns a value, which will be unique in a `KeysDict`.

    KeysDict.empty [ unique .lowercase, unique .uppercase ]
    |>KeysDict.putIn { lowercase= 'a', uppercase= 'A' }--put in
    |>KeysDict.putIn { lowercase= 'a', uppercase= 'B' }
      --checked values: .lowercase exists already! 
      -->KeysDict is unchanged
-}
unique: (value ->key) ->Uniqueness value
unique accessKey=
  TrueAtMostOnce
    (\value-> accessKey >>(==) (accessKey value))


{-| Using built-in (==) equality is often not useful in the context of association-dicts.

Do these 2 `KeysDict`s have the same size and identical values (ignoring insertion order)?

    letterCodes=
      KeysDict.fromList [ unique .letter, unique .code ]
        [ { letter= 'a', code= 97 }
        , { letter= 'b', code= 98 }
        ]
    fancyCompetingLetterCodes=
      KeysDict.empty [ unique .code, unique .letter ]
      |>KeysDict.putIn { code= 98, letter= 'b' }
      |>KeysDict.putIn { code= 97, letter= 'a' }
    
    KeysDict.equal
      letterCodes
      fancyCompetingLetterCodes
> `True`

    lettersAndCodesInAlphabet
-}
equal:
  KeysDict value ->KeysDict value
  ->Bool
equal twoKeyDictA=
  emptyOrMore
    { ifEmpty= isEmpty twoKeyDictA
    , ifMore=
        \value more->
          equal
            (twoKeyDictA |>remove identity value)
            more
    }

{-| A `KeysDict` with no values inside.
-}
empty: List (Uniqueness value) ->KeysDict value
empty keys=
  ValuesWithPromises keys []

{-| Create a `KeysDict` from a association-`Dict`.
If multiple equal keys or values are present, the value **first** in the `Dict` is **prefered** (see `putIn`).
    
    lowerToUpperLetters=
      AssocList.empty
      |>AssocList.insert 'a' 'A'
      |>AssocList.insert 'b' 'B'

    lowerUpperLetters=
      lowerToUpperLetters
      |>KeysDict.fromDict
          (\k v-> { lowercase= k, uppercase= v })
          [ unique .lowercase, unique .uppercase ]
-}
fromDict:
  (key ->dictValue ->value)
  ->List (Uniqueness value)
  ->AssocDict.Dict key dictValue
  ->KeysDict value
fromDict valueFromKeyValue keys=
  AssocDict.foldl
    (\k v-> putIn (valueFromKeyValue k v))
    (empty keys)

{-| Create a `KeysDict` _conveniently_ from values.
If right or left values are given multiple times, the value **first** in the `List` is **prefered** (see `putIn`).
    
    KeysDict.fromList [ unique .lowercase, unique .uppercase ]
      [ { lowercase= 'b', uppercase= 'B' } --put in
      , { lowercase= 'a', uppercase= 'A' } --put in
      , { lowercase= 'b', uppercase= 'C' }
          --ignored, as the left value already exists
      , { lowercase= 'c', uppercase= 'A' }
          --ignored, as the right value already exists
      , { lowercase= 'c', uppercase= 'C' } --put in
      ]
-}
fromList:
  List (Uniqueness value)
  ->List value ->KeysDict value
fromList=
  List.foldl putIn <<empty


{-| `Just` the value in which `key` is present in the `KeysDict`,
if no value with the `key` is found `Nothing`.

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

**Note**: If `accessKey` is not a key,
`access` will find the most recently inserted value where `accessKey` of the value is `key`.

    KeysDict.empty [ unique .lowercase, unique .uppercase ]
    |>KeysDict.putIn { rating= 0.5, lowercase= 'a', uppercase= 'A' }
    |>KeysDict.putIn { rating= 0.5, lowercase= 'b', uppercase= 'B' }
    |>KeysDict.access .rating 0.5
> `{ rating= 0.5, lowercase= 'b', uppercase= 'B' }`
-}
access:
  (value ->key) ->key
  ->KeysDict value
  ->Maybe value
access accessKey key=
  find (accessKey >>(==) key)


{-| `ifEmpty` if the `KeysDict` contains no values,
else `ifMore` with the most recently inserted value followed by a `KeysDict` with the other values.

It has a very similar use case to a `case of` on a `List`.

    isEmpty=
      KeysDict.emptyOrMore
        { ifEmpty= True
        , ifMore= \_ _-> False
        }
    mostRecent=
      KeysDict.emptyOrMore
        { ifMore= \value _-> Just value
        , ifEmpty= Nothing
        }
    removeMostRecent keysDict=
      keysDict
      |>KeysDict.emptyOrMore
          { ifMore= \_ rest-> rest
          , ifEmpty= KeysDict
          }
-}
emptyOrMore:
  { ifEmpty: result
  , ifMore:
      value ->KeysDict value ->result
  }
  ->KeysDict value ->result
emptyOrMore
  { ifEmpty, ifMore } (ValuesWithPromises keys valueList)
  =
  case valueList of
    []->
      ifEmpty
    
    value ::rest->
      ifMore value
        (ValuesWithPromises keys rest)

{-| **not exposed**, because the usage of `emptyOrMore` should be encouraged
-}
isEmpty: KeysDict value ->Bool
isEmpty=
  emptyOrMore
    { ifEmpty= True
    , ifMore= \_ _-> False
    }
      

{-| How many values there are in a `KeysDict`.

    KeysDict.fromList [ unique .number, unique .following ]
      (List.map (\i-> { number= i, following= i+1 })
        (List.range 0 41)
      )
    |>KeysDict.size
> `42`
-}
size: KeysDict value ->Int
size=
  List.length <<values


find:
  (value ->Bool) ->KeysDict value
  ->Maybe value
find isThere=
  emptyOrMore
    { ifEmpty= Nothing
    , ifMore=
        \value rest->
          case isThere value of
            True-> Just value
            False->
              find isThere rest
    }

isPresent:
  (value ->Bool) ->KeysDict value ->Bool
isPresent isThere=
  find isThere >>(/=) Nothing

keyPresent: value ->KeysDict value ->Bool
keyPresent value ((ValuesWithPromises keys _) as twoKeyDict)=
  keys
  |>List.map
      (\(TrueAtMostOnce equalKeys)->
        isPresent (equalKeys value) twoKeyDict
      )
  |>List.foldl (||) False

{-| Put in a value. This can't be a function or a json.

If a **key** is already **present**, the `KeysDict` is **unchanged**.

    KeysDict.empty [ unique .lowercase, unique .uppercase ]
    |>KeysDict.putIn { lowercase= 'b', uppercase= 'B', rating= 0.5 }
        --put in 
    |>KeysDict.putIn { lowercase= 'a', uppercase= 'A', rating= 0.5 }
        --put in, because rating is not a key
    |>KeysDict.putIn { lowercase= 'b', uppercase= 'C', rating= 0 }
        --ignored, the left value already exists
    |>KeysDict.putIn { lowercase= 'c', uppercase= 'A', rating= 0 }
        --ignored, the right value already exists
    |>KeysDict.putIn { lowercase= 'c', uppercase= 'C', rating= 0.6 }
        --put in
-}
putIn:
  value ->KeysDict value
  ->KeysDict value
putIn value keysDict=
  case keyPresent value keysDict of
    True->
      keysDict

    False->
      let
        (ValuesWithPromises keys valueList)= keysDict
      in
      ValuesWithPromises keys
        (value ::(values keysDict))


{-| Combine 2 `KeysDict`s, so that the values in `toInsert` are put into `preferred`.
If a value on the left or right is present, prefer the last `KeysDict` (see `putIn`).

    numberNamedOperators=
      KeysDict.fromList .name .operator
        [ { operator= '+', name= "plus" }
        , { operator= '-', name= "minus" }
        ]
    customNamedOperators=
      KeysDict.fromList .operator .name
        [ { operator= '∧', name= "and" }
        , { operator= '∨', name= "or" }
        , { operator= '-', name= "negate" }
        ]
    validNamedOperators=
      KeysDict.union
        custumNamedOperators --has a '-' left
        numberOperatorNames --preferred → its '-'-value is put in
-}
union:
  KeysDict value
  ->KeysDict value
  ->KeysDict value
union toInsert preferred=
  (fold putIn preferred) toInsert

{-| Reduce the values
from most recently putIned to least recently inserted.

A fold in the other direction doesn't exist, as association-`Dict`s should rarely rely on order (see `equal`).

    brackets=
      KeysDict.empty
      |>KeysDict.putIn ( '(', ')' )
      |>KeysDict.putIn ( '{', '}' )

    openingAndClosing=
      brackets
      |>KeysDict.fold
          (\( left, right ) acc->
            acc++[ String.fromList [ left, right ] ]
          )
          []
> `[ "{}", "()" ]`
-}
fold:
  (value ->acc ->acc)
  ->acc ->KeysDict value ->acc
fold reduce initial=
  values
  >>List.foldl reduce initial


{-| Remove the value at the `key`.
If **the key does not exist**, the `KeysDict` is **unchanged**

    openClosedBrackets=
      KeysDict.empty .open .closed
      |>KeysDict.putIn { open= "(", closed= ")" }

    openClosedBrackets
    |>KeysDict.remove .open ")" 
        --unchanged, ")" is not a open value
    |>KeysDict.remove .open "("
> `KeysDict.empty`

    openClosedBrackets
    |>KeysDict.remove .closed "("
        --unchanged, "(" is not a closed value
    |>KeysDict.remove .closed ")"
> `KeysDict.empty`

**Notice:** If you don't specify `accessValue` as `left` or `right`, it acts as a normal filter

    KeysDict.empty .open .closed
    |>KeysDict.putIn { open= "(", closed= ")", meaning= Nothing }
    |>KeysDict.putIn { open= "[", closed= "]", meaning= Just (List Element) }
    |>KeysDict.putIn { open= "<, closed= ">", meaning= Nothing }
    |>KeysDict.remove .meaning Nothing
-}
remove:
  (value ->key) ->key
  ->KeysDict value
  ->KeysDict value
remove accessKey key (ValuesWithPromises keys valueList)=
  ValuesWithPromises
    keys
    (List.filter
      (\value-> (/=) (accessKey value) key)
      valueList
    )


values: KeysDict value ->List value
values (ValuesWithPromises _ valueList)=
  valueList

{-| Map all values.

    digitNames=
      KeysDict.empty .number .name
      |>KeysDict.putIn { number= 0, name= "zero" }
      |>KeysDict.putIn { number= 1, name= "one" }

    mathSymbolNames=
      digitNames
      |>KeysDict.map .symbol .name
          (\{ number, name }->
            { symbol= String.fromInt number, name= name }
          )
      |>KeysDict.putIn { symbol= "+", name= "plus" }
-}
map:
  (value ->resultValue)
  ->List (Uniqueness resultValue)
  ->KeysDict value
  ->KeysDict resultValue
map alter resultKeys=
  empty resultKeys
  |>fold (alter >>putIn)


{-| Convert a `KeysDict` to an association-`Dict`.

    casedLetters=
      KeysDict.fromList .lowercase .uppercase
        [ { uppercase= 'A', lowercase= 'a' }
        , { uppercase= 'B', lowercase= 'b' }
        ]
    lowerFromUppercase=
      KeysDict.toDict .uppercase .lowercase
        casedLetters
-}
toDict:
  (value ->dictKey) ->(value ->dictValue)
  ->KeysDict value
  ->AssocDict.Dict dictKey dictValue
toDict toKey toValue=
  fold
    (\value->
      AssocDict.insert
        (toKey value) (toValue value)
    )
    AssocDict.empty


{-to contributers:
  for encoding / decoding: https://ellie-app.com/bPXzkZZHwyQa1
-}

{-| Convert a `KeysDict` to a `Json.Encode.Value`.

    someKeysDict=
      KeysDict.empty
      |>KeysDict.putIn ( 1, 11 )
      |>KeysDict.putIn ( 2, 22 )
    Encode.encode 1
      (KeysDict.encode
        Encode.int Encode.int
        someKeysDict
      )

    """
    [
     {
      \"left\": 2,
      \"right\": 22
     },
     {
      \"left\": 1,
      \"right\": 11
     }
    ]
    """
-}
encode:
  (value ->Encode.Value)
  ->KeysDict value ->Encode.Value
encode encodeValue=
  values
  >>Encode.list encodeValue

{-| A `Json.Decode.Decoder` for `KeysDict`s encoded by `encode`.

The order of insertion is not reconstructed (see `equal`)

    type alias NamedNumber=
      { number: Int
      , name: String
      }
    
    decodeNamedNumber=
      Decode.map NamedNumber
        (\{ number, name }->
          Decode.object
            [ ( "number", Decode.int number )
            , ( "name", Decode.string name )
            ]
        )

    """
    [
     {
      \"left\": 2,
      \"right\": "two"
     },
     {
      \"left\": 1,
      \"right\": "one"
     }
    ]
    """
    |>Decode.decodeString
        (KeysDict.decode [ unique .number, unique .name ]
          decodeNamedNumber
        )

> `Ok (ValuesWithKeys [ { number= 1, name= "one" }, { number= 2, name= "two" } ])`
> = a `KeysDict`
-}
decode:
  List (Uniqueness value)
  ->Decoder value
  ->Decoder (KeysDict value)
decode keys decodeValue=
  Decode.map (fromList keys)
    (Decode.list decodeValue)

