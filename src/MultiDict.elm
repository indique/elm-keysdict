module MultiDict exposing
  ( MultiDict
  , Uniqueness, unique
  , equal
  , empty, fromDict, fromValues
  , access, emptyOrMore, size
  , putIn, map
  , remove
  , fold
  , union
  , toDict
  , encode, decode
  )
{-|
@docs MultiDict

@docs Uniqueness, unique

## create
@docs empty, fromDict, fromValues, decode

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

Like [assoc-list](https://github.com/pzp1997/assoc-list), a `MultiDict` allows union types.

## Example: cased letters
    type alias CasedLetter=
      { lowercase: Char
      , uppercase: Char
      }

    casedLetters: MultiDict CasedLetter
    casedLetters=
      MultiDict.empty [ unique .lowercase, unique .uppercase ]
      |>MultiDict.putIn { lowercase= 'a', uppercase= 'A' }
      |>MultiDict.putIn { lowercase= 'b', uppercase= 'B' }
      |>MultiDict.putIn { lowercase= 'c', uppercase= 'C' }

    uppercase char=
      MultiDict.access .lowercase char lowerUppercaseLetters
      |>Maybe.map .uppercase
-}
type MultiDict value=
  ValuesWithPromises
    (List (Uniqueness value)) (List value)

{-| Ensures, that there is not more than one value in the `MultiDict`, where a given condition is `True` (e.g. a given key being equal to a value)

That sounds a bit complicated, so let's look at some examples.

    listWithoutKeys= MultiDict.empty []--no Uniqueness

    uniqueInCasedLetter=
      []
      |>unique .inAlphabet
      |>unique .lowercase
      |>unique .uppercase
    
    letters=
      MultiDict.fromValues uniqueInCasedLetter
        [ { inAlphabet= 0, lowercase= 'a', uppercase= 'A' }
        ]
-}
type Uniqueness value=
  TrueAtMostOnce
    (value ->value ->Bool)

{-| `accessKey` returns a value, which will be unique in a `MultiDict`.

    MultiDict.empty [ unique .lowercase, unique .uppercase ]
    |>MultiDict.putIn { lowercase= 'a', uppercase= 'A' }--put in
    |>MultiDict.putIn { lowercase= 'a', uppercase= 'B' }
      --checked values: .lowercase exists already! 
      -->MultiDict is unchanged
-}
unique: (value ->key) ->Uniqueness value
unique accessKey=
  TrueAtMostOnce
    (\value-> accessKey >>(==) (accessKey value))


{-| Using built-in (==) equality is often not useful in the context of association-dicts.

Do these 2 `MultiDict`s have the same size and identical values (ignoring insertion order)?

    letterCodes=
      MultiDict.fromValues [ unique .letter, unique .code ]
        [ { letter= 'a', code= 97 }
        , { letter= 'b', code= 98 }
        ]
    fancyCompetingLetterCodes=
      MultiDict.empty [ unique .code, unique .letter ]
      |>MultiDict.putIn { code= 98, letter= 'b' }
      |>MultiDict.putIn { code= 97, letter= 'a' }
    
    MultiDict.equal
      letterCodes
      fancyCompetingLetterCodes
> `True`

    lettersAndCodesInAlphabet
-}
equal:
  MultiDict value ->MultiDict value
  ->Bool
equal aMultiDict bMultiDict=
  bMultiDict
  |>emptyOrMore
      { ifEmpty= isEmpty aMultiDict
      , ifMore=
          \value->
            equal (remove identity value aMultiDict)
      }

{-| A `MultiDict` with no values inside and some promises for uniqueness (see `Uniqueness`).
-}
empty: List (Uniqueness value) ->MultiDict value
empty keys=
  ValuesWithPromises keys []

{-| Create a `MultiDict` from a association-`Dict`.
If multiple equal keys or values are present, the value **first** in the `Dict` is **prefered** (see `putIn`).
    
    lowerToUpperLetters=
      AssocList.empty
      |>AssocList.insert 'a' 'A'
      |>AssocList.insert 'b' 'B'

    lowerUpperLetters=
      lowerToUpperLetters
      |>MultiDict.fromDict
          (\k v-> { lowercase= k, uppercase= v })
          [ unique .lowercase, unique .uppercase ]
-}
fromDict:
  (key ->dictValue ->value)
  ->List (Uniqueness value)
  ->AssocDict.Dict key dictValue
  ->MultiDict value
fromDict valueFromKeyValue keys=
  AssocDict.foldl
    (\k v-> putIn (valueFromKeyValue k v))
    (empty keys)

{-| Create a `MultiDict` _conveniently_ from values & some promises for uniqueness (see `Uniqueness`).
If right or left values are given multiple times, the value **first** in the `List` is **prefered** (see `putIn`).
    
    MultiDict.fromValues
      [ unique .lowercase, unique .uppercase ]
      [ { lowercase= 'b', uppercase= 'B' } --put in
      , { lowercase= 'a', uppercase= 'A' } --put in
      , { lowercase= 'b', uppercase= 'C' }
          --ignored, as the left value already exists
      , { lowercase= 'c', uppercase= 'A' }
          --ignored, as the right value already exists
      , { lowercase= 'c', uppercase= 'C' } --put in
      ]
-}
fromValues:
  List (Uniqueness value)
  ->List value ->MultiDict value
fromValues=
  List.foldl putIn <<empty


{-| `Just` the value in which `key` is present in the `MultiDict`,
if no value with the `key` is found `Nothing`.

    casedLetters=
      MultiDict.empty [ unique .lowercase, unique .uppercase ]
      |>MultiDict.putIn { lowercase= 'a', uppercase= 'A' }
      |>MultiDict.putIn { lowercase= 'b', uppercase= 'B' }

    lowercase char=
      MultiDict.access .uppercase char
        casedLetters
      |>Maybe.map .lowercase
    uppercase char=
      MultiDict.access .lowercase char
        casedLetters
      |>Maybe.map .uppercase

**Note**: If `accessKey` is not a key,
`access` will find the most recently inserted value where `accessKey` of the value is `key`.

    MultiDict.empty [ unique .lowercase, unique .uppercase ]
    |>MultiDict.putIn { rating= 0.5, lowercase= 'a', uppercase= 'A' }
    |>MultiDict.putIn { rating= 0.5, lowercase= 'b', uppercase= 'B' }
    |>MultiDict.access .rating 0.5
> `{ rating= 0.5, lowercase= 'b', uppercase= 'B' }`
-}
access:
  (value ->key) ->key
  ->MultiDict value
  ->Maybe value
access accessKey key=
  find (accessKey >>(==) key)


{-| `ifEmpty` if the `MultiDict` contains no values,
else `ifMore` with the most recently inserted value followed by a `MultiDict` with the other values.

It has a very similar use case to a `case of` on a `List`.

    isEmpty=
      MultiDict.emptyOrMore
        { ifEmpty= True
        , ifMore= \_ _-> False
        }
    mostRecent=
      MultiDict.emptyOrMore
        { ifMore= \value _-> Just value
        , ifEmpty= Nothing
        }
    removeMostRecent MultiDict=
      MultiDict
      |>MultiDict.emptyOrMore
          { ifMore= \_ rest-> rest
          , ifEmpty= MultiDict
          }
-}
emptyOrMore:
  { ifEmpty: result
  , ifMore:
      value ->MultiDict value ->result
  }
  ->MultiDict value ->result
emptyOrMore { ifEmpty, ifMore }=
  \(ValuesWithPromises keys valueList)->
    case valueList of
      []->
        ifEmpty
      
      value ::rest->
        ifMore value
          (ValuesWithPromises keys rest)

{-| **not exposed**, because the usage of `emptyOrMore` should be encouraged
-}
isEmpty: MultiDict value ->Bool
isEmpty=
  emptyOrMore
    { ifEmpty= True
    , ifMore= \_ _-> False
    }
      

{-| How many values there are in a `MultiDict`.

    MultiDict.fromValues
      [ unique .number, unique .following ]
      (List.map (\i-> { number= i, following= i+1 })
        (List.range 0 41)
      )
    |>MultiDict.size
> `42`
-}
size: MultiDict value ->Int
size=
  values >>List.length


find:
  (value ->Bool) ->MultiDict value
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
  (value ->Bool) ->MultiDict value ->Bool
isPresent isThere=
  find isThere >>(/=) Nothing

keyPresent: value ->MultiDict value ->Bool
keyPresent value=
  \((ValuesWithPromises keys _) as multiDict)->
    keys
    |>List.map
        (\(TrueAtMostOnce equalKeys)->
          isPresent (equalKeys value) multiDict
        )
    |>List.foldl (||) False

{-| Put in a value. This can't be a function or a json.

If a **key** is already **present**, the `MultiDict` is **unchanged**.

    MultiDict.empty [ unique .lowercase, unique .uppercase ]
    |>MultiDict.putIn { lowercase= 'b', uppercase= 'B', rating= 0.5 }
        --put in 
    |>MultiDict.putIn { lowercase= 'a', uppercase= 'A', rating= 0.5 }
        --put in, because rating is not a key
    |>MultiDict.putIn { lowercase= 'b', uppercase= 'C', rating= 0 }
        --ignored, the left value already exists
    |>MultiDict.putIn { lowercase= 'c', uppercase= 'A', rating= 0 }
        --ignored, the right value already exists
    |>MultiDict.putIn { lowercase= 'c', uppercase= 'C', rating= 0.6 }
        --put in
-}
putIn:
  value ->MultiDict value
  ->MultiDict value
putIn value=
  \multiDict->
    case keyPresent value multiDict of
      True->
        multiDict

      False->
        let
          (ValuesWithPromises keys valueList)= multiDict
        in
        ValuesWithPromises keys
          (value ::(values multiDict))


{-| Combine 2 `MultiDict`s, so that the values in `toInsert` are put into `preferred`.
If a value on the left or right is present, prefer the last `MultiDict` (see `putIn`).

    numberNamedOperators=
      MultiDict.fromValues .name .operator
        [ { operator= '+', name= "plus" }
        , { operator= '-', name= "minus" }
        ]
    customNamedOperators=
      MultiDict.fromValues .operator .name
        [ { operator= '∧', name= "and" }
        , { operator= '∨', name= "or" }
        , { operator= '-', name= "negate" }
        ]
    validNamedOperators=
      MultiDict.union
        custumNamedOperators --has a '-' left
        numberOperatorNames --preferred → its '-'-value is put in
-}
union:
  MultiDict value
  ->MultiDict value
  ->MultiDict value
union toInsert preferred=
  (fold putIn preferred) toInsert

{-| Reduce the values
from most recently putIned to least recently inserted.

A fold in the other direction doesn't exist, as association-`Dict`s should rarely rely on order (see `equal`).

    brackets=
      MultiDict.empty
      |>MultiDict.putIn ( '(', ')' )
      |>MultiDict.putIn ( '{', '}' )

    openingAndClosing=
      brackets
      |>MultiDict.fold
          (\( left, right ) acc->
            acc++[ String.fromValues [ left, right ] ]
          )
          []
> `[ "{}", "()" ]`
-}
fold:
  (value ->acc ->acc)
  ->acc ->MultiDict value ->acc
fold reduce initial=
  values
  >>List.foldl reduce initial


{-| Remove the value at the `key`.
If **the key does not exist**, the `MultiDict` is **unchanged**

    openClosedBrackets=
      MultiDict.empty .open .closed
      |>MultiDict.putIn { open= "(", closed= ")" }

    openClosedBrackets
    |>MultiDict.remove .open ")" 
        --unchanged, ")" is not a open value
    |>MultiDict.remove .open "("
> `MultiDict.empty`

    openClosedBrackets
    |>MultiDict.remove .closed "("
        --unchanged, "(" is not a closed value
    |>MultiDict.remove .closed ")"
> `MultiDict.empty`

**Notice:** If you don't specify `accessValue` as `left` or `right`, it acts as a normal filter

    MultiDict.empty [ unique .open, unique .closed ]
    |>MultiDict.putIn
        { open= "(", closed= ")", meaning= Nothing }
    |>MultiDict.putIn
        { open= "[", closed= "]", meaning= Just (List Element) }
    |>MultiDict.putIn
        { open= "<, closed= ">", meaning= Nothing }
    |>MultiDict.remove .meaning Nothing
-}
remove:
  (value ->key) ->key
  ->MultiDict value
  ->MultiDict value
remove accessKey key=
  \(ValuesWithPromises keys valueList)->
    ValuesWithPromises
      keys
      (List.filter
        (\value-> (/=) (accessKey value) key)
        valueList
      )


values: MultiDict value ->List value
values=
  \(ValuesWithPromises _ valueList)-> valueList

{-| Map all values.

    digitNames=
      MultiDict.empty [ unique .number, unique .name ]
      |>MultiDict.putIn { number= 0, name= "zero" }
      |>MultiDict.putIn { number= 1, name= "one" }

    mathSymbolNames=
      digitNames
      |>MultiDict.map .symbol .name
          (\{ number, name }->
            { symbol= String.fromInt number, name= name }
          )
      |>MultiDict.putIn { symbol= "+", name= "plus" }
-}
map:
  (value ->resultValue)
  ->List (Uniqueness resultValue)
  ->MultiDict value
  ->MultiDict resultValue
map alter resultKeys=
  empty resultKeys
  |>fold (alter >>putIn)


{-| Convert a `MultiDict` to an association-`Dict`.

    casedLetters=
      MultiDict.fromValues .lowercase .uppercase
        [ { uppercase= 'A', lowercase= 'a' }
        , { uppercase= 'B', lowercase= 'b' }
        ]
    lowerFromUppercase=
      MultiDict.toDict .uppercase .lowercase
        casedLetters
-}
toDict:
  (value ->dictKey) ->(value ->dictValue)
  ->MultiDict value
  ->AssocDict.Dict dictKey dictValue
toDict toKey toValue=
  fold
    (\value->
      AssocDict.insert
        (toKey value) (toValue value)
    )
    AssocDict.empty

{-| Convert a `MultiDict` to a `Json.Encode.Value`.

    someMultiDict=
      MultiDict.empty
      |>MultiDict.putIn ( 1, 11 )
      |>MultiDict.putIn ( 2, 22 )
    Encode.encode 1
      (MultiDict.encode
        Encode.int Encode.int
        someMultiDict
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
  ->MultiDict value ->Encode.Value
encode encodeValue=
  values
  >>Encode.list encodeValue

{-| A `Json.Decode.Decoder` for `MultiDict`s encoded by `encode`.

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
        (MultiDict.decode [ unique .number, unique .name ]
          decodeNamedNumber
        )

> `Ok (ValuesWithKeys [ { number= 1, name= "one" }, { number= 2, name= "two" } ])`
> = a `MultiDict`
-}
decode:
  List (Uniqueness value)
  ->Decoder value
  ->Decoder (MultiDict value)
decode keys decodeValue=
  Decode.map (fromValues keys)
    (Decode.list decodeValue)

