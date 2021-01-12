module KeysDict exposing
  ( KeysDict
  , equal
  , enterableBy
  , enterBy
  , countHouses
  , putUp, tearDown
  , foldHouses, map
  , houses, toDict
  , serialize
  )
{-|
# KeysDict
Have many keys to lookup values.

Let's compare


### normal `Dict`

> You want the value where the key is `1`?

        0 :  🏡
    --> 1 :  🏚
        2 :  🏚

    the key is unique among all items

> Going through while comparing your key.

    the value is 🏚  where the key is `1`

### `KeysDict`

First of all, get used to some names

- a "house" contains aspects that make it unique & aspects that can be shared across all houses
```elm
keepInParserPipelineOperator=
  { operator= "|=" --no operator overloading wanted!
  , origin= ElmRepoPackage "parser" --is the same for |.
  }
```
- a "door" describes a unique aspect across all houses.
```elm
door .operator
--is a promise, that the operator value is never the same for two houses.
```
So... If you have a key and the type of door it could match, you can find the only matching house and enter.

> You want to enter the house where `🗝️` is `1`?

        🔑= 0, 🏠= 🏚, 🗝️= 2
        🔑= 2, 🏠= 🏡, 🗝️= 0
        🔑= 1, 🏠= 🏚, 🗝️= 1 <--

    🔑, 🗝️: doors you can "open" with a key unique across all houses

> Going through while checking every house, if the `🗝️` matches.

        🔑= 1, 🏠= 🏚, 🗝️= 1  where 🗝️ is 1   
        
> You want to enter the house where `🔑` is `0`?

    --> 🔑= 0, 🏠= 🏚, 🗝️= 2
        🔑= 2, 🏠= 🏡, 🗝️= 0
        🔑= 1, 🏠= 🏚, 🗝️= 1

    🔑, 🗝️: doors you can "open" with a key unique across all houses

> Going through while checking every house, if the `🔑` matches.

        🔑= 0, 🏠= 🏚, 🗝️= 2  where 🔑 is 0

@docs KeysDict

## create
@docs enterableBy

## scan
@docs equal, enterBy, countHouses

## put up & tear down houses
@docs putUp, tearDown

## shape
@docs foldHouses, map, houses, toDict

## extra
@docs serialize
-}

import AssocList
import KeysDict.Uniqueness as Uniqueness exposing (Uniqueness)

import Serialize exposing (Codec)

import Util exposing
  ( aspect, firstWhere, any
  , equalIgnoringOrder
  )


{-| Unsorted data structure, which promises `Uniqueness` for multiple doors.

This allows for a special kind of access:

> one of many keys -> one of many doors -> house (containing the key you searched by).

Read the module doc for more!

    type alias CasedLetter=
      { lowercase: Char
      , uppercase: Char
      }

    casedLetters: KeysDict CasedLetter
    casedLetters=
      KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
      |>putUp { lowercase= 'a', uppercase= 'A' }
      |>putUp { lowercase= 'b', uppercase= 'B' }
      |>putUp { lowercase= 'c', uppercase= 'C' }

    uppercase char=
      lowerUppercaseLetters
      |>enterBy { door= .lowercase, key= char }
      |>Maybe.map .uppercase
-}
type KeysDict house=
  HousesWithUniquenessPromises
    (List (Uniqueness house)) (Houses house)

{-| **Should not be exposed.** Hides the underlying data structure.
-}
type alias Houses house=
  List house


{-| How can you know if each house in `aKeysDict` can also be found in `bKeysDict`?

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

    (==) letterCodes fancyCompetingLetterCodes
    --> elm crashes

Because a `KeysDict`'s `Uniqueness` is defined as functions.

    (==)
      (houses letterCodes)
      (houses fancyCompetingLetterCodes)
    --> False

Even though both contain the same houses but in a different order.

### take away

> Don't use `==` to compare `KeysDict`s

> With non-comparable types, thinking about order doesn't make much sense.
> You shouldn't rely on it when using functions like folds or `houses`.

Instead, you should use

    KeysDict.equal
      letterCodes
      fancyCompetingLetterCodes
    --> True
_Houses must not contain functions or json. Elm will crash trying to see if they are equal._
-}
equal:
  KeysDict house ->KeysDict house
  ->Bool
equal=
  (aspect houses) equalIgnoringOrder

{-| A `KeysDict` with no houses inside, promising that given aspects are unique across all houses (see `Uniqueness`).
-}
enterableBy: List (Uniqueness house) ->KeysDict house
enterableBy uniqueness=
  HousesWithUniquenessPromises uniqueness []


{-| `Just` the house where `door` of the house matches the `key`;
if no such house is found, `Nothing`.

    casedLetters=
      KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
      |>putUp { lowercase= 'a', uppercase= 'A' }
      |>putUp { lowercase= 'b', uppercase= 'B' }

    lowercase char=
      casedLetters
      |>enterBy { door= .uppercase, key= char }
      |>Maybe.map .lowercase

    uppercase char=
      casedLetters
      |>enterBy { door= .lowercase, key= char }
      |>Maybe.map .uppercase

**Note**: If keys of `door` aren't promised to be unique,
`enterBy` will find the most recently inserted house where `door` of the house matches the `key`.

    ratedCasedLetters=
      KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
      |>putUp { rating= 0.5, lowercase= 'a', uppercase= 'A' }
      |>putUp { rating= 0.5, lowercase= 'b', uppercase= 'B' }
    
    enterBy { door= .rating, key= 0.5 } ratedCasedLetters
    --> { rating= 0.5, lowercase= 'b', uppercase= 'B' }
-}
enterBy:
  { door: house ->key, key: key }
  ->KeysDict house
  ->Maybe house
enterBy { door, key }=
  houses >>firstWhere (door >>(==) key)

{-| How many houses there are.

    List.foldl putIn
      (KeysDict.enterableBy
        [ door .number, door .following ]
      )
      (List.map (\i-> { number= i, following= i +1 })
        (List.range 0 41)
      )
    |>countHouses
    --> 42
-}
countHouses: KeysDict house ->Int
countHouses=
  houses >>List.length

isntUnique: house ->KeysDict house ->Bool
isntUnique house=
  \(HousesWithUniquenessPromises keys houseList)->
    keys
    |>List.map
        (\uniqueness->
          any
            (\bHouse->
              Uniqueness.violated house bHouse uniqueness
            )
            houseList
        )
    |>List.foldl (||) False

{-| Put a house into `keysDict`. _Keys must not contain functions or json. Elm will crash trying to see if they match._

If there is already a house with the same **key** is already **present**, (see `Uniqueness`), the `KeysDict` remains **unchanged**.

    KeysDict.enterableBy
      [ door .lowercase, door .uppercase ]
    |>putUp { lowercase= 'b', uppercase= 'B', rating= 0.5 }
        --put up 
    |>putUp { lowercase= 'a', uppercase= 'A', rating= 0.5 }
        --put up, because rating is not a key
    |>putUp { lowercase= 'b', uppercase= 'C', rating= 0 }
        --ignored, the .lowercase already exists
    |>putUp { lowercase= 'c', uppercase= 'A', rating= 0 }
        --ignored, the .uppercase already exists
    |>putUp { lowercase= 'c', uppercase= 'C', rating= 0.6 }
        --put up
-}
putUp:
  house ->KeysDict house
  ->KeysDict house
putUp house keysDict=
  case isntUnique house keysDict of
    True->
      keysDict

    False->
      updateHouses
        (\houseList-> house ::houseList)
        keysDict

updateHouses:
  (List house ->List house)
  ->KeysDict house ->KeysDict house
updateHouses update=
  \(HousesWithUniquenessPromises keys houseList)->
    HousesWithUniquenessPromises keys
      (update houseList)

{-| Reduce the houses from most recently to least recently inserted house.

> With non-comparable types, thinking about order doesn't make much sense.
> You shouldn't rely on it when using functions like folds or `houses`.

    brackets=
      KeysDict.enterableBy
        [ door .open, door .closed ]
      |>putUp { open= '(', closed= ')' }
      |>putUp { open= '{', closed= '}' }

    openingAndClosing=
      brackets
      |>foldHouses
          (\{ open, closed }->
            (::) (String.fromList [ open, closed ])
          )
          []
    --> []
    --> |>(::) (String.fromList [ '{' '}' ])
    --> |>(::) (String.fromList [ '(' ')' ])
-}
foldHouses:
  (house ->acc ->acc)
  ->acc ->KeysDict house ->acc
foldHouses reduce initial=
  houses
  >>List.foldl reduce initial


{-| Remove the house where `door` of the house matches the `key`.
If **the key does not exist**, the `KeysDict` is **unchanged**

    openClosedBrackets=
      KeysDict.enterableBy
        [ door .open, door .closed ]
      |>putUp { open= "(", closed= ")" }

    openClosedBrackets
    |>tearDown { door= .open, key= ")" }
        --unchanged, ")" does not exist as a .open key
    |>tearDown { door= .open, key= "(" }
    --> KeysDict.enterableBy
    -->   [ door .open, door .closed ]

    openClosedBrackets
    |>tearDown { door= .closed, key= "(" }
        --unchanged, "(" does not exist as a .closed key
    |>tearDown { door= .closed, key= ")" }
    --> KeysDict.enterableBy
    -->   [ door .open, door .closed ]

**Notice:** If there is no promise of `Uniqueness` for `door`, `remove` acts as a normal filter.

    KeysDict.enterableBy
      [ door .open, door .closed ]
    |>putUp { open= "(", closed= ")", meaning= Nothing }
    |>putUp { open= "[", closed= "]", meaning= Just List }
    |>putUp { open= "<, closed= ">", meaning= Nothing }
    |>tearDown { door= .meaning, key= Nothing }

    --> KeysDict.enterableBy
    -->   [ door .open, door .closed ]
    --> |>putUp { open= "[", closed= "]", meaning= Just List }
-}
tearDown:
  { door: house ->key, key: key }
  ->KeysDict house ->KeysDict house
tearDown { door, key }=
  updateHouses
    (List.filter
      (\house-> (/=) key (door house))
    )


{-| The `List` containing all houses from most recently (= head) to least recently inserted house.

> With non-comparable types, thinking about order doesn't make much sense.
> You shouldn't rely on it when using functions like folds or `houses`.

    isEmpty=
      List.isEmpty <<houses

    mostRecentlyInserted=
      List.head <<houses
        
    removeMostRecentlyInserted keysDict=
      case houses keysDict of
        _ ::rest->
          rest

        []->
          keysDict
-}
houses: KeysDict house ->List house
houses=
  \(HousesWithUniquenessPromises _ houseList)->
    houseList

{-| Map all houses.

    digitNames=
      KeysDict.enterableBy
        [ door .number, door .name ]
      |>putUp { number= 0, name= "zero" }
      |>putUp { number= 1, name= "one" }

    mathSymbolNames=
      digitNames
      |>mapHouses [ door .symbol, door .name ]
          (\{ number, name }->
            { symbol= String.fromInt number, name= name }
          )
      |>putUp { symbol= "+", name= "plus" }
-}
map:
  (house ->resultHouse)
  ->List (Uniqueness resultHouse)
  ->KeysDict house
  ->KeysDict resultHouse
map alter resultUniqueness=
  enterableBy resultUniqueness
  |>foldHouses (alter >>putUp)


{-| Convert a `KeysDict` to an association-`Dict`.

    casedLetters=
      KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
      |>putUp { uppercase= 'A', lowercase= 'a' }
      |>putUp { uppercase= 'B', lowercase= 'b' }
    
    lowerFromUppercase=
      KeysDict.toDict { key= .uppercase, value= .lowercase }
        casedLetters
-}
toDict:
  { key: house ->key, value: house ->value }
  ->KeysDict house
  ->AssocList.Dict key value
toDict { key, value }=
  foldHouses
    (\house->
      AssocList.insert (key house) (value house)
    )
    AssocList.empty

{-| [Serialize](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/Serialize) a `KeysDict`.

    type alias NamedNumber=
      { name: String
      , number: Int
      }
    
    namedNumberCodec=
      Serialize.record NamedNumber
      |>Serialize.field .name Decode.string
      |>Serialize.field .number Decode.int
      |>Serialize.finishRecord

    namedNumberKeysDictCodec=
      KeysDict.codec namedNumberCodec
        [ door .number, door .name ]
    
    someKeysDict=
      KeysDict.enterableBy
        [ door .number, door .name ]
      |>putUp { number= 1, name= "one" }
      |>putUp { number= 2, name= "two" }

    Serialize.encodeToJson
      (NamedNumberKeyDictCodec someKeysDict)
    |>Json.Encode.encode 1

    --> """
    --> [
    -->  {
    -->   \"number\": 2,
    -->   \"name\": "two"
    -->  },
    -->  {
    -->   \"number\": 1,
    -->   \"name\": "one"
    -->  }
    --> ]
    --> """

    """
    [
     {
      \"number\": 2,
      \"name\": "two"
     },
     {
      \"number\": 1,
      \"name\": "one"
     }
    ]
    """
    |>Json.Decode.decodeString
        (Serialize.decodeFromJson
          namedNumberKeysDictCodec
        )

    --> Ok
    -->   (KeysDict.enterableBy
    -->     [ door .number, door .name ]
    -->   |>putUp { number= 1, name= "one" }
    -->   |>putUp { number= 2, name= "two" }
    -->   )
-}
serialize:
  Codec customError house
  ->List (Uniqueness house)
  ->Codec customError (KeysDict house)
serialize serializeHouse uniqueness=
  Serialize.map
    (List.foldl putUp (enterableBy uniqueness))
    houses
    (Serialize.list serializeHouse)
