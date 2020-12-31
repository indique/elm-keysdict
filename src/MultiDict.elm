module MultiDict exposing
  ( MultiDict
  , Uniqueness, unique
  , equal
  , empty, fromItems
  , access, emptyOrMore, size
  , insert, map
  , remove
  , fold
  , union
  , items, toDict
  , encode, decode
  )
{-|
@docs MultiDict

@docs Uniqueness, unique

## create
@docs empty, fromDict, fromItems, decode

## scan
@docs equal, access, emptyOrMore, size

## in
@docs insert, union

## out
@docs remove

## shape
@docs fold, map, items, toDict, encode
-}

import AssocList

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


{-| Want to look up items from many directions?

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
      |>MultiDict.insert { lowercase= 'a', uppercase= 'A' }
      |>MultiDict.insert { lowercase= 'b', uppercase= 'B' }
      |>MultiDict.insert { lowercase= 'c', uppercase= 'C' }

    uppercase char=
      MultiDict.access .lowercase char lowerUppercaseLetters
      |>Maybe.map .uppercase
-}
type MultiDict item=
  ItemsWithPromises
    (List (Uniqueness item)) (List item)

{-| Ensures, that there is not more than one item in the `MultiDict`, where a given condition is `True` (e.g. a given key being equal to a item)

That sounds a bit complicated, so let's look at some examples.

    listWithoutKeys= MultiDict.empty []--no Uniqueness

    uniqueInCasedLetter=
      [ unique .inAlphabet
      , unique .lowercase
      , unique .uppercase
      ]
    
    letters=
      MultiDict.fromItems uniqueInCasedLetter
        [ { inAlphabet= 0, lowercase= 'a', uppercase= 'A' }
        ]
-}
type Uniqueness item=
  TrueAtMostOnce
    (item ->item ->Bool)

{-| `accessKey` returns a item, which will be unique in a `MultiDict`.

    MultiDict.empty [ unique .lowercase, unique .uppercase ]
    |>MultiDict.insert { lowercase= 'a', uppercase= 'A' }--put in
    |>MultiDict.insert { lowercase= 'a', uppercase= 'B' }
      --checked items: .lowercase exists already! 
      -->MultiDict is unchanged
-}
unique: (item ->key) ->Uniqueness item
unique accessKey=
  TrueAtMostOnce
    (\item-> accessKey >>(==) (accessKey item))


{-| How can you know if each item in `aMultiDict` can also be found in `bMultiDict`?

    letterCodes=
      MultiDict.fromItems
        [ unique .letter, unique .code ]
        [ { letter= 'a', code= 97 }
        , { letter= 'b', code= 98 }
        ]
    fancyCompetingLetterCodes=
      MultiDict.empty [ unique .code, unique .letter ]
      |>MultiDict.insert { code= 98, letter= 'b' }
      |>MultiDict.insert { code= 97, letter= 'a' }

    (==) letterCodes fancyCompetingLetterCodes
> elm crashes

Because a `MultiDict`-`Uniqueness` is defined as a function.

    (==) (MultiDict.items letterCodes)
      (MultiDict.items fancyCompetingLetterCodes)

> `False`

Even though both contain the same items but in a different order.

#### Summary

- **Don't use `==` to compare `MultiDict`s**
- Using built-in (`==`) equality isn't helpful in the context of unsorted data structures.

    MultiDict.equal
      letterCodes
      fancyCompetingLetterCodes
> `True`

    lettersAndCodesInAlphabet
-}
equal:
  MultiDict item ->MultiDict item
  ->Bool
equal aMultiDict bMultiDict=
  bMultiDict
  |>emptyOrMore
      { ifEmpty= isEmpty aMultiDict
      , ifMore=
          \item->
            equal (remove identity item aMultiDict)
      }

{-| A `MultiDict` with no items inside and some promises for uniqueness (see `Uniqueness`).
-}
empty: List (Uniqueness item) ->MultiDict item
empty uniqueness=
  ItemsWithPromises uniqueness []

{-| Create a `MultiDict` _conveniently_ from a list of items & promises for uniqueness (see `Uniqueness`).
If an item later is't considered unique, the item **first** in the `List` is **prefered** (see `insert`).
    
    MultiDict.fromItems
      [ unique .lowercase, unique .uppercase ]
      [ { lowercase= 'b', uppercase= 'B' } --put in
      , { lowercase= 'a', uppercase= 'A' } --put in
      , { lowercase= 'b', uppercase= 'C' }
          --ignored, as a .lowercase item already exists
      , { lowercase= 'c', uppercase= 'A' }
          --ignored, as a .uppercase item already exists
      , { lowercase= 'c', uppercase= 'C' } --put in
      ]
-}
fromItems:
  List (Uniqueness item)
  ->List item ->MultiDict item
fromItems uniqueness=
  List.foldl insert (empty uniqueness)


{-| `Just` the item in which `key` is equal to `accessKey`,
if no item with the `key` is found `Nothing`.

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

**Note**: If `accessKey` isn't promised to be unique,
`access` will find the most recently inserted item where `accessKey` of the item is `key`.

    MultiDict.empty [ unique .lowercase, unique .uppercase ]
    |>MultiDict.insert { rating= 0.5, lowercase= 'a', uppercase= 'A' }
    |>MultiDict.insert { rating= 0.5, lowercase= 'b', uppercase= 'B' }
    |>MultiDict.access .rating 0.5
> `{ rating= 0.5, lowercase= 'b', uppercase= 'B' }`
-}
access:
  (item ->key) ->key
  ->MultiDict item
  ->Maybe item
access accessKey key=
  find (accessKey >>(==) key)


{-| `ifEmpty` if the `MultiDict` contains no items,
else `ifMore` with the most recently inserted item followed by a `MultiDict` with the previous items.

It has a very similar use case to a `case of` on a `List`.

    isEmpty=
      MultiDict.emptyOrMore
        { ifEmpty= True
        , ifMore= \_ _-> False
        }
    mostRecent=
      MultiDict.emptyOrMore
        { ifMore= \item _-> Just item
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
      item ->MultiDict item ->result
  }
  ->MultiDict item ->result
emptyOrMore { ifEmpty, ifMore }=
  \(ItemsWithPromises keys list)->
    case list of
      []->
        ifEmpty
      
      recent ::previous->
        ifMore recent
          (ItemsWithPromises keys previous)

{-| **not exposed**, because the usage of `emptyOrMore` should be encouraged
-}
isEmpty: MultiDict item ->Bool
isEmpty=
  emptyOrMore
    { ifEmpty= True
    , ifMore= \_ _-> False
    }
      

{-| How many items there are in a `MultiDict`.

    MultiDict.fromItems
      [ unique .number, unique .following ]
      (List.map (\i-> { number= i, following= i+1 })
        (List.range 0 41)
      )
    |>MultiDict.size
> `42`
-}
size: MultiDict item ->Int
size=
  items >>List.length


find:
  (item ->Bool) ->MultiDict item
  ->Maybe item
find isThere=
  emptyOrMore
    { ifEmpty= Nothing
    , ifMore=
        \item rest->
          case isThere item of
            True->
              Just item

            False->
              find isThere rest
    }

isPresent:
  (item ->Bool) ->MultiDict item ->Bool
isPresent isThere=
  find isThere >>(/=) Nothing

isntUnique: item ->MultiDict item ->Bool
isntUnique item=
  \((ItemsWithPromises keys _) as multiDict)->
    keys
    |>List.map
        (\(TrueAtMostOnce equalKeys)->
          isPresent (equalKeys item) multiDict
        )
    |>List.foldl (||) False

{-| Put an item into `multiDict`. _Items cannot contain functions or json_.

If the item is'nt unique (e.g. a **key** is already **present**, see `Uniqueness`), the `MultiDict` remains **unchanged**.

    MultiDict.empty [ unique .lowercase, unique .uppercase ]
    |>MultiDict.insert { lowercase= 'b', uppercase= 'B', rating= 0.5 }
        --put in 
    |>MultiDict.insert { lowercase= 'a', uppercase= 'A', rating= 0.5 }
        --put in, because rating is not a key
    |>MultiDict.insert { lowercase= 'b', uppercase= 'C', rating= 0 }
        --ignored, the .lowercase already exists
    |>MultiDict.insert { lowercase= 'c', uppercase= 'A', rating= 0 }
        --ignored, the .uppercase already exists
    |>MultiDict.insert { lowercase= 'c', uppercase= 'C', rating= 0.6 }
        --put in
-}
insert:
  item ->MultiDict item
  ->MultiDict item
insert item multiDict=
  case isntUnique item multiDict of
    True->
      multiDict

    False->
      multiDict
      |>\(ItemsWithPromises keys itemList)->
          ItemsWithPromises keys
            (item ::itemList)


{-| Combine 2 `MultiDict`s, so that the items in `toInsert` are put into `preferred`.
If an item isn't unique (see `Uniqueness`), prefer the last `MultiDict` (see `insert`).

    numberNamedOperators=
      MultiDict.fromItems
        [ unique .name, unique .operator ]
        [ { operator= '+', name= "plus" }
        , { operator= '-', name= "minus" }
        ]
    customNamedOperators=
      MultiDict.fromItems
        [ unique .operator, unique .name ]
        [ { operator= '∧', name= "and" }
        , { operator= '∨', name= "or" }
        , { operator= '-', name= "negate" }
        ]
    validNamedOperators=
      MultiDict.union
        custumNamedOperators --has a '-' .operator
        numberOperatorNames
        --preferred → uniqueness for .operator
        --→ custumNamedOperators's '-'-item is ignored
-}
union:
  MultiDict item
  ->MultiDict item
  ->MultiDict item
union toInsert preferred=
  (fold insert preferred) toInsert

{-| Reduce the items from most recently inserted to least recently inserted.

A fold in the other direction doesn't exist, as association-`Dict`s should rarely rely on order (see `equal`).

    brackets=
      MultiDict.empty [ unique .open, unique .closed ]
      |>MultiDict.insert { open= '(', closed= ')' }
      |>MultiDict.insert { open= '{', closed= '}' }

    openingAndClosing=
      brackets
      |>MultiDict.fold
          (\{ open, closed }->
            (::) (String.fromList [ open, closed ])
          )
          []
> `[ "()", "{}" ]`
-}
fold:
  (item ->acc ->acc)
  ->acc ->MultiDict item ->acc
fold reduce initial=
  items
  >>List.foldl reduce initial


{-| Remove the item at the `key`.
If **the key does not exist**, the `MultiDict` is **unchanged**

    openClosedBrackets=
      MultiDict.empty [ unique .open, unique .closed ]
      |>MultiDict.insert { open= "(", closed= ")" }

    openClosedBrackets
    |>MultiDict.remove .open ")" 
        --unchanged, ")" is not a open item
    |>MultiDict.remove .open "("
> `MultiDict.empty`

    openClosedBrackets
    |>MultiDict.remove .closed "("
        --unchanged, "(" is not a closed item
    |>MultiDict.remove .closed ")"
> `MultiDict.empty`

**Notice:** If there is no promise of `Uniqueness` for `accessKey`, `remove` acts as a normal filter.

    MultiDict.empty [ unique .open, unique .closed ]
    |>MultiDict.insert
        { open= "(", closed= ")", meaning= Nothing }
    |>MultiDict.insert
        { open= "[", closed= "]", meaning= Just (List Element) }
    |>MultiDict.insert
        { open= "<, closed= ">", meaning= Nothing }
    |>MultiDict.remove .meaning Nothing
-}
remove:
  (item ->key) ->key
  ->MultiDict item
  ->MultiDict item
remove accessKey key=
  \(ItemsWithPromises keys itemList)->
    ItemsWithPromises
      keys
      (List.filter
        (\item-> (/=) (accessKey item) key)
        itemList
      )


{-| The `List` containing all items from most recently (= head) to least recently inserted item.
-}
items: MultiDict item ->List item
items=
  \(ItemsWithPromises _ itemList)-> itemList

{-| Map all items.

    digitNames=
      MultiDict.empty [ unique .number, unique .name ]
      |>MultiDict.insert { number= 0, name= "zero" }
      |>MultiDict.insert { number= 1, name= "one" }

    mathSymbolNames=
      digitNames
      |>MultiDict.map .symbol .name
          (\{ number, name }->
            { symbol= String.fromInt number, name= name }
          )
      |>MultiDict.insert { symbol= "+", name= "plus" }
-}
map:
  (item ->resultItem)
  ->List (Uniqueness resultItem)
  ->MultiDict item
  ->MultiDict resultItem
map alter resultUniqueness=
  empty resultUniqueness
  |>fold (alter >>insert)


{-| Convert a `MultiDict` to an association-`Dict`.

    casedLetters=
      MultiDict.fromItems
        [ unique .lowercase, unique .uppercase ]
        [ { uppercase= 'A', lowercase= 'a' }
        , { uppercase= 'B', lowercase= 'b' }
        ]
    lowerFromUppercase=
      MultiDict.toDict .uppercase .lowercase
        casedLetters
-}
toDict:
  (item ->dictKey) ->(item ->dictValue)
  ->MultiDict item
  ->AssocList.Dict dictKey dictValue
toDict toKey toValue=
  fold
    (\item->
      AssocList.insert
        (toKey item) (toValue item)
    )
    AssocList.empty

{-| Convert a `MultiDict` to a `Json.Encode.Value`.

    someMultiDict=
      MultiDict.empty
      |>MultiDict.insert ( 1, 11 )
      |>MultiDict.insert ( 2, 22 )
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
  (item ->Encode.Value)
  ->MultiDict item ->Encode.Value
encode encodeValue=
  items
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

> `Ok (ItemsWithKeys [ { number= 1, name= "one" }, { number= 2, name= "two" } ])`
> = a `MultiDict`
-}
decode:
  List (Uniqueness item)
  ->Decoder item
  ->Decoder (MultiDict item)
decode keys decodeItem=
  Decode.map (fromItems keys)
    (Decode.list decodeItem)

