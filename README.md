# KeysDict
Have many keys to lookup values.

Let's compare


### normal `Dict`

> You want the `🏠` where the `🔑` is `1`?

        < 🔑= 0, 🏠= 🌳 |
      → < 🔑= 1, 🏠= 🍐 |
        < 🔑= 2, 🏠= 🍐 |

        (🔑 is unique)

> Going through while comparing your `🔑`.

        🍐 where `🔑` is `1`

### `KeysDict`

> You want the pair where `🗝️` is `1` and the pair where `🔑` is `0`?

      → < 🔑= 0, 🏠= 🍐, 🗝️= 2 >
        < 🔑= 2, 🏠= 🌳, 🗝️= 0 >
        < 🔑= 1, 🏠= 🍐, 🗝️= 1 > ←

        (🔑 & 🗝️ are unique)

> Going through while checking every pair, if `🗝️` is equal, then, if `🔑` is equal.

        🔑= 1, 🗝️= 1 where 🗝️ is 1    🔑= 0, 🗝️= 2 where 🔑 is 0

&nbsp;


## 👍 How to `KeysDict`

try the [ellie for some examples](https://ellie-app.com/bSmccsw3Rz2a1) (always a version behind)

## Example: cased letters
```elm
type alias CasedLetter=
  { lowercase: Char
  , uppercase: Char
  }

lowerUppercaseLetters: KeysDict CasedLetter
lowerUppercaseLetters=
  KeysDict.empty [ unique .lowercase, unique .uppercase ]
  |>KeysDict.putIn { lowercase= 'a', uppercase= 'A' }
  |>KeysDict.putIn { lowercase= 'b', uppercase= 'B' }
  |>KeysDict.putIn { lowercase= 'c', uppercase= 'C' }

uppercase char=
  KeysDict.access .lowercase char
    lowerUppercaseLetters
  |>Maybe.map .uppercase
```

## Example: periodic table

```elm
type Element=
  Hydrogen
  | Helium

elementAtomicNumberKeysDict=
  KeysDict.fromList
    [ unique .atomicNumber, unique .element ]
    [ { element= Hydrogen, atomicNumber= 1 }
    , { element= Helium, atomicNumber= 2 }
    ]

atomicNumberByElement=
  KeysDict.toDict .element .atomicNumber
    elementAtomicNumberKeysDict
```

## Example: brackets
You have pairs that belong together:
```elm
brackets=
  KeysDict.empty [ unique .opening, unique .closing ]
  |>KeysDict.putIn { opening= '(', closing= ')' }
  |>KeysDict.putIn { opening= '{', closing= '}' }

typeChar character=
  case KeysDict.access .open character brackets of
    Just { closed }->
      String.fromList [ character, closed ]

    Nothing->
      case KeysDict.access .closed character brackets of
        Just \{ open }->
          String.fromList [ open, character ]
          
        Nothing->
          String.fromChar character

"Typing (: " ++(typeChar '(') ++". Even }: " ++(typeChar '}')
```
&nbsp;


## 👎 How not to `KeysDict`

## Example: automatic answers
```elm
answers=
  KeysDict.fromList [ unique .youSay ]
    [ { youSay= "Hi", answer= "Hi there!" }
    , { youSay= "Bye", answer=  "Ok, have a nice day and spread some love." }
    , { youSay= "How are you", answer= "I don't have feelings :(" }
    , { youSay= "Are you a robot"
      , answer= "I think the most human answer is 'Haha... yes'"
      }
    ]
```
please use a `Dict` where it is more appropriate: **`Dict`s are for one-way access**

## Example: translation, synonymes...
```elm
englishGerman=
  KeysDict.fromList []
    [ { english= "elm", german= "Ulme" }
    , { english= "git", german= "Schwachkopf" }
    , { german= "Rüster", english= "elm" }
    ]
```
A `KeysDict` is only effective, when there is **only one unique key**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict) instead!

## Example: partners, opposites...

Similar to the previous example:
```elm
partners=
  KeysDict.empty [ unique .partner, unique .partnerOfPartner ]
  |>KeysDict.putIn { partner= "Ann", partnerOfPartner= "Alan" }
  |>KeysDict.putIn { partner= "Alex", partnerOfPartner= "Alastair" }
  |>KeysDict.putIn { partner= "Alan", partnerOfPartner= "Ann" }
      --wait, this is no duplicate and is inserted
```
A `KeysDict` ony makes sense, when the **keys describe something different**.
