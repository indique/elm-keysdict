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
try in the [ellie for the example cased letters](https://ellie-app.com/bQtcqGFXrgza1)

## Example: periodic table

```elm
type Element=
  Hydrogen
  | Helium

elementAtomicNumberKeysDict=
  KeysDict.fromList [ unique .atomicNumber, unique .element ]
    [ { element= Hydrogen, atomicNumber= 1 }
    , { element= Helium, atomicNumber= 2 }
    ]

atomicNumberByElement=
  KeysDict.toDict [ unique .element, unique .atomicNumber ]
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
  case brackets |>KeysDict.access .open character of
    Just { closed }->
      String.fromList [ character, closed ]

    Nothing->
      case brackets |>KeysDict.access .closed character
        of
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
  KeysDict.fromList .youSay .answer
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
  KeysDict.fromList .english .german
    [ { english= "elm", german= "Ulme" }
    , { english= "git", german= "Schwachkopf" }
    , { german= "Rüster", english= "elm" }
    ]
```
A right → left and backwards relationship is only fitting,
when **left or right don't have multiple translations**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict)

## Example: partners, opposites...

Similar to the previous example:
```elm
partners=
  KeysDict.empty
  |>KeysDict.putIn { partner= "Ann", partnerOfPartner= "Alan" }
  |>KeysDict.putIn { partner= "Alex", partnerOfPartner= "Alastair" }
  |>KeysDict.putIn { partner= "Alan", partnerOfPartner= "Ann" }
      --wait, this is no duplicate and gets putIned?
```
A `KeysDict` ony makes sense, when the **left & right sides describe something different**.
