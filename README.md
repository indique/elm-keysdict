# MultiDict
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

### `MultiDict`

> You want the pair where `🗝️` is `1` and the pair where `🔑` is `0`?

      → < 🔑= 0, 🏠= 🍐, 🗝️= 2 >
        < 🔑= 2, 🏠= 🌳, 🗝️= 0 >
        < 🔑= 1, 🏠= 🍐, 🗝️= 1 > ←

        (🔑 & 🗝️ are unique)

> Going through while checking every pair, if `🗝️` is equal, then, if `🔑` is equal.

        🔑= 1, 🗝️= 1 where 🗝️ is 1    🔑= 0, 🗝️= 2 where 🔑 is 0

&nbsp;


## 👍 How to `MultiDict`

try the [ellie for some examples](https://ellie-app.com/bSmccsw3Rz2a1) (always a version behind)

## Example: cased letters
```elm
type alias CasedLetter=
  { lowercase: Char
  , uppercase: Char
  }

lowerUppercaseLetters: MultiDict CasedLetter
lowerUppercaseLetters=
  MultiDict.empty [ unique .lowercase, unique .uppercase ]
  |>MultiDict.putIn { lowercase= 'a', uppercase= 'A' }
  |>MultiDict.putIn { lowercase= 'b', uppercase= 'B' }
  |>MultiDict.putIn { lowercase= 'c', uppercase= 'C' }

uppercase char=
  MultiDict.access .lowercase char
    lowerUppercaseLetters
  |>Maybe.map .uppercase
```

## Example: periodic table

```elm
type Element=
  Hydrogen
  | Helium

elementAtomicNumberMultiDict=
  MultiDict.fromValues
    [ unique .atomicNumber, unique .element ]
    [ { element= Hydrogen, atomicNumber= 1 }
    , { element= Helium, atomicNumber= 2 }
    ]

atomicNumberByElement=
  MultiDict.toDict .element .atomicNumber
    elementAtomicNumberMultiDict
```

## Example: brackets
You have pairs that belong together:
```elm
brackets=
  MultiDict.empty [ unique .opening, unique .closing ]
  |>MultiDict.putIn { opening= '(', closing= ')' }
  |>MultiDict.putIn { opening= '{', closing= '}' }

typeChar character=
  case MultiDict.access .open character brackets of
    Just { closed }->
      String.fromValues [ character, closed ]

    Nothing->
      case MultiDict.access .closed character brackets of
        Just \{ open }->
          String.fromValues [ open, character ]
          
        Nothing->
          String.fromChar character

"Typing (: " ++(typeChar '(') ++". Even }: " ++(typeChar '}')
```
&nbsp;


## 👎 How not to `MultiDict`

## Example: automatic answers
```elm
answers=
  MultiDict.fromValues [ unique .youSay ]
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
  MultiDict.fromValues []
    [ { english= "elm", german= "Ulme" }
    , { english= "git", german= "Schwachkopf" }
    , { german= "Rüster", english= "elm" }
    ]
```
A `MultiDict` is only effective, when there is **only one unique key**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict) instead!

## Example: partners, opposites...

Similar to the previous example:
```elm
partners=
  MultiDict.empty [ unique .partner, unique .partnerOfPartner ]
  |>MultiDict.putIn { partner= "Ann", partnerOfPartner= "Alan" }
  |>MultiDict.putIn { partner= "Alex", partnerOfPartner= "Alastair" }
  |>MultiDict.putIn { partner= "Alan", partnerOfPartner= "Ann" }
      --wait, this is no duplicate and is inserted
```
A `MultiDict` ony makes sense, when the **keys describe something different**.
