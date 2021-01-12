module KeysDict.Uniqueness exposing
  ( Uniqueness, door
  , violated
  )
{-|
@docs Uniqueness, door

## just for extending core functionality
@docs violated
-}

import Util exposing (aspect)


{-| A promise, that there are never 2 houses with equal keys for the same door.

    listWithoutKeys=
      KeysDict.enterableBy 
        []--no promised Uniqueness
    
    doorsInCasedLetter=
      [ door .inAlphabet
      , door .lowercase
      , door .uppercase
      ]
    
    letters=
      KeyssDict.enterableBy doorsInCasedLetter
      |>putUp { inAlphabet= 0, lowercase= 'a', uppercase= 'A' }
      |>putUp { inAlphabet= 0, lowercase= 'α', uppercase= 'Α' }
        --rejected .inAlphabet key 0 is the same!
        -- → The KeysDict doesn't change
-}
type Uniqueness house=
  NeverTrue (EqualInGivenAspect house)

{-| **Not exposed.** Left out as an implementation detail.
-}
type alias EqualInGivenAspect house=
  house ->house ->Bool


{-| What's a door? It's a spot to `access` a house if you have a matching key.

Give `door` one aspect of a house, and it will be ensured that this aspect of a house unique for all houses.

    KeysDict.enterableBy
      [ door .lowercase, door .uppercase ]
    |>putUp { lowercase= 'a', uppercase= 'A' }--put up
    |>putUp { lowercase= 'a', uppercase= 'B' }
      --checked items: .lowercase key already exists! 
      --→ KeysDict is unchanged
-}
door: (house ->keyNeeded) ->Uniqueness house
door doorInHouse=
  NeverTrue (aspect doorInHouse (==))

{-| Do 2 values fail to fulfill `Uniqueness`?

    door .name
    |>violated
        { name= "smile", symbol= '😊' }
        { symbol= '🙂', name= "smile" }
    --> True
-}
violated: house ->house ->Uniqueness house ->Bool
violated aHouse bHouse (NeverTrue equalInAnAspect)=
  equalInAnAspect aHouse bHouse

