## 1.0.1

- update ellie examples

## 2.0.0

#### breaking changes

- split `Uniqueness`, `unique` off of `KeysDict` into `KeysDict.Uniqueness`

#### changes to edit case by case

- remove `fromDict` & `fromList` & `union`: use folds instead
- remove `emptyOrMore`: most code will work with a simple `case houses keysDict of`
- replace `encode` & `decode` with `serialize` which returns a `Codec`
- replace `remove` with `tearDown` which takes `{ door, key }`
- replace `access` with `enterBy` which takes `{ door, key }`
- change `toDict` to take `{ key, value }`

#### just a quick rename

- rename `empty` to `accessibleThrough`
- rename `values` to `houses`
- rename `putIn` to `putUp`
- rename `size` to `countHouses`
- move `unique` into `KeysDict.Uniqueness` renamed as `door`

general reasons:
- remove verboseness of `DataStructureX.commonName` by making a method `uniqueName` easily exposable
- remove abstractness by painting a picture: houses with doors and keys that are being matched against
- simplify: combine, etc
