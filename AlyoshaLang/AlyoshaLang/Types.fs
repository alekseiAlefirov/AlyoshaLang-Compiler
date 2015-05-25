module Types

type alyoshaType =
    | TypeVal of typeVal
    //| IdTypeVariable of string
    | FunType of alyoshaType * alyoshaType
    | ConnectionTypeVariable of int
    | TypeScheme of alyoshaType * (int list)
    | AnyType

and typeVal =
    | IntType
    | BoolType
    | StringType
    | UnitType