module Types

type alyoshaType =
    | TypeVal of typeVal
    | FunType of alyoshaType * alyoshaType
    | RefType of alyoshaType
    | ConnectionTypeVariable of int
    | TypeScheme of alyoshaType * (int list)
    | AnyType

and typeVal =
    | IntType
    | BoolType
    | StringType
    | UnitType