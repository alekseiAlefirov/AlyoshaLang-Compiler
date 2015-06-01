module Typer

open AlyoshaAST
open Types
open FunScopes
open VariablesInformation
open System.Collections.Generic

type typeConstraint = alyoshaType * alyoshaType

type unifyResult =
    | Success of typeConstraint list
    | Error of string

type Substitution = Dictionary<int, alyoshaType> * Dictionary<int, int>

exception UnifyException
exception NoSuchTypeVariableInSubstitutionException
//exception AssignToFunArgumentIdException of string
exception ReassignNotDefinedReferenceException of string
exception ReassignNotTheReferenceException of string
exception LetDefinitionIsFollowedByNothingException of string
// exception ElseMissingException
exception NotDefinedYetException of string
exception IdIsNotDefinedException of string
exception DifferentArgsWithSameNameException
exception MutuallyRecursiveFunctionsAreNotSupportedYetException
exception FunctionAndArgumentWithSameNameException
exception SameNamesForMutuallyRecursiveFunctionsException


let rec substituteIntoConstraintList (constraintList : typeConstraint list) (typeVariable : alyoshaType) (typeToSubstitute : alyoshaType) =
    
    let substituteIntoConstraint (constr : typeConstraint) (typeVariable : alyoshaType) (typeToSubstitute : alyoshaType) =
        let rec substituteIntoType (intoWhatType : alyoshaType ) (typeVariable : alyoshaType) (typeToSubstitute : alyoshaType) =
            match intoWhatType, typeVariable with
            | ConnectionTypeVariable n1, ConnectionTypeVariable n2 ->
                if n1 = n2 then typeToSubstitute else intoWhatType
            | FunType (arg, value), _ -> FunType (substituteIntoType arg typeVariable typeToSubstitute, substituteIntoType value typeVariable typeToSubstitute)
            | RefType inType, _ -> RefType (substituteIntoType inType typeVariable typeToSubstitute)
            | _ -> intoWhatType

        let left, right = constr
        substituteIntoType left typeVariable typeToSubstitute,
            substituteIntoType right typeVariable typeToSubstitute
    
    match constraintList with
    | [] -> []
    | c :: cs -> (substituteIntoConstraint c typeVariable typeToSubstitute) :: (substituteIntoConstraintList cs typeVariable typeToSubstitute)
    

let unifyConstraintList nextVarNumberGetter (constraintList : typeConstraint list) =
    let rec unifyConstraintList (constraintList : typeConstraint list) =
        match constraintList with
        |[] -> []
        |c :: cs ->
            let left, right = c
            match left, right  with
            | TypeVal t1, TypeVal t2 ->
                if t1 = t2 then unifyConstraintList cs else raise UnifyException
            | ConnectionTypeVariable n1, ConnectionTypeVariable n2 ->
                if n1 = n2 then unifyConstraintList cs 
                else
                    if n1 < n2 then c :: (unifyConstraintList (substituteIntoConstraintList cs left right))
                    else (right, left) :: (unifyConstraintList (substituteIntoConstraintList cs right left))
            | ConnectionTypeVariable _ , _ -> c :: (unifyConstraintList (substituteIntoConstraintList cs left right))
            | _ , ConnectionTypeVariable _ -> (right, left) :: (unifyConstraintList (substituteIntoConstraintList cs right left))
            | FunType (arg1, value1), FunType(arg2, value2) -> unifyConstraintList((arg1, arg2)::(value1,value2)::cs)
            | RefType inType1, RefType inType2 -> unifyConstraintList((inType1, inType2) :: cs)
            | AnyType, t | t , AnyType -> unifyConstraintList ((t, ConnectionTypeVariable (nextVarNumberGetter ())) :: cs)
            | _ -> raise UnifyException

        
    let substitution = new Dictionary<int, alyoshaType>()
    let rec fold (subs : typeConstraint list) =
        match subs with
        | (ConnectionTypeVariable num, x) :: ss ->
            substitution.Add(num, x)
            fold ss
        | [] -> ()
        | _ -> raise UnifyException
    constraintList |> unifyConstraintList |> fold
    
    let rec substituteInRight rightType =
        match rightType with
        | ConnectionTypeVariable n ->
            if substitution.ContainsKey n then
                Some substitution.[n]
            else
                None
        | TypeVal _ -> None
        | FunType (arg, value) ->
            let newL, newR = substituteInRight arg, substituteInRight value
            match newL, newR with
            | Some x, Some y -> Some(FunType (x, y))
            | Some x, None -> Some(FunType (x, value))
            | None, Some y -> Some(FunType (arg, y))
            | None, None -> None
        | RefType inType ->
            let newInType = substituteInRight inType
            match newInType with
            | Some x -> Some (RefType x)
            | None -> None
        | AnyType -> None
        | TypeScheme _ -> raise UnifyException

    let isChanged = ref true
    while !isChanged do
        isChanged := false
        let subList = substitution |> Seq.map (fun x -> (x.Key, x.Value)) |> List.ofSeq
        for sub in subList do
            let n, value = sub
            match substituteInRight value with
            | Some x -> 
                substitution.[n] <- x
                isChanged := true
            | None -> ()

    // for variables remained in right part of substitution
    let reverseSubstitution = new Dictionary<int, int>()
    for sub in substitution do
        let n, value = sub.Key, sub.Value
        match value with
        | ConnectionTypeVariable m ->
            reverseSubstitution.[m] <- n
        | _ -> ()

    substitution, reverseSubstitution

let getTypeFromUnifiedSubstitution (unifiedSubstitution : Substitution) resTypeNum =
    let substitution, reverseSub = unifiedSubstitution
    if substitution.ContainsKey resTypeNum then
        let unifiedValue = substitution.[resTypeNum]
        let rec getFreeVariables uV fV =
            match uV with
            | AnyType | TypeScheme _ -> raise UnifyException
            | TypeVal _ -> fV
            | RefType inType ->
                getFreeVariables inType fV
            | FunType (arg, value) -> 
                getFreeVariables arg (getFreeVariables value fV)
            | ConnectionTypeVariable n ->
                if n <= resTypeNum then fV
                else Set.add n fV
        let freeVariables =  getFreeVariables unifiedValue Set.empty
        if freeVariables.IsEmpty then 
            unifiedValue
        else 
            TypeScheme (unifiedValue, List.ofSeq freeVariables)
    elif reverseSub.ContainsKey resTypeNum then
        //left reverse substitude means not free variable value
        let value = reverseSub.[resTypeNum]
        match value with
        | n when n < resTypeNum -> ConnectionTypeVariable n
        | _ -> raise UnifyException
    else raise UnifyException

let getTypeFromConstraints constraints resTypeVariableNum nextVarNumberGetter =
    getTypeFromUnifiedSubstitution (unifyConstraintList nextVarNumberGetter constraints) resTypeVariableNum
    
    
    

let checkProgram (prog : program) =
    
    let typeContext = new Dictionary<string, int * alyoshaType * bool>()
    let TableOfSymbols = new ResizeArray<varIdInformation>();
    
    let connectionVariablesCounter = ref -1
    let nextVariableNumId () = 
        incr connectionVariablesCounter
        !connectionVariablesCounter

    let tableOfSymbolsCounter = ref -1
    let nextIdInTable () =
        incr tableOfSymbolsCounter
        !tableOfSymbolsCounter

    let getInstanceOfType (sourceType : alyoshaType) =
        match sourceType with
        | TypeScheme (inType, variables) ->
            let newVars = new Dictionary<int, alyoshaType>()
            for n in variables do
                newVars.[n] <- ConnectionTypeVariable (nextVariableNumId ())

            let rec f inType =                
                match inType with
                | TypeVal _ -> inType
                | FunType (arg, value) -> FunType(f arg, f value)
                | RefType inType -> RefType(f inType)
                | ConnectionTypeVariable num -> 
                    if newVars.ContainsKey num then
                        newVars.[num]
                    else inType
                | _ -> raise UnifyException            
            f inType
        | _ -> sourceType
    
    let rec getConstraintsFromBlock (constraintList : typeConstraint list) (blockValueType : alyoshaType) (blk : expression list) : typeConstraint list =
        match blk with
        | expr :: [] -> 
            let res = getConstraintsFromExpr constraintList blockValueType expr
            res
        | (Statement (LetAssignment ass) ) :: exprs -> 
            let varName, oldContext, newConstraints =
                match ass with
                | UsualAssignment (varId, expr) ->
                    let varName, tableId = varId
                    
                    let oldContext = if typeContext.ContainsKey varName then Some (typeContext.[varName]) else None
                    let newConstraints = getConstraintsFromExpr constraintList AnyType expr
                    let boundExprType = getTypeOfExpr expr
                    
                    if !tableId = -1 then 
                        tableId := nextIdInTable () 
                        TableOfSymbols.Add {Type = boundExprType; ScopeInfo = -1}
                    else ()
                    typeContext.[varName] <- (!tableId, boundExprType, true)

                    varName, oldContext, newConstraints
                | ReadNum varId -> 
                    let varName, tableId = varId

                    let oldContext = if typeContext.ContainsKey varName then Some (typeContext.[varName]) else None
                    let idType = TypeVal IntType

                    if !tableId = -1 then
                        tableId := nextIdInTable ()
                        TableOfSymbols.Add {Type = idType; ScopeInfo = -1}
                    else ()
                    typeContext.[varName] <- (!tableId, idType, true)

                    varName, oldContext, constraintList
                | ReadLine varId ->
                    let varName, tableId = varId

                    let oldContext = if typeContext.ContainsKey varName then Some (typeContext.[varName]) else None
                    let idType = TypeVal StringType

                    if !tableId = -1 then
                        tableId := nextIdInTable ()
                        TableOfSymbols.Add {Type = idType; ScopeInfo = -1}
                    else ()
                    typeContext.[varName] <- (!tableId, idType, true)

                    varName, oldContext, constraintList

            let res = getConstraintsFromBlock newConstraints blockValueType exprs
            match oldContext with
            | Some oldType -> typeContext.[varName] <- oldType
            | None -> typeContext.Remove varName |> ignore
            res
            
        | (Statement (LetRecursiveAssignment asss)) :: exprs ->
            
            //check if names of mutually recursive functions are unique
            //add new typeContext for their names, save old
            //add new records in TableOfSymbols
            let oldFunNamesContext, newFunTypeVariables =
                let rec processFunNames = function
                    | [] -> []
                    | ((funName, funTableId), _, _, _) :: asss-> 
                        let rec checkThisFunNameIsUnique a =
                            match a with
                            | [] -> true
                            | ((nextFunName, _), _, _, _) :: aa ->  
                                if nextFunName = funName then false
                                else checkThisFunNameIsUnique aa
                        if not (checkThisFunNameIsUnique asss) then raise SameNamesForMutuallyRecursiveFunctionsException

                        let oldContext = 
                            if typeContext.ContainsKey funName then Some typeContext.[funName]
                            else None

                        let newTypeVariable = ConnectionTypeVariable (nextVariableNumId())
                        if !funTableId = -1 then
                            funTableId := nextIdInTable ()
                            TableOfSymbols.Add { Type = newTypeVariable; ScopeInfo = -1 }
                        else ()
                        typeContext.[funName] <- (!funTableId, newTypeVariable, false)
                        ((funName, oldContext), newTypeVariable) :: (processFunNames asss)

                processFunNames asss |> List.unzip
                
            let newConstraints, trueFunTypes =
                let rec processTheirAbstractions constraints newFunTypeVariables = function
                    | [] -> constraints, []
                    | ((funName, funTableId), args, funBody, _) :: asss ->
                        match newFunTypeVariables with
                            | funType :: nextFunTypeVariable ->
                                let abstrBody = Abstraction (args, funBody, ref -1)
                                let newConstraints = getConstraintsFromExpr constraintList funType abstrBody
                                let trueFunType = funName, (!funTableId, getTypeOfExpr abstrBody, true)
                                let resConstraints, nextTrueFunTypes = processTheirAbstractions newConstraints nextFunTypeVariable asss
                                resConstraints, trueFunType :: nextTrueFunTypes
                            | _ -> invalidArg "newFunTypeVariables" "Should be not empty"

                processTheirAbstractions constraintList newFunTypeVariables asss
                
            let rec putTrueFunTypesIntoContext = function
                | [] -> ()
                | (funName, tft) :: t -> 
                    typeContext.[funName] <- tft
                    putTrueFunTypesIntoContext t
            putTrueFunTypesIntoContext trueFunTypes

            let resConstraints = getConstraintsFromBlock newConstraints blockValueType exprs
            let rec returnOldContext oldFunNamesContext =
                match oldFunNamesContext with
                | [] -> ()
                | (funName, Some c) :: t -> 
                    typeContext.[funName] <- c
                    returnOldContext t
                | (funName, None) :: t ->
                    typeContext.Remove funName |> ignore
                    returnOldContext t                    
            returnOldContext oldFunNamesContext
            resConstraints            

        | expr :: exprs ->
            let newConstraints = getConstraintsFromExpr constraintList (TypeVal UnitType) expr
            getConstraintsFromBlock newConstraints blockValueType exprs
        | [] -> constraintList

    and getConstraintsFromExpr (constraintList : typeConstraint list ) (exprValueType : alyoshaType) (expr : expression)  : typeConstraint list =
        
        match expr with
        | Statement stmnt ->
            match stmnt with
            | LetAssignment ass -> //it has to be the last expression in the block
                match ass with
                |UsualAssignment (var, _)
                |ReadNum var
                |ReadLine var ->
                    let varName, _ = var
                    raise (LetDefinitionIsFollowedByNothingException varName)
            | LetRecursiveAssignment _ -> //it has to be the last expression in the block
                raise (LetDefinitionIsFollowedByNothingException "Some recursive definition")
            | Reassignment ass -> 
                let newConstraints =
                    match ass with
                    | UsualAssignment ((varName, tableId), expr) ->
                        let oldContext = if typeContext.ContainsKey varName then Some (typeContext.[varName]) else None
                        match oldContext with
                        | Some (realTableId, boundType, _) ->
                            if !tableId = -1 then tableId := realTableId
                            let inType = ConnectionTypeVariable (nextVariableNumId())
                            getConstraintsFromExpr ((boundType, RefType(inType)) :: constraintList) inType expr
                        | None -> 
                            raise (ReassignNotDefinedReferenceException varName)
                    | ReadNum (varName, tableId) -> 
                        let oldContext = if typeContext.ContainsKey varName then Some (typeContext.[varName]) else None
                        match oldContext with
                        | Some (realTableId, boundType, _) ->
                            if !tableId = -1 then tableId := realTableId
                            (boundType, RefType (TypeVal IntType)) :: constraintList
                        | None -> 
                            raise (ReassignNotDefinedReferenceException varName)
                    | ReadLine (varName, tableId) ->
                        let oldContext = if typeContext.ContainsKey varName then Some (typeContext.[varName]) else None
                        match oldContext with
                        | Some (realTableId, boundType, _) ->
                            if !tableId = -1 then tableId := realTableId
                            (boundType, RefType (TypeVal StringType)) :: constraintList
                        | None -> 
                            raise (ReassignNotDefinedReferenceException varName)                    
                (exprValueType, TypeVal UnitType) :: newConstraints

            | IfStatement (condition, trueBlock, elifList, elseBlock) ->
                let afterConditionConstraints = getConstraintsFromExpr constraintList (TypeVal BoolType) condition
                let afterTrueBlockConstraints = getConstraintsFromBlock afterConditionConstraints exprValueType (unboxBlock trueBlock)
                let afterElifListConstraints =
                    let folder constraints elifNode =
                        let elifCondition, elifBlock = elifNode
                        let newConstraints = getConstraintsFromExpr constraints (TypeVal BoolType) elifCondition
                        getConstraintsFromBlock newConstraints exprValueType (unboxBlock elifBlock)
                    elifList |> List.fold folder afterTrueBlockConstraints
                match elseBlock with
                | Some elseBlock ->
                    getConstraintsFromBlock afterElifListConstraints exprValueType (unboxBlock elseBlock)
                | None ->
                    (exprValueType, TypeVal UnitType)::afterElifListConstraints

            | WhileStatement (condition, whileBlock) ->
                let afterConditionConstraints = getConstraintsFromExpr ((exprValueType, TypeVal UnitType)::constraintList) (TypeVal BoolType) condition
                getConstraintsFromBlock afterConditionConstraints (TypeVal UnitType) (unboxBlock whileBlock)

            | WriteStatement expr ->
                getConstraintsFromExpr ((exprValueType, TypeVal UnitType)::constraintList) AnyType expr
            | MatchStatement _ ->
                raise (NotDefinedYetException "No match statements allowed")
                    

        | OrList exprList | AndList exprList ->
            let folder constraints expr =
                getConstraintsFromExpr constraints (TypeVal BoolType) expr 
            List.fold folder ((exprValueType, TypeVal BoolType)::constraintList) exprList
        | Not expr ->
            getConstraintsFromExpr ((exprValueType, TypeVal BoolType)::constraintList) (TypeVal BoolType) expr
        | IsEqual (expr1, expr2) | NotEqual (expr1, expr2) ->
            let commonType = ConnectionTypeVariable (nextVariableNumId ())
            let afterFirstConstraints = getConstraintsFromExpr ((exprValueType, TypeVal BoolType)::constraintList) commonType expr1
            getConstraintsFromExpr afterFirstConstraints commonType expr2

        | Greater (expr1, expr2) | Less (expr1, expr2) | NotGreater (expr1, expr2) | NotLess (expr1, expr2) ->
            let afterFirstConstraints = getConstraintsFromExpr ((exprValueType, TypeVal BoolType)::constraintList) (TypeVal IntType) expr1
            getConstraintsFromExpr afterFirstConstraints (TypeVal IntType) expr2
            
        | Sum seList ->
            let rec getSeconds l =
                match l with
                | [] -> []
                | (_ , x) :: t -> x :: (getSeconds t)
            let folder constraints expr =
                getConstraintsFromExpr constraints (TypeVal IntType) expr
            seList |> getSeconds |> List.fold folder ((exprValueType, TypeVal IntType) :: constraintList)

        | Mult seList ->
            let rec getSeconds l =
                match l with
                | [] -> []
                | (_ , x) :: t -> x :: (getSeconds t)
            let folder constraints expr =
                getConstraintsFromExpr constraints (TypeVal IntType) expr
            seList |> getSeconds |> List.fold folder ((exprValueType, TypeVal IntType) :: constraintList)

        | StringConcat seList ->
            let folder constraints expr =
                getConstraintsFromExpr constraints (TypeVal StringType) expr
            seList |> List.fold folder ((exprValueType, TypeVal StringType) :: constraintList)

        | Mod (expr1, expr2) ->
            let afterFirstConstraints = getConstraintsFromExpr ((exprValueType, TypeVal IntType)::constraintList) (TypeVal IntType) expr1
            getConstraintsFromExpr afterFirstConstraints (TypeVal IntType) expr2

        | SequenceExpression exprBlock ->
            getConstraintsFromBlock constraintList exprValueType (unboxBlock exprBlock)

        | ExprId (varName, tableId) ->
            if typeContext.ContainsKey varName then
                let context = typeContext.[varName]
                match context with
                | realTableId, contextType, true -> //it is let-bound value that can be polymorphic
                    if !tableId = -1 then tableId := realTableId else ()
                    (exprValueType, (getInstanceOfType contextType)) :: constraintList
                | realTableId, contextType, false ->
                    if !tableId = -1 then tableId := realTableId else ()
                    (exprValueType, contextType)::constraintList
            else
                raise (IdIsNotDefinedException varName)

        | Abstraction (args, value, _) -> 
            let valueType = ConnectionTypeVariable(nextVariableNumId ())
            
            let funType, newOldContext = foldAbstractionArgs args valueType
            let newConstraints = getConstraintsFromExpr constraintList valueType value
            let rec returnContext newOldContext =
                match newOldContext with
                | [] -> ()
                | (name, Some oldValue) :: cs ->
                    typeContext.[name] <- oldValue
                    returnContext cs
                | (name, None) :: cs ->
                    typeContext.Remove name |> ignore
                    returnContext cs
            returnContext newOldContext
            (exprValueType, funType) :: newConstraints

        | Application (appL, appR) ->
            let rec foldGetFTypeAndConstraints args = 
                match args with
                | [] -> (FunType(TypeVal UnitType, exprValueType) , constraintList)
                | arg :: [] -> 
                    let argType = ConnectionTypeVariable (nextVariableNumId ())
                    FunType (argType, exprValueType), (getConstraintsFromExpr constraintList argType arg)
                | arg :: t ->
                    let funValueType, newConstraints = foldGetFTypeAndConstraints t
                    let argType = ConnectionTypeVariable (nextVariableNumId ())
                    FunType (argType, funValueType), (getConstraintsFromExpr newConstraints argType arg) 

            let funType, newConstraints = foldGetFTypeAndConstraints appR
            getConstraintsFromExpr newConstraints funType appL

        | Reference expr ->
            let inType = ConnectionTypeVariable (nextVariableNumId ())
            let newConstraints = getConstraintsFromExpr constraintList inType expr
            (exprValueType, RefType (inType)) :: newConstraints
            
        | Unref expr ->  
            let outType = ConnectionTypeVariable (nextVariableNumId ())
            let newConstraints = getConstraintsFromExpr constraintList outType expr
            (RefType (exprValueType), outType) :: newConstraints

        | NumVal _ ->
            (exprValueType, TypeVal IntType) :: constraintList
        | StringVal _ ->
            (exprValueType, TypeVal StringType) :: constraintList
        | BoolVal _ ->
            (exprValueType, TypeVal BoolType) :: constraintList
        | UnitVal ->
            (exprValueType, TypeVal UnitType) :: constraintList
            

    and foldAbstractionArgs args abstractionBodyType =
        match args with
        | [] -> FunType (TypeVal UnitType, abstractionBodyType), []
        | (argName, tableId) :: [] -> 
            let oldContext =
                if typeContext.ContainsKey argName then Some (typeContext.[argName])
                else None
            let argIdType = ConnectionTypeVariable (nextVariableNumId ())
                   
            if !tableId = -1 then
                tableId := nextIdInTable () 
                TableOfSymbols.Add { Type = argIdType; ScopeInfo = -1 }
            else ()

            typeContext.[argName] <- (!tableId, argIdType, false)
            FunType (argIdType, abstractionBodyType), [argName, oldContext]
        | (argName, tableId) :: args ->
            let valueType, fold = foldAbstractionArgs args abstractionBodyType
            let rec checkArgIsUnique argName fold =
                match fold with
                | [] -> true
                | (a, c)::fs ->
                    if argName = a then false
                    else checkArgIsUnique argName fs
            if checkArgIsUnique argName fold then
                let oldContext =
                    if typeContext.ContainsKey argName then Some (typeContext.[argName])
                    else None
                let argIdType = ConnectionTypeVariable (nextVariableNumId ())
                        
                if !tableId = -1 then
                    tableId := nextIdInTable ()
                    TableOfSymbols.Add { Type = argIdType; ScopeInfo = -1 }
                else ()

                typeContext.[argName] <- (!tableId, argIdType, false)
                FunType (argIdType, valueType), ((argName, oldContext) :: fold)
            else
                raise DifferentArgsWithSameNameException
    
    and getTypeOfExpr expr : alyoshaType =
        let resTypeVariableNum = nextVariableNumId ()
        let resTypeVariable = ConnectionTypeVariable resTypeVariableNum
        let constraints = getConstraintsFromExpr [] resTypeVariable expr
        getTypeFromConstraints constraints resTypeVariableNum nextVariableNumId

    and getTypeOfRecExpr typeVar expr : alyoshaType =
        let typeVarNum =
            match typeVar with
            | ConnectionTypeVariable x -> x
            | _ -> raise (invalidArg (sprintf "%A" typeVar) ("getTypeOfRecExpr : supposed to be ConnectionTypeVariable"))
        let constraints = getConstraintsFromExpr [] typeVar expr
        getTypeFromConstraints constraints typeVarNum nextVariableNumId
        
    let substitution = 
        match prog with
        | Program (_, _, blk) -> getConstraintsFromBlock [] (TypeVal UnitType) (unboxBlock blk) |> unifyConstraintList nextVariableNumId

    for varInfo in TableOfSymbols do
        match varInfo.Type with
        | ConnectionTypeVariable n ->
            let realType = getTypeFromUnifiedSubstitution substitution n    
            varInfo.Type <- realType                        
        | _ -> ()
    TableOfSymbols.ToArray();
(**)