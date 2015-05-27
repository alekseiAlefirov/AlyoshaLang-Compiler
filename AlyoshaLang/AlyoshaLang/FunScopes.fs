module FunScopes

open System.Collections.Generic

type variableIdScopeRelationType =
    | ExternalParameter
    | NaturalParameter
    | CoRecursiveFun
    | InnerVariable
    | OwnName

[<AllowNullLiteralAttribute>]
type Scope(id : int, 
            depth : int,
            parentScope : int,
            ownName : int, //for scopes of recursive funs
            corecursiveFuns : Set<int>,
            usedVariables : (int * variableIdScopeRelationType) list) =
    
    let externalParameters, naturalParametersNotSorted, innerVariables =
        let rec divide epAcc npAcc ivAcc = function
        | (n, ExternalParameter) :: t -> divide (n :: epAcc) npAcc ivAcc t
        | (n, NaturalParameter) :: t -> divide epAcc (n :: npAcc) ivAcc t
        | (n, InnerVariable) :: t -> divide epAcc npAcc (n :: ivAcc) t
        | (_, OwnName) :: t -> divide epAcc npAcc ivAcc t
        | (_, CoRecursiveFun) :: t -> divide epAcc npAcc ivAcc t
        | [] -> (List.toArray epAcc), (List.toArray npAcc), (List.toArray ivAcc)
        divide [] [] [] usedVariables

    //sort natural parameters upside down
    let naturalParameters =
        let comparer n m = m - n
        naturalParametersNotSorted |> Array.sortWith comparer

    let inScopeVarTable = 
        let res = new Dictionary<int, variableIdScopeRelationType * int>()
        for i = 0 to externalParameters.Length - 1 do
            res.[externalParameters.[i]] <- (ExternalParameter, i)
        
        for i = 0 to naturalParameters.Length - 1 do
            res.[naturalParameters.[i]] <- (NaturalParameter, i)
        
        for i = 0 to innerVariables.Length - 1 do
            res.[innerVariables.[i]] <- (InnerVariable, i)

        res

    member this.Id = id
    member this.Depth = depth
    member this.ParentScope = parentScope
    member this.ExternalParameters = externalParameters
    member this.NaturalParameters = naturalParameters
    member this.InnerVariables = innerVariables
    member this.InScopeVarTable = inScopeVarTable

//№ of Scope-owner for unique variable
//Saved in Table of Symbols
type varScopeInfo = int

