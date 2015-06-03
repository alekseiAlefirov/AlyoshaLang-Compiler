module Conversions

open AlyoshaAST


type AlyoshaConversion = (program -> program)

let FoldConstants p =
    match p with
    |Program (programName, unionBlock, ast) ->
        let rec foldConstantsInBlock blk = 
            let wasFold = ref false
            let foldRes = 
                blk |> unboxBlock |> List.map 
                    (fun x ->
                        match foldConstantsInExpr x with
                        | Some goodStaff -> 
                            wasFold := true
                            goodStaff
                        | None -> x
                    )
            if !wasFold then Some (Block foldRes)
            else None

        and foldConstantsInExpr = function
            | Statement stmnt ->
                let foldStatement = 
                    match stmnt with
                    | LetAssignment ass ->
                        match ass with
                        | UsualAssignment (v, expr) -> 
                            match foldConstantsInExpr expr with
                            | Some x -> Some (LetAssignment (UsualAssignment (v, x)))
                            | None -> None
                        | _ -> None
                    | LetRecursiveAssignment assList ->
                        let wasFold = ref false
                        let foldAssList = 
                            assList |> List.map
                                (fun x ->
                                    let f, args, expr, scopeId = x
                                    match foldConstantsInExpr expr with
                                    | Some foldStaff -> 
                                        wasFold := true
                                        (f, args, foldStaff, scopeId) 
                                    | None -> x
                                )
                        match !wasFold with
                        | true -> Some (LetRecursiveAssignment foldAssList)
                        | false -> None
                    | Reassignment ass ->
                        match ass with
                        | UsualAssignment (v, expr) -> 
                            match foldConstantsInExpr expr with
                            | Some x -> Some (Reassignment (UsualAssignment (v, x)))
                            | None -> None
                        | _ -> None
                    | IfStatement (condition, trueBlock, elifList, elseOptionBlock) ->
                        let wasFold = ref false
                        let foldConditionFun condition =
                            match foldConstantsInExpr condition with
                            | Some x -> 
                                wasFold := true
                                x
                            | None -> condition
                        let foldBlockFun blk =
                            match  foldConstantsInBlock blk with
                            | Some x -> 
                                wasFold := true
                                x
                            | None -> blk
                        let foldCondition = foldConditionFun condition
                        let foldTrueBlock = foldBlockFun trueBlock
                        let foldElifList =
                            elifList |> List.map
                                (fun (x, y) ->
                                    foldConditionFun x, foldBlockFun y
                                )
                        let foldElseBlock =
                            match elseOptionBlock with
                            | Some x -> Some (foldBlockFun x)
                            | None -> None
                        match !wasFold with
                        | true -> Some (IfStatement (foldCondition, foldTrueBlock, foldElifList, foldElseBlock))
                        | false -> None
                    | WhileStatement (expr, blk) ->
                        match foldConstantsInExpr expr, foldConstantsInBlock blk with
                        | Some e, Some b -> Some (WhileStatement (e,b))
                        | Some e, None -> Some (WhileStatement (e, blk))
                        | None, Some b -> Some (WhileStatement (expr, b))
                        | None, None -> None
                    | WriteStatement expr ->
                        match foldConstantsInExpr expr with
                        | Some x -> Some (WriteStatement x)
                        | None -> None
                    | MatchStatement _ -> None //TODO
                match foldStatement with
                | Some stmnt -> Some (Statement stmnt)
                | None -> None
            
            | OrList exprs ->
                let foldExprs = exprs |> Block |> foldConstantsInBlock
                match foldExprs with
                | Some (Block foldExprs) ->
                    let wasOne = ref false
                    let wasFold = ref false
                    let rec foldOr acc = function
                    | (BoolVal b) :: t ->
                        if !wasOne then wasFold := true else wasOne := true
                        foldOr (acc || b) t
                    | x :: t -> 
                        let foldAcc, foldT = foldOr acc foldExprs
                        foldAcc, (x :: foldT)
                    | [] -> acc, []
                    let foldAcc, foldExprs2 = foldOr false foldExprs
                    match !wasFold, foldAcc, foldExprs2 with
                    | true, true, [] -> Some (BoolVal true)
                    | true, true, _ -> Some (OrList ((BoolVal true) :: foldExprs2))
                    | true, false, [] -> Some (BoolVal false)
                    | true, false, [x] -> Some x
                    | true, false, _ -> Some (OrList foldExprs2)
                    | false, _, _ -> Some (OrList foldExprs)
                        
                | None -> None
            | AndList exprs ->
                let foldExprs = exprs |> Block |> foldConstantsInBlock
                match foldExprs with
                | Some (Block foldExprs) ->
                    let wasOne = ref false
                    let wasFold = ref false
                    let rec foldAnd acc = function
                    | (BoolVal b) :: t ->
                        if !wasOne then wasFold := true else wasOne := true
                        foldAnd (acc && b) t
                    | x :: t -> 
                        let foldAcc, foldT = foldAnd acc foldExprs
                        foldAcc, (x :: foldT)
                    | [] -> acc, []
                    let foldAcc, foldExprs2 = foldAnd true foldExprs
                    match !wasFold, foldAcc, foldExprs2 with
                    | true, true, [] -> Some (BoolVal true)
                    | true, true, [x] -> Some x
                    | true, true, _ -> Some (AndList foldExprs2)
                    | true, false, [] -> Some (BoolVal false)
                    | true, false, _ -> Some (AndList ((BoolVal false)::foldExprs2))
                    | false, _, _ -> Some (AndList foldExprs)
                | None -> None
    
            | Not expr ->
                let foldExpr = foldConstantsInExpr expr
                match foldExpr with
                | Some foldExpr -> 
                    match foldExpr with
                    | BoolVal true -> Some (BoolVal false)
                    | BoolVal false -> Some (BoolVal true)
                    | _ -> Some (Not foldExpr)
                | None -> None

            | IsEqual (expr1, expr2) ->
                let foldExpr1, foldExpr2 = foldConstantsInExpr expr1, foldConstantsInExpr expr2
                let left, right =
                    match foldExpr1, foldExpr2 with
                    | Some foldExpr1, Some foldExpr2 -> Some foldExpr1, Some foldExpr2
                    | None, Some foldExpr2 -> Some expr1, Some foldExpr2
                    | Some foldExpr1, None -> Some foldExpr1, Some expr2
                    | None, None -> None, None

                match left, right with
                | Some (NumVal n1), Some (NumVal n2) -> Some (BoolVal (n1 = n2))
                | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 = b2))
                | Some (StringVal s1), Some (StringVal s2) -> Some (BoolVal (s1 = s2))
                | Some UnitVal, Some UnitVal -> Some (BoolVal true)
                | Some l, Some r -> Some (IsEqual(l, r)) //TODO if new types appear
                | _ -> None


            | NotEqual (expr1, expr2) ->
                let foldExpr1, foldExpr2 = foldConstantsInExpr expr1, foldConstantsInExpr expr2
                let left, right =
                    match foldExpr1, foldExpr2 with
                    | Some foldExpr1, Some foldExpr2 -> Some foldExpr1, Some foldExpr2
                    | None, Some foldExpr2 -> Some expr1, Some foldExpr2
                    | Some foldExpr1, None -> Some foldExpr1, Some expr2
                    | None, None -> None, None

                match left, right with
                | Some (NumVal n1), Some (NumVal n2) -> Some (BoolVal (n1 <> n2))
                | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 <> b2))
                | Some (StringVal s1), Some (StringVal s2) -> Some (BoolVal (s1 <> s2))
                | Some UnitVal, Some UnitVal -> Some (BoolVal false)
                | Some l, Some r -> Some (NotEqual(l, r)) //TODO if new types appear
                | _ -> None
    
            (*| Greater of expression * expression
            | Less of expression * expression
            | NotGreater of expression * expression
            | NotLess of expression * expression

            | Sum of (sumSign * expression) list
            | Mult of (mulSign * expression) list
            | Mod of expression * expression

            | StringConcat of expression list
    
            | SequenceExpression of block
            | ExprId of varId
            | Abstraction of (varId list) * expression * (int ref) //int ref is for the scope information
            | Application of expression * (expression list)*)
            | Reference expr ->
                match foldConstantsInExpr expr with
                | Some foldExpr -> Some (Reference foldExpr)
                | None -> None
            | Unref expr ->
                match foldConstantsInExpr expr with
                | Some (Reference (NumVal _ as n)) -> Some n
                | Some (Reference (BoolVal _ as b)) -> Some b
                | Some (Reference (StringVal _ as s)) -> Some s
                | Some (Reference (UnitVal as u)) -> Some u
                | Some foldExpr -> Some (Unref foldExpr)
                | None -> None
            | NumVal _ as x -> Some x
            | BoolVal _ as x -> Some x
            | StringVal _ as x -> Some x
            | UnitVal as x -> Some x
            | _ -> None //TODO

        match foldConstantsInBlock ast with
        | Some foldStaff -> Program (programName, unionBlock, foldStaff)
        | None -> p
