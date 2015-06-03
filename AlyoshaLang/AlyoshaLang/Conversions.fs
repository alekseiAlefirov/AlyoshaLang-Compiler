module Conversions

open AlyoshaAST


type AlyoshaConversion = (program -> program)

let FoldConstants p =
    match p with
    |Program (programName, unionBlock, ast) ->
        
        let rec processList flag exprList =
            exprList |> List.map 
                (fun x ->
                    match foldConstantsInExpr x with
                    | Some goodStaff -> 
                        flag := true
                        goodStaff
                    | None -> x
                )
        
        and foldConstantsInBlock blk = 
            let wasFold = ref false
            let foldUnits exprList =
                let rec f acc = function
                | [] -> acc
                | (UnitVal) :: t -> 
                    wasFold := true
                    f acc t
                | x :: t -> f (x :: acc) t
                exprList |> List.rev |> function
                    | [] -> []
                    | bot :: t -> f [bot] t
            let foldRes = 
                blk |> unboxBlock |> processList wasFold |> foldUnits
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
                | Some (IfStatement (condition, trueBlock, elifList, elseBlockOpt) as stmnt) ->
                    match condition with
                    | BoolVal true ->
                        SequenceExpression trueBlock
                    | BoolVal false ->
                        let rec processElif = function
                            | [] -> 
                                match elseBlockOpt with
                                | Some body -> SequenceExpression body
                                | None -> UnitVal
                            | el :: t -> 
                                match el with
                                | BoolVal true, body -> SequenceExpression body
                                | BoolVal false, _ -> processElif t
                                | c, b -> Statement (IfStatement (c, b, t, elseBlockOpt))
                        processElif elifList
                    | _ -> Statement stmnt
                    |> Some
                | Some (WhileStatement (BoolVal true, _)) -> Some UnitVal
                | Some stmnt -> Some (Statement stmnt)
                | None -> None
            
            | OrList exprs ->
                let wasFold = ref false
                let foldExprs = exprs |> processList wasFold
                match !wasFold with
                | true ->
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
                        
                | false -> None

            | AndList exprs ->
                let wasFold = ref false
                let foldExprs = exprs |> processList wasFold
                match !wasFold with
                | true ->
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
                | false -> None
    
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
    
            | Greater (expr1, expr2) ->
                let foldExpr1, foldExpr2 = foldConstantsInExpr expr1, foldConstantsInExpr expr2
                let left, right =
                    match foldExpr1, foldExpr2 with
                    | Some foldExpr1, Some foldExpr2 -> Some foldExpr1, Some foldExpr2
                    | None, Some foldExpr2 -> Some expr1, Some foldExpr2
                    | Some foldExpr1, None -> Some foldExpr1, Some expr2
                    | None, None -> None, None

                match left, right with
                | Some (NumVal n1), Some (NumVal n2) -> Some (BoolVal (n1 > n2))
                | Some l, Some r -> Some (Greater(l, r))
                | _ -> None
            | Less (expr1, expr2) ->
                let foldExpr1, foldExpr2 = foldConstantsInExpr expr1, foldConstantsInExpr expr2
                let left, right =
                    match foldExpr1, foldExpr2 with
                    | Some foldExpr1, Some foldExpr2 -> Some foldExpr1, Some foldExpr2
                    | None, Some foldExpr2 -> Some expr1, Some foldExpr2
                    | Some foldExpr1, None -> Some foldExpr1, Some expr2
                    | None, None -> None, None

                match left, right with
                | Some (NumVal n1), Some (NumVal n2) -> Some (BoolVal (n1 < n2))
                | Some l, Some r -> Some (Less(l, r))
                | _ -> None

            | NotGreater (expr1, expr2) ->
                let foldExpr1, foldExpr2 = foldConstantsInExpr expr1, foldConstantsInExpr expr2
                let left, right =
                    match foldExpr1, foldExpr2 with
                    | Some foldExpr1, Some foldExpr2 -> Some foldExpr1, Some foldExpr2
                    | None, Some foldExpr2 -> Some expr1, Some foldExpr2
                    | Some foldExpr1, None -> Some foldExpr1, Some expr2
                    | None, None -> None, None

                match left, right with
                | Some (NumVal n1), Some (NumVal n2) -> Some (BoolVal (n1 <= n2))
                | Some l, Some r -> Some (NotGreater(l, r))
                | _ -> None

            | NotLess (expr1, expr2) ->
                let foldExpr1, foldExpr2 = foldConstantsInExpr expr1, foldConstantsInExpr expr2
                let left, right =
                    match foldExpr1, foldExpr2 with
                    | Some foldExpr1, Some foldExpr2 -> Some foldExpr1, Some foldExpr2
                    | None, Some foldExpr2 -> Some expr1, Some foldExpr2
                    | Some foldExpr1, None -> Some foldExpr1, Some expr2
                    | None, None -> None, None

                match left, right with
                | Some (NumVal n1), Some (NumVal n2) -> Some (BoolVal (n1 >= n2))
                | Some l, Some r -> Some (NotLess(l, r))
                | _ -> None

            | Sum terms ->
                let signs, terms = List.unzip terms
                let wasFold = ref false
                let foldTerms = terms |> processList wasFold
                let folder acc term =
                    let accNum, notFolded = acc
                    match term with
                    | (sign, NumVal n) ->
                        wasFold := true
                        match sign with
                        | Plus -> (accNum + n), notFolded
                        | Minus -> (accNum - n), notFolded
                    | _ -> accNum, (term :: notFolded)
                let res = List.zip signs foldTerms |> List.fold folder (0, []) |> (fun (x, y) ->
                            
                            let terms = List.rev y
                            let rec bubblePlus leftAcc = function
                                | [] -> None
                                | ((Plus, _) as term) :: t -> Some (term :: ((List.rev leftAcc) @ t))
                                | ((Minus, _) as term) :: t -> bubblePlus (term :: leftAcc) t
                            let terms = 
                                match x with
                                | 0 ->  terms
                                | _ -> (Plus, NumVal x) :: terms
                            
                            match terms with
                            | [] -> NumVal 0
                            | (Plus, n) :: [] -> n
                            |  _ ->
                                match bubblePlus [] terms with
                                | Some x -> Sum x
                                | None -> Sum ((Plus, NumVal 0) :: terms)
                        )
                match !wasFold with
                | true -> Some res
                | false -> None
            
            | Mult factors ->
                let signs, factors = List.unzip factors
                let wasFold = ref false
                let foldFactors = factors |> processList wasFold
                let folder acc factor =
                    let accNum, notFolded = acc
                    match factor with
                    | (sign, NumVal n) ->
                        wasFold := true
                        match sign with
                        | Mul -> (accNum * n), notFolded
                        | Div -> (accNum / n), notFolded
                    | _ -> accNum, (factor :: notFolded)
                let res = List.zip signs foldFactors |> List.fold folder (1, []) |> (fun (x, y) ->
                            
                            let terms = List.rev y
                            let rec bubbleMul leftAcc = function
                                | [] -> None
                                | ((Mul, _) as factor) :: t -> Some (factor :: ((List.rev leftAcc) @ t))
                                | ((Div, _) as factor) :: t -> bubbleMul (factor :: leftAcc) t
                            let terms = 
                                match x with
                                | 1 -> terms
                                | _ -> (Mul, NumVal x) :: terms
                            
                            match terms with
                            | [] -> NumVal 0
                            | (Mul, n) :: [] -> n
                            |  _ ->
                                match bubbleMul [] terms with
                                | Some x -> Mult x
                                | None -> Mult ((Mul, NumVal 0) :: terms)
                        )
                match !wasFold with
                | true -> Some res
                | false -> None

            | Mod (expr1, expr2) ->  
                let foldExpr1, foldExpr2 = foldConstantsInExpr expr1, foldConstantsInExpr expr2
                let left, right =
                    match foldExpr1, foldExpr2 with
                    | Some foldExpr1, Some foldExpr2 -> Some foldExpr1, Some foldExpr2
                    | None, Some foldExpr2 -> Some expr1, Some foldExpr2
                    | Some foldExpr1, None -> Some foldExpr1, Some expr2
                    | None, None -> None, None

                match left, right with
                | Some (NumVal n1), Some (NumVal n2) -> Some (NumVal (n1 % n2))
                | Some l, Some r -> Some (Mod(l, r))
                | _ -> None

            | StringConcat stringList ->
                let wasFold = ref false
                let folder (acc : (expression list * string)) str =
                    let processedExprs, currentString = acc
                    match foldConstantsInExpr str with
                    | Some str ->
                        wasFold := true
                        match str with
                        | StringVal s ->
                            processedExprs, String.concat "" [currentString; s]
                        | _ ->
                            match currentString with
                            | "" -> (str::processedExprs), ""
                            | _ -> str::(StringVal currentString)::processedExprs, ""
                    | None ->
                        match str with
                        | StringVal s ->
                            wasFold := true
                            processedExprs, String.concat "" [currentString; s]
                        | _ ->
                            match currentString with
                            | "" -> str::processedExprs, ""
                            | _ -> str::(StringVal currentString)::processedExprs, ""
                        
                let res = stringList |> List.fold folder ([], "") 
                        |> (fun (x, y) ->
                                match y with
                                | "" -> x
                                | _ -> (StringVal y) :: x
                            )
                        |> List.rev
                match !wasFold with
                | true -> 
                    match res with
                    | [] -> StringVal ""
                    | [s] -> s
                    | _ -> StringConcat res
                    |> Some
                | false -> None
    
            | SequenceExpression blk ->
                blk |> foldConstantsInBlock |> function
                    | Some b -> b |> SequenceExpression |> Some
                    | None -> None
            | ExprId _ -> None

            | Abstraction (args, expr, scopeId) ->
                match foldConstantsInExpr expr with
                | Some e -> (args, e, scopeId) |> Abstraction |> Some
                | None -> None                
            
            | Application (funExpr, argExprs) ->
                let foldFunExpr = foldConstantsInExpr funExpr
                let wasFold = ref false
                let foldArgExprs = argExprs |> processList wasFold
                match foldFunExpr, !wasFold with
                | Some x, true -> (x, foldArgExprs) |> Application |> Some
                | Some x, false -> (x, argExprs) |> Application |> Some
                | None, true -> (funExpr, foldArgExprs) |> Application |> Some
                | _ -> None

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
            | _ -> None

        match foldConstantsInBlock ast with
        | Some foldStaff -> Program (programName, unionBlock, foldStaff)
        | None -> p
