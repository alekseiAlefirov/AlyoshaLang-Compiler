module CodeGenerator

open AlyoshaAST
open FunScopes
open VariablesInformation

open System.Text

exception NotSupportedYet of string

type Type =
    | IntType
    | BoolType
    | StringType
    | UnitType
    | FunType

let typeId = function
    | IntType -> 0
    | BoolType -> 1
    | StringType -> 2
    | UnitType -> 3
    | FunType -> 4

let GenerateCode (ast : AlyoshaAST.program) tableOfSymbols (scopes : Scope []) stringConstants =

    let programName, mainBlock = match ast with Program (pn, _, mb) -> (pn, mb)
    
    let strBuilder = new StringBuilder()
    let println (x : 'a) =
        Printf.kprintf (fun s -> strBuilder.Append(s).Append "\n" |> ignore) x
    let print (x : 'a) =
        Printf.kprintf (fun s -> strBuilder.Append(s) |> ignore) x
    let printIntendln (x : 'a) =
        Printf.kprintf (fun s -> strBuilder.Append("    ").Append(s).Append "\n" |> ignore) x
    
    let printHeader() =
        println(" .386")
        println(" .model flat,stdcall")
        println("option casemap:none")
        println("")


    let printIncludes() =
        println("includelib kernel32.lib")
        println("includelib masm32.lib")
        println("include c:\masm32\include\windows.inc")
        println("include c:\masm32\include\kernel32.inc")
        println("include c:\masm32\include\masm32.inc")
        println("")

    let printData() =
        println(" .data")
        println("heapHandle dd  ?") //handle to operate the heap
        //heapObjectHandle
        
        println("")

    let printConsts() =
        println(" .const")
        //print typeIds
        //print string constants
        println("")

    let printCode() =
        
        let writeProcName = "writeProc"
        let readNumProcName = "readNumProc"
        let currentScope = ref 0
        let scopeSize n =
            let theScope = scopes.[n]
            1 //selfsize
            + 5       //codeId, isAssigned, mutually recursive funs block ptr, self pointer
            + 1     //№ of natural parameters
            + theScope.NaturalParameters.Length * 2
            + theScope.ExternalParameters.Length * 2
            + theScope.InnerVariables.Length * 2
        
        let scopeSelfPtrOffset = 4 * 4

        let naturalParameterOffsetInScope n m =
            6 + m*2

        let externalParameterOffsetInScope n m =
            6 + (scopes.[n].NaturalParameters.Length)*2 + m*2

        let innerVariableOffsetInScope n m =
            6 + (scopes.[n].NaturalParameters.Length)*2 + (scopes.[n].ExternalParameters.Length)*2 + m*2

        // m for tableId
        let parameterOffsetInScope n m =
            let paramType, num = scopes.[n].InScopeVarTable.[m]
            match paramType with
            | NaturalParameter -> naturalParameterOffsetInScope n num
            | ExternalParameter -> externalParameterOffsetInScope n num
            | InnerVariable -> innerVariableOffsetInScope n num
            | OwnName
            | CoRecursiveFun -> invalidArg "parameterOffsetInScope" ""
        (*let printSinglePush x =
            println "%s %s" "push" x*)

        let printPushValueFromRegs() =
            printIntendln "push eax"
            printIntendln "push ebx"

        (*let printClean() =
            println "mov es"*)

        let printCleanStack n =
            printIntendln "add esp, %d" (n*4)

        (*let printPushUnit () =
            printSinglePush "0"
            printSinglePush "0" *)
        
        let printRegInt n =
            printIntendln "mov eax, %d" (typeId IntType) 
            printIntendln "mov ebx, %d" n
            
        let printRetUnit() =
            printIntendln "mov eax, %d" (typeId UnitType) 
            printIntendln "mov ebx, 0"    
        
        let printReadNumProcCode() =
            println "readNumProc:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            printIntendln "sub esp, 12 ; input, stringbuffer, actual length"
            printIntendln ""
            printIntendln "invoke GetStdHandle, STD_INPUT_HANDLE"
            printIntendln "mov [ebp - 4], eax"
            printIntendln ""
            printIntendln "invoke GetProcessHeap"
            printIntendln "mov heapHandle, eax"
            printIntendln ""
            printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, 10"
            printIntendln "mov [ebp - 8], eax"
            printIntendln ""
            printIntendln "mov ebx, ebp"
            printIntendln "sub ebx, 12"
            printIntendln "invoke ReadConsole, [ebp - 4], [ebp - 8], 10, ebx, NULL"
            printIntendln "mov eax, [ebp - 8]"
            printIntendln "mov ebx, [ebp - 12]"
            printIntendln "mov	byte ptr [eax + ebx - 2], 0"
            printIntendln "invoke atodw, [ebp - 8]"
            printIntendln "push eax"
            printIntendln "invoke HeapFree, heapHandle, 0, [ebp - 8]"
            printIntendln "pop eax"
            printIntendln ""
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"
            
        let printWriteProcCode() =
            let printLengthOfNumber() = 
                println ("getLengthOfNumber:")
                printIntendln ("push ebp; save old ebp value")
                printIntendln ("mov ebp, esp")
                printIntendln ("sub esp, 4")
                printIntendln "mov eax, 1"
                printIntendln "mov [ebp-4], eax"
                printIntendln "mov eax, [ebp + 8]"

                printIntendln "cmp eax, 0"
                printIntendln "je finishGLoN"
                printIntendln "jg divagain"
                printIntendln "mov ebx, 1"
                printIntendln "add [ebp - 4], ebx"
                printIntendln "mov eax, 0"
                printIntendln "sub eax, [ebp + 8]"
                println "  divagain:"
                printIntendln "mov ebx, 10"
                printIntendln "sub edx, edx"
                printIntendln "div ebx"
                printIntendln "cmp eax, 0"
                printIntendln "je finishGLoN"
                printIntendln "mov ebx, 1"
                printIntendln "add [ebp-4], ebx"
                printIntendln "jmp divagain"

                println "  finishGLoN:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"
                printIntendln "ret"
                println ""

            let printWriteNumber() =
                println "writeInt:"
                printIntendln "push ebp		;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 12		"


                printIntendln "invoke GetStdHandle, STD_OUTPUT_HANDLE"
                printIntendln "mov [ebp - 4], eax"
                printIntendln "mov ebx, 0"
                printIntendln "mov [ebp - 8], ebx  ;initialize string length"

                printIntendln "invoke GetProcessHeap"
                printIntendln "mov heapHandle, eax"

                printIntendln "push [ebp + 8]"
                printIntendln "call getLengthOfNumber"
                printIntendln "add esp, 4"

                printIntendln "mov [ebp - 8], eax"

                printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, [ebp - 8]"
                printIntendln "mov [ebp - 12], eax"

                printIntendln "invoke dwtoa, [ebp + 8] , [ebp - 12]"

                printIntendln "invoke WriteConsole, [ebp - 4], [ebp - 12], [ebp - 8], NULL, NULL"
                printIntendln "invoke HeapFree, heapHandle, 0, [ebp - 12]"

                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""
                
            let printWriteBool() =
                println "writeBool:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteString() = 
                println "writeString:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteUnit() =
                println "writeUnit:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteFun() =
                println "writeFun:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteProc() =
                println "%s:" writeProcName
                printIntendln "push ebp		;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 4		; local vars: type"
                printIntendln "mov eax, [ebp + 12]"
                printIntendln "shl eax, 24"
                printIntendln "shr eax, 24"
                printIntendln "mov [ebp - 4], eax"
                printIntendln ""
                printIntendln "wp0:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 0"
                printIntendln "jne wp2"
                printIntendln "push [ebp + 8]"
                printIntendln "call writeInt"
                printIntendln "add esp, 4"
                printIntendln "jmp finishWP"
                printIntendln ""
                printIntendln "wp1:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 1"
                printIntendln "jne wp2"
                printIntendln "push [ebp + 8]"
                printIntendln "call writeBool"
                printIntendln "add esp, 4"
                printIntendln "jmp finishWP"
                printIntendln ""
                printIntendln "wp2:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 2"
                printIntendln "jne wp3"
                printIntendln "push [ebp + 8]"
                printIntendln "call writeString"
                printIntendln "add esp, 4"
                printIntendln "jmp finishWP"
                printIntendln ""
                printIntendln "wp3:	"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 3"
                printIntendln "jne wp4"
                printIntendln "push [ebp + 8]"
                printIntendln "call writeUnit"
                printIntendln "add esp, 4"
                printIntendln "jmp finishWP"
                printIntendln ""
                printIntendln "wp4:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 4"
                printIntendln "jne finishWP"
                printIntendln "push [ebp + 8]"
                printIntendln "call writeFun"
                printIntendln "add esp, 4"
                printIntendln "jmp finishWP"
                printIntendln ""
                printIntendln "finishWP:"
                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"
                printIntendln ""
                printIntendln "ret"
                println ""

            printLengthOfNumber()
            printWriteNumber()
            printWriteBool() 
            printWriteString()
            printWriteUnit()
            printWriteFun()
            printWriteProc()
            
        let printCreateScope n =
            
            let theScope = scopes.[n]
            printIntendln "invoke GetProcessHeap"
            printIntendln "mov heapHandle, eax"
            printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, %d" ((scopeSize n)*4)
            //write self size and self pointer
            printIntendln "mov ebx, %d" (scopeSize n)
            printIntendln "mov [eax], ebx"
            printIntendln "mov [eax + %d], eax ;put ptr to created scope in its field" scopeSelfPtrOffset


        let printCleanCurrentScopeStack() =     
            
            //not sure, if it necessary to invoke GetProcessHeap again to free the memory

            //here should be check if any parameters are ptrs and proper deletion

            printIntendln "mov eax, [esp]"
            printIntendln "invoke HeapFree, heapHandle, 0, eax"
            printIntendln "add esp, 4"

        let rec printBlock (blk : expression list) =
            match blk with
            | expr :: exprs ->
                printExpr expr
                printBlock exprs
            | [] ->
                //don't forget to save return value
                ()

        //evaluate expr and mov result to eax and ebx
        and printExpr = function
        | Statement stmnt ->
            match stmnt with
            | LetAssignment ass ->
                match ass with
                | UsualAssignment ((_, tableId), expression) ->
                    printExpr expression

                    //eax and ebx are occupied
                    printIntendln "mov ecx, [esp]"
                    printIntendln "add ecx, %d" (parameterOffsetInScope !currentScope !tableId)
                    //add copy code
                    printIntendln "mov [ecx], eax"
                    printIntendln "mov [ecx + 4], ebx"
                    printRetUnit()
                | ReadNum (_, tableId) ->
                    printIntendln "call %s" readNumProcName
                    printIntendln "mov ecx, [esp]"
                    printIntendln "add ecx, %d" (parameterOffsetInScope !currentScope !tableId)
                    printIntendln "mov ebx, %d" (typeId IntType)
                    printIntendln "mov [ecx], ebx"
                    printIntendln "mov [ecx + 4], eax"
                    printRetUnit()
                //| ReadLine of varId
                | _ -> raise (NotSupportedYet "CodeGenerator")
            //| LetRecursiveAssignment of (varId * (varId list) * expression * (int ref)) list //int ref is for the scope information
            //| Assignment of assignment
            //| IfStatement of (expression * block * ((expression * block) list) * (block option))
            //| WhileStatement of expression * block
            | WriteStatement expr ->
                printExpr expr
                printPushValueFromRegs()
                printIntendln "%s %s" "call" writeProcName
                printCleanStack 2
                printRetUnit()
                
            //| MatchStatement of varId * (guard list)
            | _ -> raise (NotSupportedYet "CodeGenerator")
        (*| OrList of (expression list)
        | AndList of (expression list)
    
        | Not of expression

        | IsEqual of expression * expression
        | NotEqual of expression * expression
    
        | Greater of expression * expression
        | Less of expression * expression
        | NotGreater of expression * expression
        | NotLess of expression * expression

        | Sum of (sumSign * expression) list
        | Mult of (mulSign * expression) list
        | Mod of expression * expression
    
        | SequenceExpression of block*)
        | ExprId (_, tableId) ->
            printIntendln "mov ecx, [esp]"
            printIntendln "add ecx, %d" (parameterOffsetInScope !currentScope !tableId)
            printIntendln "mov eax, [ecx]"
            printIntendln "mov ebx, [ecx + 4]"
            
        (*| Abstraction of (varId list) * expression * (int ref) //int ref is for the scope information
        | Application of expression * (expression list)*)
        | NumVal n ->
            printRegInt n
        (*| BoolVal of bool
        | StringVal of string
        | UnitVal*)
        | _ -> raise (NotSupportedYet "CodeGenerator")

        //let printReadNumCode() =
        //let printReadLineCode() =
        //let printInitialize
        //let printAbstractionSwitcher() =
        //let printAbstractionBody(scope : Scope) =
        let printMain() =
            currentScope := 0
            println "%s %s" programName "PROC"
            printCreateScope !currentScope
            printIntendln "push eax"

            printBlock(unboxBlock mainBlock)
            printCleanCurrentScopeStack()
            printIntendln("invoke ExitProcess, NULL")
            println "%s %s" programName "ENDP"
        let printEnd() =
            println "%s %s" "end" programName

        println(" .code")
        println ""
        printReadNumProcCode()
        printWriteProcCode()
        printMain()
        printEnd()

    printHeader()
    printIncludes()
    printData()
    printConsts()
    printCode()
    strBuilder.ToString()
    
