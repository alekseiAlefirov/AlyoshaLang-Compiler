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

let GenerateCode (ast : AlyoshaAST.program) tableOfSymbols scopes stringConstants =

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

        (*let printSinglePush x =
            println "%s %s" "push" x*)

        let printPushValueFromRegs() =
            println "push eax"
            println "push ebx"

        (*let printClean() =
            println "mov es"*)

        let printCleanStack n =
            println "add esp, %d" (n*4)

        (*let printPushUnit () =
            printSinglePush "0"
            printSinglePush "0" *)
        
        let printRegInt n =
            println "mov eax, %d" (typeId IntType) 
            println "mov ebx, %d" n
            
        let printRetUnit() =
            println "mov eax, %d" (typeId UnitType) 
            println "mov ebx, 0"    
            
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
            
        
        let printCleanScopeCode () =
            //
            ()       

        let rec printBlock (blk : expression list) =
            match blk with
            | expr :: exprs ->
                printExpr expr
                printBlock exprs
            | [] ->
                //don't forget to save return value
                printCleanScopeCode()

        //evaluate expr and mov result to eax and ebx
        and printExpr = function
        | Statement stmnt ->
            match stmnt with
            //| LetAssignment of assignment
            //| LetRecursiveAssignment of (varId * (varId list) * expression * (int ref)) list //int ref is for the scope information
            //| Assignment of assignment
            //| IfStatement of (expression * block * ((expression * block) list) * (block option))
            //| WhileStatement of expression * block
            | WriteStatement expr ->
                printExpr expr
                printPushValueFromRegs()
                println "%s %s" "call" writeProcName
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
    
        | SequenceExpression of block
        | ExprId of varId
        | Abstraction of (varId list) * expression * (int ref) //int ref is for the scope information
        | Application of expression * (expression list)*)
        | NumVal n ->
            printRegInt n
        (*| BoolVal of bool
        | StringVal of string
        | UnitVal*)
        | _ -> raise (NotSupportedYet "CodeGenerator")

        //let printWriteCode() =
        //let printReadNumCode() =
        //let printReadLineCode() =
        //let printAbstractionSwitcher() =
        //let printAbstractionBody(scope : Scope) =
        let printMain() =
            println "%s %s" programName "PROC"
            printBlock(unboxBlock mainBlock)
            //clean after
            println "%s %s" programName "ENDP"
        let printEnd() =
            println "%s %s" "end" programName

        println(" .code")
        println ""
        printWriteProcCode()
        printMain()
        printEnd()

    printHeader()
    printIncludes()
    printData()
    printConsts()
    printCode()
    strBuilder.ToString()
    
