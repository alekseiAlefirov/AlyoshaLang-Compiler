module CodeGenerator

open AlyoshaAST
open FunScopes
open Scoper
open VariablesInformation

open System.Text

exception NotSupportedYet of string

type Type =
    | IntType
    | BoolType
    | StringType
    | UnitType
    | FunType
    | RefType

let typeId = function
    | IntType -> 0
    | BoolType -> 1
    | StringType -> 2
    | UnitType -> 3
    | FunType -> 4
    | RefType -> 5

let GenerateCode (ast : AlyoshaAST.program) tableOfSymbols (scopes : Scope []) (stringConstants : StringConstantsDictionary) =

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
        println("heapObjHandle dd ?")
        println("_currentDepth dd 0")
        
        println("")

    let printConsts() =
        let printStringConstants() =
            for i = 0 to stringConstants.Length - 1 do
                println "%s_stringConstant_%d db '%s'" programName i (stringConstants.getString i)
        println(" .const")
        //TODO: print typeIds
        printStringConstants()
        println "unitStringConstant db '()'"
        println "falseStringConstant db 'false'"
        println "trueStringConstant db 'true'"
        println "funStringConstant db 'fun'"
        println "refStringConstant db 'ref'"
        println("")

    let printCode() =
        
        let writeProcName = "_writeProc"
        let readNumProcName = "_readNumProc"
        let isAssignedByte = int (256.0 ** 7.0)
        let ifCounter = ref 0
        let whileCounter = ref 0
        let currentScope = ref 0
        let currentScopePtrEbpOffset = 12
        let scopeSize n =
            let theScope = scopes.[n]
            1 //selfsize
            + 4       //codeId, mutually recursive funs block ptr, self pointer
            + 1     //№ of natural parameters
            + theScope.NaturalParameters.Length * 2
            + 1     //№ of external parameters
            + theScope.ExternalParameters.Length * 2
            + theScope.InnerVariables.Length * 2
        
        let scopeSelfPtrOffset = 4 * 4
        let scopeNumOfNatParamsOffset = 5 * 4
        let scopeNumOfExtParamsOffset n = (6 * 4) + (scopes.[n].NaturalParameters.Length * 4)

        let naturalParameterOffsetInScope n m =
            6 + m*2

        let externalParameterOffsetInScope n m =
            6 + (scopes.[n].NaturalParameters.Length)*2 + 1 + m*2

        let innerVariableOffsetInScope n m =
            6 + (scopes.[n].NaturalParameters.Length)*2 + 1 + (scopes.[n].ExternalParameters.Length)*2 + m*2

        // m for tableId
        let parameterOffsetInScope n m =
            let paramType, num = scopes.[n].InScopeVarTable.[m]
            match paramType with
            | NaturalParameter -> naturalParameterOffsetInScope n num
            | ExternalParameter -> externalParameterOffsetInScope n num
            | InnerVariable -> innerVariableOffsetInScope n num
            | OwnName
            | CoRecursiveFun -> invalidArg "parameterOffsetInScope" ""

        let printPushValueFromRegs() =
            printIntendln "push eax"
            printIntendln "push ebx"

        let printCleanStack n =
            printIntendln "add esp, %d" (n*4)
        
        let printRegInt n =
            printIntendln "mov eax, %d" (typeId IntType) 
            printIntendln "mov ebx, %d" n
            
        let printRetUnit() =
            printIntendln "mov eax, %d" (typeId UnitType) 
            printIntendln "mov ebx, 0"    
        
        let printReadNumProcCode() =
            println "%s:" readNumProcName
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

        
        //TODO: let printReadLineCode() =
            
        let printWriteProcCode() =
            let printLengthOfNumber() = 
                println ("_getLengthOfNumber:")
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
                println "_writeInt:"
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
                printIntendln "call _getLengthOfNumber"
                printIntendln "add esp, 4"

                printIntendln "mov [ebp - 8], eax"

                printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, [ebp - 8] + 1; +1 byte for dwtoa, possibly 0 to end"
                printIntendln "mov [ebp - 12], eax"

                printIntendln "invoke dwtoa, [ebp + 8] , [ebp - 12]"

                printIntendln "invoke WriteConsole, [ebp - 4], [ebp - 12], [ebp - 8], NULL, NULL"
                printIntendln "invoke HeapFree, heapHandle, 0, [ebp - 12]"

                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""
                
            let printWriteBool() =
                println "_writeBool:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 12"

                printIntendln "invoke GetStdHandle, STD_OUTPUT_HANDLE"
                printIntendln "mov [ebp - 4], eax"

                printIntendln "mov eax, [ebp + 8]"
                printIntendln "cmp eax, 0"
                printIntendln "jne writeBoolTrue"
                printIntendln "mov [ebp - 8], offset falseStringConstant"
                printIntendln "mov ebx, 5"
                printIntendln "mov [ebp - 12], ebx"
                printIntendln "jmp writeBoolFinish"
                println "writeBoolTrue:"
                printIntendln "mov [ebp - 8], offset trueStringConstant"
                printIntendln "mov ebx, 4"
                printIntendln "mov [ebp - 12], ebx"                                
                
                println "writeBoolFinish:"
                printIntendln "invoke WriteConsole, [ebp - 4], [ebp - 8], [ebp - 12], NULL, NULL"

                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteString() = 
                println "_writeString:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 8"

                printIntendln "mov eax, [ebp + 12]"
                printIntendln "mov ebx, 0"
                printIntendln "mov bl, ah"
                printIntendln "mov [ebp - 8], ebx"

                printIntendln "invoke GetStdHandle, STD_OUTPUT_HANDLE"
                printIntendln "mov [ebp - 4], eax"

                printIntendln "invoke WriteConsole, [ebp - 4], [ebp + 8], [ebp - 8], NULL, NULL"
                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteUnit() =
                println "_writeUnit:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 12"

                printIntendln "invoke GetStdHandle, STD_OUTPUT_HANDLE"
                printIntendln "mov [ebp - 4], eax"

                printIntendln "mov [ebp - 8], offset unitStringConstant"
                printIntendln "mov ebx, 2"
                printIntendln "mov [ebp - 12], ebx"
                printIntendln "invoke WriteConsole, [ebp - 4], [ebp - 8], [ebp - 12], NULL, NULL"

                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteFun() =
                println "_writeFun:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 12"

                printIntendln "invoke GetStdHandle, STD_OUTPUT_HANDLE"
                printIntendln "mov [ebp - 4], eax"

                printIntendln "mov [ebp - 8], offset funStringConstant"
                printIntendln "mov ebx, 3"
                printIntendln "mov [ebp - 12], ebx"
                printIntendln "invoke WriteConsole, [ebp - 4], [ebp - 8], [ebp - 12], NULL, NULL"

                printIntendln "mov esp, ebp ; restore esp"
                printIntendln "pop ebp"

                printIntendln "ret"
                println ""

            let printWriteRef() =
                println "_writeRef:"
                printIntendln "push ebp;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 12"

                printIntendln "invoke GetStdHandle, STD_OUTPUT_HANDLE"
                printIntendln "mov [ebp - 4], eax"

                printIntendln "mov [ebp - 8], offset refStringConstant"
                printIntendln "mov ebx, 3"
                printIntendln "mov [ebp - 12], ebx"
                printIntendln "invoke WriteConsole, [ebp - 4], [ebp - 8], [ebp - 12], NULL, NULL"

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
                printIntendln "mov ebx, 0"
                printIntendln "mov bl, al"
                printIntendln "mov [ebp - 4], ebx"
                printIntendln ""
                printIntendln "_wp0:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 0"
                printIntendln "jne _wp1"
                printIntendln "push [ebp + 8]"
                printIntendln "call _writeInt"
                printIntendln "add esp, 4"
                printIntendln "jmp _finishWP"
                printIntendln ""
                printIntendln "_wp1:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 1"
                printIntendln "jne _wp2"
                printIntendln "push [ebp + 8]"
                printIntendln "call _writeBool"
                printIntendln "add esp, 4"
                printIntendln "jmp _finishWP"
                printIntendln ""
                printIntendln "_wp2:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 2"
                printIntendln "jne _wp3"
                printIntendln "push [ebp + 12]"
                printIntendln "push [ebp + 8]"
                printIntendln "call _writeString"
                printIntendln "add esp, 4"
                printIntendln "jmp _finishWP"
                printIntendln ""
                printIntendln "_wp3:	"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 3"
                printIntendln "jne _wp4"
                printIntendln "push [ebp + 8]"
                printIntendln "call _writeUnit"
                printIntendln "add esp, 4"
                printIntendln "jmp _finishWP"
                printIntendln ""
                printIntendln "_wp4:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 4"
                printIntendln "jne _wp5"
                printIntendln "push [ebp + 8]"
                printIntendln "call _writeFun"
                printIntendln "add esp, 4"
                printIntendln "jmp _finishWP"
                printIntendln ""
                printIntendln "_wp5:"
                printIntendln "mov eax, [ebp - 4]"
                printIntendln "cmp eax, 5"
                printIntendln "jne _finishWP"
                printIntendln "push [ebp + 8]"
                printIntendln "call _writeRef"
                printIntendln "add esp, 4"
                printIntendln "jmp _finishWP"
                printIntendln ""
                printIntendln "_finishWP:"
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
            printWriteRef()
            printWriteProc()
            
        
        let printAddNewObjCode() =
            println "_addNewObj:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            printIntendln "sub esp, 8; ptr to last objRecord + new obj"
            
            printIntendln "mov ebx, heapObjHandle"
            printIntendln "mov ebx, [ebx]"
            printIntendln "cmp ebx, 0"
            printIntendln "jne _objHandleIsNotEmpty"
            printIntendln "mov ebx, heapObjHandle"
            printIntendln "_objHandleIsNotEmpty:"
            printIntendln "mov [ebp - 4], ebx"
            printIntendln "invoke HeapAlloc, heapHandle, 9, 16"
            printIntendln "mov [ebp - 8], eax"

            printIntendln "mov ebx, heapObjHandle"
            printIntendln "mov [ebx], eax"

            printIntendln "mov ebx, [ebp + 12]"
            printIntendln "mov [eax], ebx"
            printIntendln "mov ebx, [ebp + 8]"
            printIntendln "mov [eax + 4], ebx"
            printIntendln "mov ebx, [ebp - 4]"
            printIntendln "mov [eax + 8], ebx"
            printIntendln "mov ebx, _currentDepth"
            printIntendln "mov [eax + 12], ebx"

            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln "ret"
            printIntendln ""

        let printCopyObjCode() =
            println "_copyObj:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            
            printIntendln "sub esp, 8"
            printIntendln "mov ebx, [ebp + 12]"
            printIntendln "shl ebx, 8 ; in case it was assigned"
            printIntendln "shr ebx, 8"
            printIntendln "mov [ebp - 4], ebx"
            printIntendln "mov ebx, [ebp + 8]"
            printIntendln "mov [ebp - 8], ebx"
            
            printIntendln "mov ebx, [ebp - 4]"
            printIntendln "_copyIfFun:"
            printIntendln "cmp bl, %d" (typeId FunType)
            printIntendln "jne _copyIfString"
            printIntendln ";TODO fun copy"
            printIntendln "jmp _endObjCopy"

            printIntendln "_copyIfString:"
            printIntendln "cmp bl, %d" (typeId StringType)
            printIntendln "jne _copyIfRef"
            printIntendln "mov eax, 0"
            printIntendln "mov al, bh"
            printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, eax"
            printIntendln "mov [ebp - 8], eax"
            printIntendln "mov edi, eax"
            printIntendln "mov esi, [ebp + 8]"
            printIntendln "mov eax, [ebp + 12]"
            printIntendln "mov ecx, 0"
            printIntendln "mov cl, ah"
            printIntendln "rep movsb"
            printIntendln "jmp _endObjCopy"

            printIntendln "_copyIfRef:"
            printIntendln "cmp bl, %d" (typeId RefType)
            printIntendln "jne _endObjCopy"
            printIntendln "mov eax, [ebp - 8]"
            printIntendln "mov ebx, [eax]"
            printIntendln "add ebx, 1"
            printIntendln "mov [eax], ebx"
            printIntendln "jmp _endObjCopy"

            printIntendln "_endObjCopy: "
            
            printIntendln "mov eax, [ebp - 4]"
            printIntendln "mov ebx, [ebp - 8]"
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln "ret"
            printIntendln ""

        let printAssignObjCode() =
            println "_assignObj:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"

            printIntendln "mov eax, [ebp + 12]"
            printIntendln "rol eax, 8"
            printIntendln "cmp al, 0"
            printIntendln "jne _isAlreadyAssigned"
            printIntendln "mov eax, [ebp + 12]"
            printIntendln "add eax, %d" isAssignedByte
            printIntendln "mov ebx, [ebp + 8]"
            printIntendln "jmp _retAssignedValue"

            printIntendln "_isAlreadyAssigned:"
            printIntendln "push [ebp + 12]"
            printIntendln "push [ebp + 8]"
            printIntendln "call _copyObj"
            printIntendln "add esp, 8"
            printIntendln "add eax, %d" isAssignedByte

            printIntendln "push eax"
            printIntendln "push ebx"
            printIntendln "call _addNewObj"
            printIntendln "pop ebx"
            printIntendln "pop eax"

            printIntendln "_retAssignedValue: "
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln "ret"
            printIntendln ""

        let printCleanHeapObjCurrentDepthCode() =
            println "_cleanHeapObjHandleCurrentDepth:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            printIntendln "sub esp, 4   ;pointer to last obj"

            printIntendln "mov ebx, heapObjHandle"
            printIntendln "mov ebx, [ebx]"
            printIntendln "mov [ebp - 4], ebx"
            
            printIntendln "_deleteObjRecordsLoop:"
            printIntendln "mov eax, [ebp - 4]"
            printIntendln "cmp eax, 0"
            printIntendln "je _endDeleteObjRecords"

            printIntendln "mov ebx, [eax + 12]"
            printIntendln "mov ecx, _currentDepth"
            printIntendln "cmp ebx, ecx"
            printIntendln "jne _endDeleteObjRecords"
            printIntendln "mov ecx, [eax + 8]"
            printIntendln "mov [ebp - 4], ecx"
            printIntendln "push [eax]"
            printIntendln "push [eax + 4]"
            printIntendln "call _deleteObj"
            printIntendln "add esp, 8"

            printIntendln "_endDeleteObjRecords:"
            printIntendln "mov ecx, [ebp - 4]"
            printIntendln "mov ebx, heapObjHandle"
            printIntendln "mov ebx, ecx"

            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"

        let printDeleteObjCode() =
            println "_deleteObj:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"

            printIntendln "mov ebx, [ebp + 12]"
            printIntendln "_deleteIfFun:"
            printIntendln "cmp bl, %d" (typeId FunType)
            printIntendln "jne _deleteIfString"
            printIntendln ";TODO fun deletion"
            printIntendln "jmp _endObjDeletion"

            printIntendln "_deleteIfString:"
            printIntendln "cmp bl, %d" (typeId StringType)
            printIntendln "jne _deleteIfRef"
            printIntendln "invoke HeapFree, heapHandle, 0, [ebp + 8]"

            printIntendln "_deleteIfRef:"
            printIntendln "cmp bl, %d" (typeId RefType)
            printIntendln "jne _endObjDeletion"
            printIntendln "mov eax, [ebp + 8]"
            printIntendln "mov ebx, [eax]"
            printIntendln "sub ebx, 1"
            printIntendln "jz _deleteRefObj"
            printIntendln "mov [eax], ebx"
            printIntendln "jmp _endObjDeletion"
            printIntendln "_deleteRefObj:"
            printIntendln "push [eax + 4]"
            printIntendln "push [eax + 8]"
            printIntendln "call _deleteObj"
            printIntendln "add esp, 8"
            printIntendln "mov eax, [ebp + 8]"
            printIntendln "invoke HeapFree, heapHandle, 0, eax"

            printIntendln "_endObjDeletion: "
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"

        let printIsEqualCode() =
            println "_isEqual:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            printIntendln "mov eax, [ebp + 20]"
            printIntendln "mov ebx, [ebp + 12]"
            printIntendln "mov ecx, [ebp + 16]"
            printIntendln "mov edx, [ebp + 8]"

            printIntendln "cmp al, %d" (typeId IntType)
            printIntendln "jne _isEqualIfBool"
            printIntendln "cmp ecx, edx"
            printIntendln "je _isEqualRetTrue"
            printIntendln "jmp _isEqualRetFalse"

            printIntendln "_isEqualIfBool:"
            printIntendln "cmp al, %d" (typeId BoolType)
            printIntendln "jne _isEqualIfString"
            printIntendln "cmp ecx, edx"
            printIntendln "je _isEqualRetTrue"
            printIntendln "jmp _isEqualRetFalse"

            printIntendln "_isEqualIfString:"
            printIntendln "cmp al, %d" (typeId StringType)
            printIntendln "jne _isEqualIfUnit"
            printIntendln "cmp ah, bh"
            printIntendln "jne _isEqualRetFalse"
            printIntendln "mov eax, 0"
            printIntendln "mov al, bh"
            printIntendln "jmp _stringCompareCond"
            printIntendln "_stringCompareLoop:"
            printIntendln "sub al, 1"
            printIntendln "mov esi, ecx"
            printIntendln "add esi, eax"
            printIntendln "mov edi, edx"
            printIntendln "add edi, eax"

            printIntendln "mov ebx, 0"
            printIntendln "mov bh, byte ptr [esi]"
            printIntendln "mov bl, byte ptr [edi]"
            printIntendln "cmp bh, bl"
            printIntendln "jne _isEqualRetFalse"

            printIntendln "_stringCompareCond:"
            printIntendln "cmp al, 0"
            printIntendln "jne _stringCompareLoop"
            printIntendln "jmp _isEqualRetTrue"

            printIntendln "_isEqualIfUnit:"
            printIntendln "cmp al, %d" (typeId UnitType)
            printIntendln "jne _isEqualIfFun"
            printIntendln "jmp _isEqualRetTrue"

            printIntendln "_isEqualIfFun:"
            printIntendln "cmp al, %d" (typeId FunType)
            printIntendln "jne _isEqualIfRef"
            printIntendln "cmp ecx, edx"
            printIntendln "je _isEqualRetTrue"
            printIntendln "jmp _isEqualRetFalse"

            printIntendln "_isEqualIfRef:"
            printIntendln "cmp al, %d" (typeId FunType)
            printIntendln "jne _isEqualRetFalse"
            printIntendln "cmp ecx, edx"
            printIntendln "je _isEqualRetTrue"
            printIntendln "jmp _isEqualRetFalse"

            printIntendln "_isEqualRetTrue:"
            printIntendln "mov eax, %d" (typeId BoolType)
            printIntendln "mov ebx, 1"
            printIntendln "jmp _isEqualEnd"
            printIntendln "_isEqualRetFalse:"
            printIntendln "mov eax, %d" (typeId BoolType)
            printIntendln "mov ebx, 0"

            printIntendln "_isEqualEnd:"
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"

        let printGreaterCode() =
            println "_greaterOp:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"

            printIntendln "mov eax, [ebp + 12]"
            printIntendln "mov ebx, [ebp + 8]"

            printIntendln "cmp eax, ebx"
            printIntendln "jg _isGreater"
            printIntendln "mov ebx, 0 ;false"
            printIntendln "jmp _greaterOpFinish"

            printIntendln "_isGreater:"
            printIntendln "mov ebx, 1 ;true"

            printIntendln "_greaterOpFinish:"
            printIntendln "mov eax, %d" (typeId BoolType)
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"
            
        let printLessCode() =
            println "_lessOp:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"

            printIntendln "mov eax, [ebp + 12]"
            printIntendln "mov ebx, [ebp + 8]"

            printIntendln "cmp eax, ebx"
            printIntendln "jl _isLess"
            printIntendln "mov ebx, 0 ;false"
            printIntendln "jmp _lessOpFinish"

            printIntendln "_isLess:"
            printIntendln "mov ebx, 1 ;true"

            printIntendln "_lessOpFinish:"
            printIntendln "mov eax, %d" (typeId BoolType)
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"

        let printNotGreaterCode() =
            println "_notGreaterOp:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"

            printIntendln "mov eax, [ebp + 12]"
            printIntendln "mov ebx, [ebp + 8]"

            printIntendln "cmp eax, ebx"
            printIntendln "jng _isNotGreater"
            printIntendln "mov ebx, 0 ;false"
            printIntendln "jmp _notGreaterOpFinish"

            printIntendln "_isNotGreater:"
            printIntendln "mov ebx, 1 ;true"

            printIntendln "_notGreaterOpFinish:"
            printIntendln "mov eax, %d" (typeId BoolType)

            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"

        let printNotLessCode() =
            println "_notLessOp:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"

            printIntendln "mov eax, [ebp + 12]"
            printIntendln "mov ebx, [ebp + 8]"

            printIntendln "cmp eax, ebx"
            printIntendln "jnl _isNotLess"
            printIntendln "mov ebx, 0 ;false"
            printIntendln "jmp _notLessOpFinish"

            printIntendln "_isNotLess:"
            printIntendln "mov ebx, 1 ;true"

            printIntendln "_notLessOpFinish:"
            printIntendln "mov eax, %d" (typeId BoolType)
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"


        let printStringConcat () =
            println "_stringConcat:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            printIntendln "sub esp, 8; length and ptr to new string"
            //collect length
            printIntendln "mov eax, 0; number of strings"
            printIntendln "mov ebx, 0; total length"
            printIntendln "mov edx, ebp; position of string value in stack"
            printIntendln "add edx, 8"
            printIntendln "_collectStringLengthLoop:"
            printIntendln "add eax, 1"
            printIntendln "add edx, 8"
            
            printIntendln "mov ecx, [edx]"
            printIntendln "mov cl, ch"
            printIntendln "shl ecx, 24"
            printIntendln "shr ecx, 24"
            printIntendln "add ebx, ecx"

            printIntendln "mov ecx, [ebp + 8]"
            printIntendln "cmp eax, ecx"
            printIntendln "jne _collectStringLengthLoop"
            printIntendln "mov [ebp - 4], ebx"

            //not sure heapHandle should be re-initialized
            printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, [ebp - 4]"
            printIntendln "mov [ebp - 8], eax"

            //collect res string
            printIntendln "mov eax, 0; number of strings"
            printIntendln "mov ebx, [ebp - 8]; position in res string"
            printIntendln "mov edx, ebp; position of 64b string value in stack"
            printIntendln "add edx, 8"
            printIntendln "_collectStringLoop:"
            printIntendln "add eax, 1"
            printIntendln "add edx, 8"
            
            printIntendln "mov edi, ebx"

            printIntendln "mov ecx, [edx]"
            printIntendln "mov cl, ch"
            printIntendln "shl ecx, 24"
            printIntendln "shr ecx, 24"
            printIntendln "add ebx, ecx"

            printIntendln "mov esi, [edx - 4]"

            printIntendln "rep movsb"

            printIntendln "mov ecx, [ebp + 8]"
            printIntendln "cmp eax, ecx"
            printIntendln "jne _collectStringLoop"

            //res
            printIntendln "mov eax, [ebp - 4]"
            printIntendln "shl eax, 8"
            printIntendln "mov al, %d" (typeId StringType)
            printIntendln "mov ebx, [ebp - 8]"

            //add an obj record
            printIntendln "push eax"
            printIntendln "push ebx"
            printIntendln "call _addNewObj"
            printIntendln "pop ebx"
            printIntendln "pop eax"

            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"
           
        let printMakeARefCode() =
            println "_makeARef:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            printIntendln "sub esp, 8"

            printIntendln "push [ebp + 12]"
            printIntendln "push [ebp + 8]"
            printIntendln "call _copyObj"
            printIntendln "add esp, 8"
            printIntendln "mov [ebp - 4], eax"
            printIntendln "mov [ebp - 8], ebx"

            printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, 12" //first dword for refcounter

            printIntendln "mov ebx, 1"
            printIntendln "mov [eax], ebx"
            printIntendln "mov ebx, [ebp - 4]"
            printIntendln "mov [eax + 4], ebx"
            printIntendln "mov ebx, [ebp - 8]"
            printIntendln "mov [eax + 8], ebx"

            printIntendln "mov ebx, eax"
            printIntendln "mov eax, %d" (typeId RefType)

            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"

        let printUnrefCode() =
            println "_unref:"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"

            printIntendln "mov eax, [ebp + 12]"
            printIntendln "rol eax, 8"
            printIntendln "mov al, 0"
            printIntendln "jne _endUnref"
            printIntendln "push [ebp + 12]"
            printIntendln "push [ebp + 8]"
            printIntendln "call _deleteObj"
            printIntendln "add esp, 8"

            printIntendln "_endUnref:"
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""
            printIntendln "ret"
        
        let printCreateScope n =
            
            let theScope = scopes.[n]

            printIntendln "invoke GetProcessHeap"
            printIntendln "mov heapHandle, eax"
            printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE, %d" ((scopeSize n)*4)
            //write self size and self pointer
            printIntendln "mov ebx, %d" (scopeSize n)
            printIntendln "mov [eax], ebx"
            printIntendln "mov [eax + %d], eax ;put ptr to created scope in its field" scopeSelfPtrOffset

            printIntendln "mov ebx, %d" (theScope.NaturalParameters.Length)
            printIntendln "mov [eax + %d], ebx ;put number of its natural parameters" scopeNumOfNatParamsOffset

            printIntendln "mov ebx, %d" (theScope.ExternalParameters.Length)
            printIntendln "mov [eax + %d], ebx ;put number of its external parameters" (scopeNumOfExtParamsOffset n)

        let printCleanCurrentScopeStack() =     
            
            //not sure, if it is necessary to invoke GetProcessHeap again to free the memory

            printIntendln "push eax"
            printIntendln "push ebx"
            printIntendln "call _copyObj"
            printIntendln "add esp, 8"
            printIntendln "mov [ebp - 4], eax"
            printIntendln "mov [ebp - 8], ebx"            
                        
            printIntendln "call _cleanHeapObjHandleCurrentDepth"

            printIntendln "pop eax" //ptr to this function
            printIntendln "sub _currentDepth, 1"
            //TODO: simple values should not be added
            printIntendln "call _addNewObj"
            printIntendln "pop ebx"
            printIntendln "pop eax"
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln ""

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

                    printIntendln "push eax"
                    printIntendln "push ebx"
                    printIntendln "call _assignObj"
                    printIntendln "add esp, 8"
                    //eax and ebx are occupied
                    printIntendln "mov ecx, [ebp - %d]" currentScopePtrEbpOffset//offset to this scope ptr
                    printIntendln "add ecx, %d" ((parameterOffsetInScope !currentScope !tableId) * 4)
                    printIntendln "mov [ecx], eax"
                    printIntendln "mov [ecx + 4], ebx"
                    printRetUnit()
                | ReadNum (_, tableId) ->
                    printIntendln "call %s" readNumProcName
                    printIntendln "mov ecx, [ebp - %d]" currentScopePtrEbpOffset//offset to this scope ptr
                    printIntendln "add ecx, %d" ((parameterOffsetInScope !currentScope !tableId) * 4)
                    printIntendln "mov ebx, %d" (typeId IntType)
                    printIntendln "mov [ecx], ebx"
                    printIntendln "mov [ecx + 4], eax"
                    printRetUnit()
                //| ReadLine of varId
                | _ -> raise (NotSupportedYet "CodeGenerator")
            //| LetRecursiveAssignment of (varId * (varId list) * expression * (int ref)) list //int ref is for the scope information
            | Reassignment ass ->
                match ass with
                | UsualAssignment ((_, tableId), expression) ->
                    printExpr expression

                    printIntendln "push eax"
                    printIntendln "push ebx"

                    //delete old value
                    let pois = ((parameterOffsetInScope !currentScope !tableId) * 4)
                    printIntendln "mov ecx, [ebp - %d]" currentScopePtrEbpOffset//offset to this scope ptr
                    printIntendln "add ecx, %d" pois
                    printIntendln "push [ecx]"
                    printIntendln "push [ecx + 4]"
                    printIntendln "call _deleteObj"
                    printIntendln "add esp, 8"


                    printIntendln "call _assignObj"
                    printIntendln "add esp, 8"
                    
                    printIntendln "push eax"
                    printIntendln "push ebx"
                    printIntendln "call _makeARef"
                    printIntendln "add esp, 8"

                    printIntendln "add eax, %d" isAssignedByte
                    //eax and ebx are occupied
                    printIntendln "mov ecx, [ebp - %d]" currentScopePtrEbpOffset//offset to this scope ptr
                    printIntendln "add ecx, %d" pois
                    printIntendln "mov [ecx], eax"
                    printIntendln "mov [ecx + 4], ebx"
                    printRetUnit()
                | ReadNum (_, tableId) ->
                    printIntendln "call %s" readNumProcName
                    printIntendln "mov ecx, [ebp - %d]" currentScopePtrEbpOffset//offset to this scope ptr
                    printIntendln "add ecx, %d" ((parameterOffsetInScope !currentScope !tableId) * 4)
                    printIntendln "mov ebx, %d" (typeId IntType)
                    printIntendln "mov [ecx], ebx"
                    printIntendln "mov [ecx + 4], eax"
                    printRetUnit()
                //| ReadLine of varId
                | _ -> raise (NotSupportedYet "CodeGenerator")
            | IfStatement (condition, trueBlock, elifList, elseBlock) ->
                let elifCounter = ref 0
                printExpr condition
                printIntendln "cmp ebx, 0"
                match elifList, elseBlock with
                | _ :: _, _ -> 
                    printIntendln "je _elif_%d_0" !ifCounter
                | [], Some _ ->
                    printIntendln "je _else_%d" !ifCounter
                | [], None ->
                    printIntendln "_if_%d_ended" !ifCounter

                printBlock (unboxBlock trueBlock)
                printIntendln "jmp _if_%d_ended" !ifCounter

                let rec elifListPrint = function
                    | (condition, body) :: [] ->
                        printIntendln "_elif_%d_%d:" !ifCounter !elifCounter
                        printExpr condition
                        printIntendln "cmp ebx, 0"
                        match elseBlock with
                        | Some x ->
                            printIntendln "je _else_%d" !ifCounter
                        | None -> printIntendln "je _if_%d_ended" !ifCounter
                        printBlock (unboxBlock body)
                        printIntendln "jmp _if_%d_ended" !ifCounter
                        incr elifCounter
                    | (condition, body) :: t ->
                        printIntendln "_elif_%d_%d:" !ifCounter !elifCounter
                        printExpr condition
                        printIntendln "cmp ebx, 0"
                        match elseBlock with
                        | Some x ->
                            printIntendln "je _else_%d" !ifCounter
                        | None -> printIntendln "je _elif_%d_%d" !ifCounter (!elifCounter + 1)
                        printBlock (unboxBlock body)
                        printIntendln "jmp _if_%d_ended" !ifCounter
                        incr elifCounter
                    | [] -> ()                
                elifListPrint elifList
                
                match elseBlock with
                | Some blk ->
                    printIntendln "_else_%d:" !ifCounter
                    printBlock (unboxBlock blk)
                | None -> ()

                printIntendln "_if_%d_ended:" !ifCounter
                incr ifCounter

            | WhileStatement (expr, (Block blk)) ->
                let conditionLabel = sprintf "_while_%d_cond" !whileCounter
                let bodyLabel = sprintf "_while_%d_body" !whileCounter
                printIntendln "jmp %s" conditionLabel
                printIntendln "%s:" bodyLabel
                printBlock blk
                printIntendln "%s:" conditionLabel
                printExpr expr
                printIntendln "cmp ebx, 1"
                printIntendln "je %s" bodyLabel
                printRetUnit()                
                incr whileCounter
            | WriteStatement expr ->
                printExpr expr
                printPushValueFromRegs()
                printIntendln "%s %s" "call" writeProcName
                printCleanStack 2
                printRetUnit()
                
            //| MatchStatement of varId * (guard list)
            | _ -> raise (NotSupportedYet "CodeGenerator")
        | OrList elist ->
            match elist with
            | e :: es ->
                printExpr e
                let rec f = function
                | e :: es -> 
                    printIntendln "push ebx"
                    printExpr e
                    printIntendln "pop eax"
                    printIntendln "or ebx, eax"
                    f es
                | [] -> 
                    printIntendln "mov eax, %d" (typeId BoolType)
                f es
            | [] -> () //should not happen

        | AndList elist ->
            match elist with
            | e :: es ->
                printExpr e
                let rec f = function
                | e :: es -> 
                    printIntendln "push ebx"
                    printExpr e
                    printIntendln "pop eax"
                    printIntendln "and ebx, eax"
                    f es
                | [] -> 
                    printIntendln "mov eax, %d" (typeId BoolType)
                f es
            | [] -> () //should not happen
    
        | Not expr ->
            printExpr expr
            printIntendln "not ebx"

        | IsEqual (expr1, expr2) ->
            printExpr expr1
            printIntendln "push eax"
            printIntendln "push ebx"
            printExpr expr2
            printIntendln "push eax"
            printIntendln "push ebx"
            printIntendln "call _isEqual"
            printIntendln "add esp, 16"
        | NotEqual (expr1, expr2) ->
            printExpr (Not (IsEqual (expr1, expr2)))
    
        | Greater (expr1, expr2) ->
            printExpr expr1
            printIntendln "push ebx"
            printExpr expr2
            printIntendln "push ebx"
            printIntendln "call _greaterOp"
            printIntendln "add esp, 8"
        | Less  (expr1, expr2) ->
            printExpr expr1
            printIntendln "push ebx"
            printExpr expr2
            printIntendln "push ebx"
            printIntendln "call _lessOp"
            printIntendln "add esp, 8"
        | NotGreater  (expr1, expr2) ->
            printExpr expr1
            printIntendln "push ebx"
            printExpr expr2
            printIntendln "push ebx"
            printIntendln "call _notGreaterOp"
            printIntendln "add esp, 8"
        | NotLess  (expr1, expr2) ->
            printExpr expr1
            printIntendln "push ebx"
            printExpr expr2
            printIntendln "push ebx"
            printIntendln "call _notLessOp"
            printIntendln "add esp, 8"

        | Sum terms ->
            match terms with
            | (_, expr) :: t -> 
                printExpr expr //first expr always >=0
                let rec f = function
                | (sign, expr) :: t ->
                    printIntendln "push ebx"
                    printExpr expr
                    printIntendln "pop eax"
                    match sign with
                    | Plus ->
                        printIntendln "add eax, ebx"
                    | Minus ->
                        printIntendln "sub eax, ebx"
                    printIntendln "mov ebx, eax"
                    f t
                    printIntendln "mov eax, %d" (typeId IntType)
                | [] -> ()
                f t
            | _ -> () //should not happen


        | Mult factors ->
            match factors with
            | (_, expr) :: t -> 
                printExpr expr //first expr always *
                let rec f = function
                | (sign, expr) :: t ->
                    printIntendln "push ebx"
                    printExpr expr
                    printIntendln "pop eax"
                    match sign with
                    | Mul ->
                        printIntendln "imul eax, ebx"
                    | Div ->
                        printIntendln "mov edx, 0"
                        printIntendln "idiv ebx"
                    printIntendln "mov ebx, eax"
                    f t
                    printIntendln "mov eax, %d" (typeId IntType)
                | [] -> ()
                f t
            | _ -> () //should not happen
        | StringConcat strings ->
            let count = ref 0
            let rec f = function
            | [] -> ()
            | s :: ss ->
                incr count
                f ss
                printExpr s
                printIntendln "push eax"
                printIntendln "push ebx"
            f strings
            printIntendln "push %d" !count
            printIntendln "call _stringConcat"
            printIntendln "add esp, %d" (((!count)*2 + 1)*4)

                
        | Mod (expr1, expr2) ->
            printExpr expr1
            printIntendln "push ebx"
            printExpr expr2
            printIntendln "pop eax"
            printIntendln "mov edx, 0"
            printIntendln "idiv ebx"
            printIntendln "mov ebx, edx"
            printIntendln "mov eax, %d" (typeId IntType)
    
        | SequenceExpression (Block blk) ->
            printBlock blk
        | ExprId (_, tableId) ->
            printIntendln "mov ecx, [ebp - %d]" currentScopePtrEbpOffset
            printIntendln "add ecx, %d" ((parameterOffsetInScope !currentScope !tableId) * 4)
            printIntendln "mov eax, [ecx]"
            printIntendln "mov ebx, [ecx + 4]"
            
        (*| Abstraction of (varId list) * expression * (int ref) //int ref is for the scope information
        | Application of expression * (expression list) ->
            TODO: here parameters should be put*)
        | Reference expr ->
            printExpr expr
            printIntendln "push eax"
            printIntendln "push ebx"
            printIntendln "call _makeARef"
            printIntendln "add esp, 8"
        | Unref expr ->
            printExpr expr
            printIntendln "push [ebx + 4]"
            printIntendln "push [ebx + 8]"
            printIntendln "push eax"
            printIntendln "push ebx"
            printIntendln "call _unref"
            printIntendln "add esp, 8"
            printIntendln "pop ebx"
            printIntendln "pop eax"

        | NumVal n ->
            printRegInt n
        | BoolVal b ->
            printIntendln "mov eax, %d" (typeId BoolType)
            match b with
            | true -> printIntendln "mov ebx, 1"
            | false -> printIntendln "mov ebx, 0"
        | StringVal str ->
            printIntendln "mov eax, %d" (isAssignedByte + str.Length * 256 + (typeId StringType))
            printIntendln "mov ebx, offset %s_stringConstant_%d" programName (stringConstants.getIndex str)
        | UnitVal ->
            printIntendln "mov eax, %d" (typeId UnitType)
            printIntendln "mov ebx, 0"
        | _ -> raise (NotSupportedYet "CodeGenerator")

        let printAbstractionSwitcher() =
            println "%s_abstraction_switch:" programName
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            for i = 1 to scopes.Length - 2 do
                printIntendln "_switch_%d:" i
                printIntendln "mov eax, [ebp + 8]"
                printIntendln "cmp eax, %d" i
                printIntendln "jne %s%d" "_switch_" (i+1)
                printIntendln "call %s_abstraction_scope_%d" programName i
                printIntendln "jmp _finish_abstraction_switch"

            if scopes.Length > 1 then
                printIntendln "_switch_%d:" (scopes.Length - 1)
                printIntendln "mov eax, [ebp + 8]"
                printIntendln "cmp eax, %d" (scopes.Length - 1)
                printIntendln "jne %s" "_finish_abstraction_switch"
                printIntendln "call %s_abstraction_scope_%d" programName (scopes.Length - 1)
                printIntendln "jmp _finish_abstraction_switch"

            printIntendln "_finish_abstraction_switch:"
            printIntendln "mov esp, ebp ; restore esp"
            printIntendln "pop ebp"
            printIntendln "ret"
            printIntendln ""

        
        let printAbstractionBodies() =
            for i = 1 to scopes.Length - 1 do
                println "%s_abstraction_scope_%d:" programName i
                printIntendln "push ebp		;save old ebp value"
                printIntendln "mov ebp, esp	;save pointer to this frame value"
                printIntendln "sub esp, 8 ;space for result"
                printIntendln "mov eax, [ebp + 12]"
                printIntendln "push eax"
                printExpr scopes.[i].Body
                printCleanCurrentScopeStack()
                println "ret"
                println ""
        
        let printMain() =
            
            let printInitializeHeapObjHandle() =
                printIntendln "invoke GetProcessHeap"
                printIntendln "mov heapHandle, eax"
                printIntendln ""
                printIntendln "invoke HeapAlloc, heapHandle, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, 4"
                printIntendln "mov heapObjHandle, eax"
            let printCloseHeapObjHandle() = 
                printIntendln "invoke HeapFree, heapHandle, 0, heapObjHandle"

            currentScope := 0
            println "%s %s" programName "PROC"
            printIntendln "push ebp		;save old ebp value"
            printIntendln "mov ebp, esp	;save pointer to this frame value"
            printInitializeHeapObjHandle()
            printCreateScope !currentScope
            printIntendln "sub esp, 8 ;space for result"
            printIntendln "push eax"

            printBlock(unboxBlock mainBlock)
            printCleanCurrentScopeStack()
            printCloseHeapObjHandle()
            printIntendln("invoke ExitProcess, NULL")
            println "%s %s" programName "ENDP"
        let printEnd() =
            println "%s %s" "end" programName

        println(" .code")
        println ""
        printReadNumProcCode()
        printWriteProcCode()
        printCopyObjCode()
        printAssignObjCode()
        printAddNewObjCode()
        printDeleteObjCode()
        printCleanHeapObjCurrentDepthCode()
        printIsEqualCode()
        printGreaterCode()
        printLessCode()
        printNotGreaterCode()
        printNotLessCode()
        printStringConcat()
        printMakeARefCode()
        printUnrefCode()
        printMain()
        printEnd()

    printHeader()
    printIncludes()
    printData()
    printConsts()
    printCode()
    strBuilder.ToString()
    
