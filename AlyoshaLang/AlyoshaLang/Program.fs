// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Text.Lexing
open System.IO

open AlyoshaAST
open AlyoshaParser
open AlyoshaLexer
open Typer
open Scoper
open CodeGenerator
open Conversions


let parseFromFile (fileName : string) =
    let fileReader = new StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader(fileReader)
    let ast = AlyoshaParser.start AlyoshaLexer.tokenize lexbuf
    ast

let parseFromString code =
    let lexbuf = LexBuffer<char>.FromString(code)
    let ast = AlyoshaParser.start AlyoshaLexer.tokenize lexbuf
    ast

[<EntryPoint>]
let main argv = 
    try
        let ast = parseFromFile argv.[0]
        let table = checkProgram ast
        let ast = FoldConstants ast
        let scopes, stringConstantsDict = GetScopes ast table
        let asmCode = GenerateCode ast table scopes stringConstantsDict
        
        let programName = 
            match ast with Program (x, _, _) -> x
        use out = new System.IO.StreamWriter(sprintf "%s.asm" programName)
        out.WriteLine(asmCode)
        out.Close()
        use buildFile = new System.IO.StreamWriter(sprintf "%s_build.bat" programName)
        buildFile.WriteLine(sprintf @"c:\masm32\bin\ml /c /coff %s.asm" programName)
        buildFile.WriteLine(sprintf @"c:\masm32\bin\link /SUBSYSTEM:CONSOLE /LIBPATH:c:\masm32\lib %s.obj" programName)
        buildFile.Close()
    with
    | Typer.UnifyException -> System.Console.WriteLine("AlyoshaLang-Compiler: type inference error")
    | :? System.ArgumentException -> System.Console.WriteLine("Seems the program file can not be found. Maybe it's other")
    | x -> System.Console.WriteLine("AlyoshaLang-Compiler: Lex/Syntax or maybe some other error. Maybe your file not found.")
    0 // return an integer exit code
