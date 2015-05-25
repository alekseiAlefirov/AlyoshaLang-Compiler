// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Text.Lexing
open System.IO

open AlyoshaAST
open AlyoshaParser
open AlyoshaLexer
open Typer


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
    let ast = parseFromFile "..\..\..\CodeSamples\sample4.txt"
    let table = checkProgram ast
    //let ast = parseFromString "x"
    0 // return an integer exit code
