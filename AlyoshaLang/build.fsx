// include Fake lib
#r @"packages\FAKE.3.34.7\tools\FakeLib.dll"
open Fake
open Fake.FscHelper

let srcDir = "./AlyoshaLang/"
let objDir = srcDir + "obj/"
let buildDir  = "./build/"
let dllDir = "./packages/"


// Targets
Target "Clean" (fun _ -> 
    CleanDirs [buildDir]
)

Target "AlyoshaLang.exe" (fun _ ->
  [srcDir + "AlyoshaAST.fs"; 
    objDir + "AlyoshaParser.fs";
    objDir + "AlyoshaLexer.fs"; 
    //objDir + "AlyoshaParser.fsi";
    srcDir + "Types.fs";  
    //srcDir + "Conversions.fs";
    srcDir + "FunScopes.fs";
    srcDir + "VariablesInformation.fs";
    srcDir + "Typer.fs";
    srcDir + "Scoper.fs"; 
    srcDir + "CodeGenerator.fs";
    srcDir + "Program.fs";]
  |> Fsc (fun p ->
           { p with Output = buildDir + "AlyoshaLang.exe"
                    References =
                      [ dllDir + "FSharp.Core.dll"
                        dllDir + "FSharp.PowerPack.dll" ] })
)

// Build order
"Clean"
  ==> "AlyoshaLang.exe"

// start build
RunTargetOrDefault "AlyoshaLang.exe"