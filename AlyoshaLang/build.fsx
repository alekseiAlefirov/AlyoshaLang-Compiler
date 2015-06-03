// include Fake lib
#r @"packages\FAKE\tools\FakeLib.dll"
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
  !! "AlyoshaLang/*.fs"
    ++ "AlyoshaLang/obj/*.fs"
    ++ "AlyoshaLang/*.fsy"
    ++ "AlyoshaLang/*.fsl"
  |> Fsc (fun p ->
           { p with References =
                      [ "FSharp.Core.dll"
                        "FSharp.PowerPack.dll" ] })
)

// Build order
"Clean"
  ==> "BuildApp"

// start build
RunTargetOrDefault "BuildApp"