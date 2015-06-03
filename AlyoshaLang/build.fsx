// include Fake lib
#r @"packages\FAKE\tools\FakeLib.dll"
open Fake
open Fake.FscHelper

let buildDir  = "./build/"


// Targets
Target "Clean" (fun _ -> 
    CleanDirs [buildDir]
)

Target "Main.exe" (fun _ ->
  ["Main.fs"]
  |> Fsc (fun p ->
           { p with References =
                      [ "Something.dll"
                        "Otherthing.dll" ] })
)

// Build order
"Clean"
  ==> "BuildApp"

// start build
RunTargetOrDefault "BuildApp"