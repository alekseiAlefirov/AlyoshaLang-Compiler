# AlyoshaLang-Compiler
Semester task for Transltation Processes lead to this kinda program.

AlyoshaLang is functional programming language, translating by this compiler to ASM x86 (MASM32 currently).

Implentation language is Microsoft F#.

To build this, you need 
- MS VS2012 and FSharp 3.0
- FSharp.PowerPack for FSharp 3.0 (https://fsharppowerpack.codeplex.com/releases)
  PowerPack is needed for lexing/parsing instruments.

OR
- launch "AlyoshLang/build.bat"

Launch: AlyoshaLang.exe "in.in".

Output : a.asm and a.bat for compiling and linking by MASM32.

So, for succesive using your programs, you need to
 - Install MASM32 on C:\
 (http://www.masm32.com/masmdl.htm)
 
 See some code samples in CodeSamples folder!
