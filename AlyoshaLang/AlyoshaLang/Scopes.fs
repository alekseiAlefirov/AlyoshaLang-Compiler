module Scopes

type scope = Scope of int           //depth
                    * (int array)   //used variables

type varIdScopeInfo = VarScopeInfo of int //№ in Table of symbols

