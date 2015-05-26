module VariablesInformation

open Types
open FunScopes

type varIdInformation = {
    mutable Type : alyoshaType;
    mutable ScopeInfo : varScopeInfo;
    }

