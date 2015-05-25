module VariablesInformation

open Types
open Scopes

type varIdInformation = {
    mutable Type : alyoshaType;
    mutable ScopeInfo : varIdScopeInfo;
    }

