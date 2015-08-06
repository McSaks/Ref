(* ::Package:: *)

BeginPackage["Ref`"]

Unprotect[Evaluate[$Context<>"*"]];

Unprotect/@{Not,Unset,Set};

`Private`formatUsage[`Private`str_] := StringReplace[`Private`str,
  "`" ~~ Shortest[`Private`s__] ~~ "`" :>
  "\!\(\*StyleBox[\(" <> StringReplace[`Private`s, {
     "_" ~~ Shortest[`Private`i__] ~~ "_" :>
     "\*StyleBox[\(" <> `Private`i <> "\), \"TI\"]",
     "!" -> "!\[InvisibleSpace]"
  }] <> "\),\"MR\",ShowStringCharacters->True]\)"];

Ref::usage = "`Ref[_args..._]` is a handle reference to some expression that can be got via `EvalRef`" // `Private`formatUsage;
(* UpSet::"Ref`usage" = "`_ref_ ^= _val_` sets the value of the referent, whereas `_ref_ = _newref_` set the reference itself." // `Private`formatUsage; *)
Not::"Ref`usage" = "`!_ref_` dereferences `_ref_`." // `Private`formatUsage;
SetRef::usage = "`SetRef[_ref_, _val_]` sets the value of the referent. Can be written as `!_ref_ = _val_`." // `Private`formatUsage;
NewRef::usage = "`NewRef[]` creates a unique null reference.
`NewRef[_val_]` creates a reference with referent `_val_`." // `Private`formatUsage;
EvalRef::usage = "`EvalRef[_expr_]` dereferences all references in `_expr_`. `!_ref_` dereferences only `_ref_` once." // `Private`formatUsage;
RefSetQ::usage = "`RefSetQ[_ref_]` test if `_ref_` is set." // `Private`formatUsage;
RefNullQ::usage = "`RefNullQ[_ref_]` test if `_ref_` is not set." // `Private`formatUsage;
UnRef::usage = "`UnRef[_ref_]` clears reference `_ref_` to null. Can be written as `!_ref_ =.`. \
Note that `!_ref_ = Null` does not clear the reference but only sets the referent to `Null`; \
neither does `_ref_ = Null` which simply sets a new value to the variable `_ref_`, possibly leaving hanging reference. \
`UnRef[]` clears all references created so far." // `Private`formatUsage;
RefBlock::usage = "`RefBlock[_expr_]` evaluates `_expr_` and clears afterwards all \[OpenCurlyQuote]local\[CloseCurlyQuote] references set inside. \
Any \[OpenCurlyQuote]global\[CloseCurlyQuote] reference unset and reset inside `RefBlock` is treated as local \
and is unset on exit from `RefBlock`.
`RefBlock[{_var_ = _value_, \[Ellipsis]}, _expr_]` uses `Module` to localise `_var_, \[Ellipsis]`." // `Private`formatUsage;
Refs::usage = "`Refs[]` returns the list of set references." // `Private`formatUsage;


Begin["`Private`"];


SetRef[r_Ref, val_] := Eval[r] = val;
(* (r_ ^= val_) := SetRef[r, val] /; Head[r] === Ref; *)
(! r_ = val_) := SetRef[r, val] /; Head[r] === Ref;
! r_Ref ^:= Eval[r];
(* r_Ref[] := Eval[r]; *)
Unset /: (! r_ =.) := UnRef[r] /; Head[r] === Ref;
Eval@e_Eval := e;
EvalRefOnce[expr_] := expr /. r_Ref :> Eval[r];
EvalRef[expr_] := FixedPoint[EvalRefOnce, expr];
newRef[id_: "Ref`Symbols`ref$"] := Ref[Unique[id]];
NewRef[expr_] := With[{ref = newRef[]}, SetRef[ref, expr]; ref];
NewRef[] := newRef[];
RefSetQ[r_Ref] := MemberQ[Extract[#, {1, 1, 1}] & /@ DownValues[Eval], r];
RefNullQ[r_Ref] := !RefSetQ[r];
RefSetQ[_] = RefNullQ[_] = False;
UnRef[] := (Clear @ Eval; Eval@e_Eval := e;);
UnRef[r_Ref] := (DownValues[Eval] = DeleteCases[DownValues[Eval], HoldPattern[
   Verbatim[HoldPattern] [Eval[r]] :> _
  ]];);
Refs[] := Most[Extract[#, {1, 1, 1}] & /@ DownValues[Ref`Private`Eval]];

Eval@r_Ref /; RefNullQ@r ^= Null;


SetAttributes[RefBlock, HoldAll];
RefBlock[vars: {___}, expr_] := RefBlock[Module[vars, expr]];
RefBlock[expr_] := scope[
  Block[
    { SetRef },
    SetRef[r_Ref, val_] := If[RefSetQ[r],
      Eval[r] = val,
      Eval[r] = val;
      scopeExit[UnRef[r]];
    ];
    expr
  ]
];



Unprotect @ ToString;
MakeBoxes[e : Eval@Ref[s___], form_] := With[ { args = RowBox[ Riffle[MakeBoxes[#, form]& /@ {s}, ","] /.
    str_String :> StringReplace[str, "Ref`Symbols`ref$" ~~ sy_ :> sy] ] },
  InterpretationBox[
   RowBox @ {"\[LeftSkeleton]", SubscriptBox["Null", args], "\[RightSkeleton]"},
   Unevaluated@e]];
ToString[e : Eval@Ref[s___], form : _|PatternSequence[]] := "\[LeftSkeleton]Eval@Ref[" <> StringJoin@Riffle[(ToString[#, form]&) /@ {s}, ", "] <> "]\[RightSkeleton]";
MakeBoxes[e : Ref[s___], form_] := With[ { args = RowBox[ Riffle[MakeBoxes[#, form]& /@ {s}, ","] /.
    str_String :> StringReplace[str, "Ref`Symbols`ref$" ~~ sy_ :> sy] ] },
  InterpretationBox[
   RowBox @ {"\[LeftSkeleton]", SubscriptBox["Ref", args], "\[RightSkeleton]"},
   Unevaluated@e]];
Protect @ ToString;



Do[With[{a=aa},
a[___] /; Message[a::args, a] = $Failed;
],{aa,{UnRef,RefBlock}}];
EvalRef[args___] /; Message[EvalRef::argx, EvalRef, Length@Hold@args] = $Failed;
SetRef[args_] /; Message[EvalRef::argr, EvalRef, 2] = $Failed;
SetRef[args_, _] /; Message[EvalRef::args, EvalRef] = $Failed;
SetRef[args:PatternSequence[]|PatternSequence[_,_,__]] /; Message[EvalRef::argrx, EvalRef, Length@Hold@args, 2] = $Failed;
NewRef[PatternSequence[_,__]] /; Message[NewRef::argt, EvalRef, Length@Hold@args, 0, 1] = $Failed;
Refs[arg1_, args__] /; Message[Refs::argrx, Refs, Length@Hold[arg1, args], 0] := $Failed;
Refs[arg1_] /; Message[Refs::argr, Refs, 0] := $Failed;


(* copy from my ScopeExit`, see https://github.com/McSaks/wl-init/blob/master/ScopeExit.m *)
SetAttributes[scope, HoldAll];
scope[{vars___}, body_] := scope[Module[{vars}, body]];
scope[body_] :=
  With[{o =
      Reap[ CheckAbort[
        Catch[
          Sow[Null, scope]; (* sowing dummy expr *)
          body
        , ex_, Exception]
      , abort], scope] },
    ReleaseHold @ Reverse @ Last @ Last @ o; (* ⟵ evaluate 'finally' upward *)
    With[ {f = First @ o},
      If[f === abort, Abort[]];
      If[ Head @ f === Exception,
        If[ Last @ f =!= scope,
          Throw @@ f, (* ⟵ rethrow *)
          First @ f   (* ⟵ return via ScopeReturn *)
        ],
        f   (* ⟵ return result *)
      ]
    ]
  ];
SetAttributes[scopeExit, HoldFirst];
scopeExit[finally_] := Sow[Hold[finally], scope];

End[];

Protect/@{Not,Unset,Set};
Protect[Evaluate[$Context<>"*"]];
Unprotect[Evaluate[$Context<>"$*"]];


EndPackage[];
