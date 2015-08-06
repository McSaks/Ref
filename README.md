# Reference object in Wolfram Language (a.k.a. Mathematica) ##

`Ref[args...]` represents a reference object, i.e. an object that can reference any expression.

## Typical usage:

Load package:
```mma
Needs["Ref`"];
```

Create __new__ null (unset) Ref:
```mma
ref = NewRef[]   (* ⟶ Ref[ref$1] *)
```

Make the __same__ Ref, __no copying__ here:
```mma
ref2 = ref   (* ⟶ Ref[ref$1] *)
```

They are __initially unset__:
```mma
RefSetQ[ref]     (* ⟶ False *)
RefSetQ[ref2]    (* ⟶ False *)
RefNullQ[ref]    (* ⟶ True *)
RefNullQ[ref2]   (* ⟶ True *)
```

__Bind__ a value:
```mma
!ref2 = 42   (* ⟶ 42 *)
```

Now both __are set__…
```mma
RefSetQ[ref]     (* ⟶ True *)
RefSetQ[ref2]    (* ⟶ True *)
RefNullQ[ref]    (* ⟶ False *)
RefNullQ[ref2]   (* ⟶ False *)
```

… to 42, as we can see by __dereferencing__:
```mma
{!ref, !ref2}   (* ⟶ {42, 42} *)
```

__Redirect__ ref2 to a new value, i.e. create new Ref, point it to `"value"`, and assign to ref2:
```mma
ref2 = NewRef["value"]   (* ⟶ Ref[ref$2] *)

(* Just the same as: *)
ref2 = NewRef[];
!ref2 = "value";
```

Now we have two __distinct__ Refs:
```mma
!ref    (* ⟶ 42 *)
!ref2   (* ⟶ "value" *)
```

__Clear__ a reference:
```mma
UnRef[ref]
```

A null Ref behaves as if it were bound to `Null`:
```mma
RefSetQ[ref]    (* ⟶ False *)
RefNullQ[ref]   (* ⟶ True *)
!ref            (* ⟶ Null *)
```

Note: don’t clear a Ref by binding it to `Null`!  
~~`!ref = Null`~~ does not clear the reference but only sets the referent to `Null`.  
~~`ref = Null`~~ simply sets a new value `Null` to the variable `ref`, it is not a reference at all.

Dereference all Refs in an expression:
```mma
EvalRef @ {ref, ref2}   (* ⟶ {Null, "value"} *)
```

__List__ all set Refs:
```mma
Refs[]   (* ⟶ {Ref[ref$2]} *)
```

__Clear all__ references:
```mma
UnRef[]
Refs[]   (* ⟶ {} *)
```

Use Refs in a block and __clean up__ when the work is done:
```mma
RefBlock[
  ref1 = NewRef[42];
  ref2 = ref1;
  ref1 =.;
  !ref2
]   (* ⟶ 42 *)
Refs[]   (* ⟶ {} *)
```

`RefBlock` has a `Module`-like syntax, so code above may be written using local variables as
```mma
RefBlock[ {
    ref1 = NewRef[42],
    ref2
  },
  ref2 = ref1;
  ref1 =.;
  !ref2
]   (* ⟶ 42 *)
Refs[]   (* ⟶ {} *)
```
