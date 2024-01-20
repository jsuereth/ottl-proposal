# OpenTelemetry Transformation Template Language Experimentation

This repository represents experiments for the OpenTelemetry template language.

Specifically, we're investigating a few optimisations:

- An extension that simplifies manipulating nested datastructures (like protos),
  as this is core/critical to transformation of OTLP.
- A type system for OTTL.
  - This should help catch errors earlier, and provide better error messages.
  - This could improve runtime performance via hot-path accessors for datatypes.
- Possibility of a multi-pass optimiser to reduce necessary components in evaluation.

## Grammar

A hazy definition of the grammar.

```
<expr> := 
  <application> |
  <binary_expr> |
  <accessor> |
  <structure_constructor> |
  <list_expr> |
  <literal> |
  <identifier>

# Calling a function
<application> := <expr> '(' optional_repeated(<expr>, ',') ')'

# Binary Operations
<binary_expr> := <expr> <binary_op> <expr>
<binary_op> :=
   '==' | "!=" |             # Boolean operators
   '+' | '-' | '*' | '/' |   # numeric operators
   'with' |                  # Structural join
   'in'                      # contained-in operator.

# Accessing members
<accessor> := <expr> '.' <identifier>

# Defining ad-hoc structure
<structure_constructor> :=  '{' optional_repeated(<field-assignment>, ',') '}'
<field-assignment> := <identifier> ':' <expr>

# Defining a list of items
<list_expr> := '[' optional_repeated(<expr>, ',') ']'

# TODO - specifiy allowed strings for identifiers.
<identifier> := ...

# Literals, e.g. true, false, "Hi", 1.
<literal> :=
  "Nil" |
  <bool-literal> |
  <int-literal> |
  <double-literal> |
  <string-literal>
<string-literal> := ...
<int-literal> := ...
<bool-literal> := "true" | "false"
<double-literal> := ...
```

We're using some hackery to have left-recursive grammars in "nom".

## Type System.

We have the following types:

- `AnyValue` - A special type, mostly acting as a union of types allowed in attributes.
- `Nil` - A bottom type.
- `Constructor(name, args)` - A named type, with possible type arguments (e.g. Int, List[A])
- `Structural(fields)` - A structural type consisting of known fields and their types.


TODO - Formal specification

- `Nil` is a subtype of all types.
- `Bool`, `Int`, `Double`, `Bytes`, `ArrayValue`, `KeyValueList` are subtypes of `AnyValue`.
- A `Structural` type is mergable with a named type IFF the `fields` of the structural type
  are assignable to fields of the named type.

We have an ad-hoc type inference system in place.  When inferring the type of a `List`, it is able
to unify `Nil` => {special primitive types} => `AnyValue`.  This means lists are NOT guaranteed to
be homogenous.

## TODOs

- [ ] Flatten structural merging to `Set` calls to showcase existing OTTL.
- [ ] Implement string literals
- [ ] Implement comperhensions for lists + key-value lists.
- [ ] Move off nom-recursive to a more robust solution.
- [ ] Evaluate literal arithmetic operations in compiler.
- [ ] Allow comments in the language.


## Examples

```
span with { 
    status: { 
        code: 1+2-3
    }, 
    kind: 2
}
```

becomes

```
Merge(span,{status: {code: Plus(1, Minus(2, 3))}, kind: 2})
```