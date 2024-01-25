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
<statement> :=
  on <context-identifier>
  (<pattern-match>)?
  <statement-action>

<context-identifier> := 'span' | 'spanevent' | 'metrics' | 'log'

<statement-action> :=
  'drop' | 
  'yield' <expr>

<pattern-match> := 'when' <expr> 'is' <pattern-extractor>

<pattern-extractor> := 
  <identifier> |
  <identifier> '(' <pattern_extractor> ')'

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

## Compiler Phases

1. Parser takes raw strings and turns it into an AST.
2. Typer takes the AST and inferrs/assigned types returning a typed AST.
3. IR transforms the AST into an intermediate representation on which we can perform our optimisations.
4. transform has optimisations we execute before outputing our final interpretation nodes.
   - We remove any mathematical operations that can be calculated at compile time.
   - We flatten the `with` (Merge) operations to a set of `set(field,value)` operations.

By the end of the compiler, the only IR nodes left should be:

```
MultiExpr(exprs) // Evaluate all of these.
Lookup(id)       // Pull a value from context, e.g. span.status.code
Literal(value)   // A compile-time constant to use/provide.
MakeList(exprs)  // Construct a list using the expressions provided.
FunctionApply(name, exprs) // Execute a function with given set of value.
```

Note: `MultiExpr` should NOT appear anywhere a single value is expected.

## Built-In Environment

- `span` has type `Span`
- `metric` has type `Metric`
- `resource` has type `Resource`

### Built-in Types

While this isn't expressible in the OTTL language, we define a symbol table against named
types that looks as follows:

```
struct Time {}
struct SpanID {}
struct TraceID {}
struct SpanStatus {
  code Int,
  message String,
}
struct Span {
  name String,
  kind Int,
  status SpanStatus,
  startTime Time,
  endTime Time,
  spanID SpanID,
  traceId TraceID,
  // TODO - attributes, events, dropped*
}

struct Log {
  spanID SpanID,
  traceId TraceID,
  time Time,
  observed_time Time,
  severity_number Int,
  severity_text String,
  body AnyVal,
  flags Int,
  // TODO - attributes
}
```

The rest of the types are unimplemented as this is just a proof-of-concept.


### Built-in Symbols / Functions

We expose the following built-in symbols (note this is pseudo-code not expressable OTTL)

```go
func aSum(metric *Metric) Option[*Sum]
```

## TODOs

- [X] Flatten structural merging to `Set` calls to showcase existing OTTL.
- [ ] Implement string literals
- [ ] Implement comperhensions for lists + key-value lists.
- [ ] Move off nom-recursive to a more robust solution.
- [X] Evaluate literal arithmetic operations in compiler.
- [ ] Allow comments in the language.
- [ ] Formal Type specification
- [ ] Enforce requirements after each phase/transform of the tree.


## Examples

*Structural literal with compile-time-evaluation*

```
on span
yield span with {
    status: {
        code: 1+2-3
    },
    kind: 2
}
```

becomes

```
context: span
guard: true
expr: set(span.kind, 2)
      set(span.status.code, 0)
```

*Pattern match with expansion to guard*

```
on metric
when metric is aSum(sum)
yield metric with { 
    sum: sum 
}
```


becomes

```
context: metric
guard: IsSome(aSum(metric))
expr: set(metric.sum, OptGet(aSum(metric)))
```

Note: this assumes:
- `aSum` built in function of `(*Metric) Option[*Sum]`.
- `IsSome` built in function of `[k] (*Option[k]) bool`.
- `OptGet` built-in function of `[k] (*Option[k]) k`.
