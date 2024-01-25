# Proposal for OTTL improvements

A modest proposal for improving OTTL

## Context

Today OTTL is a scripting language for the OpenTelemetry Collector that simplifies/unifies common
OTLP transformation activities, like adding attributes to resources or data points. It is rapidly gaining in
popularity and provides an important baseline featureset for enabling OTLP pipelines. However, the
language is currently, purposefully, restricted. We believe the language can be improved while still preserving functoinality.

## Review of today's OTTL

OTTL is an interpreted language. It takes expressions like `set(attribute["service.name"], "my-service") where attribute["service.name"] == nil` and executes them on streams of OTLP data.

OTTL exposes the following to the runtime environment:

- An "execution scope" it will execute against, e.g. `metric`, `resource`, `log`, etc.
- A statement to execute which will manipulate a type of telemetry.
- A boolean expression which determins whether its statement should be executed.

TODO - more

## Collection of complaints

- Artificial distinction between rvalues and statements (e.g. ParseJSON can be used as an rvalue, but replace_pattern has no value and can only write to a field)
- very limited condition support (statements can only have a single condition, structured as foo() where baz); I think I want a ternary operator
- inconsistent and surprising syntax limitations (parentheses cannot be safely added around any expression, Boolean values can't be used as conditions without appending == true, time values can't be stored in cache fields, etc)
- Even trivial manipulation requires a specific "built-in" function, e.g.
  - `set`
  - `delete_key`
  - `delete_keys`
  - `keep_keys`
  - `replace_all_matches`
  - `replace_all_patterns`
  - `replace_match`
  - `replace_pattern`
  - `truncate_all`
  - `limit`
  - Isn't `set(attributes["key"], nil)` the same as `delete_key(attributes, "key")` ?
- Inconsistency in identifier lookups. trace_id.string and trace_id["string"] don't refer to the same thing
  even though body.string and body["string"] do.
- Dealing with lists: https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29289
- Lack of Contains/"in" operator: https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29289
- Identifying if a given expressoin *only* touches hierarchical components, e.g. looking at resource
  but not log when modifying logs: https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29016
- Statements do NOT include the context they operator on: https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29017

## Proposals

At a high level we propose the following:

- Migrate to a stateful lexer to improve literal support.
  - This would allow supporting string-formatting literals, e.g. "I can reference {expression}s"
  - TODO - other fixes from complaints as needed.
- Add a type system for better "prior to evaluation" error messages, including the ability to
  get error messages without running the collector. (e.g. Go, Rust, Typescript) 
- Allow operations to operate against structural data, prefarrable with a JSON-like feel. (e.g. Jsonix, TypeScript, Dart)
  - Assign multiple values at the same time.
  - Have the visual structural mirrored in the code.
- Provide List comprehenesions (e.g. Typescript, Python, Kotlin) that simplifies operating against
  lists or "KeyValueList" i.e. Attributes.
  - This should dramatically reduce need for built-in functions to be worthwhile.
  - This *should* replace need for: `limit`, `truncate_all`, `replace_*`, `keep_keys`, `delete_keys`.
- We move to a multi-phase compilation so that we can not only add type checking but erase more advanced
  features (e.g. structural merging, list-comprehensions) to simpler features for evaluation.
- Body, as an `AnyValue`, requires lots of typechecking in every statement.  You wind up with similar
  code in the statement + the where.
- Pattern matching, to simplify dealing with generic `AnyValue` attributes and log bodies, reducing the need
  to duplicate intent between `where` clause and statements.

### Prototype Grammar

We propose expanding the high level grammar to include the context in the syntax:

Example:

```
On <context>
( <pattern> ) ?
(Drop | Yield <expr>)
( Where <expr> ) ?
```

This matches the output of:

- An "execution scope" it will execute against, e.g. `metric`, `resource`, `log`, etc.
- A statement to execute which will manipulate a type of telemetry.
- A boolean expression which determins whether its statement should be executed.

However, this introduces a new notion, "Pattern" where you can both test if a particular value is of a type
and extract that type for usage in the statement at the same time.

In addition to patterns, we also introduce new expresions.

#### Pattern Matching

We propose adding patterns to OTTL grammar.  A pattern defines both a filter that will participate
in what was previously only the `where` clause, but also defines new terms (names) that can directly reference typed values from more generic structures.

The pattern grammar is specified as:

```
<pattern> := 
      `WHEN` <expr> `AS` <extraction> ('if' <expr> )?
<extraction> :=
     <identifier> |
     <extraction-function> '(' opt_repeated(<extraction>, ',')* ')'
```

We would provide the following built-in extraction functions:

```
Boolean({id}: bool)
Bytes({id}: List[u8])
Int({id}: i64)
Double({id}: f64)
List({id}: List[AnyValue])
Map({id}: Map[Anyvalue])
```

These should mirror existing conversion functions in OTTL.

Example usage would be:

```
ON log
WHEN log.body AS String(content) if content != Nil
YIELD log with {
  attributes: parse_regex_to_attributes("^Host=(?P<host>[^,]+), Type=(?P<type>.*)$", content)
}
```

Here, we are looking for logs that contain bodies that are string and attempt to parse out host/type syntax from them and apply these to the attributes.  The `WHEN` clause features as both part of the boolean expression when to enable the OTTL statement as well as performing extraction of content in the final
statement (the `content` term).


Extraction functions are simply built-in functions that return optional results.

```
Boolean = (AnyVal) Optional[bool]
Bytes = (AnyVal) Optional[Bytes]
Int = (AnyVal) Optional[Int]
Double = (AnyVal) Optional[Double]
List = (AnyVal) Optional[List[AnyVal]]
Map = (AnyVal) Optional[Map[String, AnyVal]]
```

Additionally, we can provide structural pattern matching in the future where needed, e.g. we could evolve into:

```
On log
WHEN log.body AS {
  "event.name": "some-exact-value",
  "payload": payload
} if payload.some.nested.attribute == "SomeValue"
DROP
```

#### New Binary Expressions

We propose adding new binary expressions to help resolve some syntactical hurdles in the existing Language.

```
<expr> '.' <identifier> # More complicated "accessor" patterns.
<expr> 'with' <expr>  # Merge/Assign operation
<expr> 'in' <expr>    # Contains expression
```

The `.` no longer being part of identifiers would allow accessing members from any expression. When combined
with the type system and multi-phase compilation, we can still erase most `foo.bar` syntax to a single lookup
while also allowing expansion of syntax like `[ 'foo' ].length` if desired.

A new `merge` operation would provide similar value as `merge_maps` but exectue generically on any
type with structure. When combined with [Structural Expressions](#structural-expressions), we can use this to flatten what would today be multiple OTTL statments. For Example:

```
set(metric.name, "name") where ...
set(metric.description, "new desription") where ...
```

could be:

```
on metric
where ...
yield metric with {
  name: "name",
  description: "description"
}
```

The new `merge` operation could be erased in multi-phase compilation to a series of `set` operations.


The `in` operator would proivde a mechanism to check lists/maps, solving https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29289.



#### Structural Expressions

We propose adding new structural literals that allow users to define data overrides in a syntax that
matches the hierarchy they see.

```
<structural-literal> := '{' <field-assignment>* '}'
<field-assignemtn> := <field-name> ':' <expr> (',')?
```

### Type System

We define the following kind of types:

- `AnyValue` - A special type, mostly acting as a union of types allowed in attributes.
- `Nil` - A bottom type.
- `Constructor(name, args)` - A named type, with possible type arguments (e.g. `Int`, `List[A]`)
- `Structural(fields)` - A structural type consisting of known fields and their types.

Additionally, for any named type (`Constructor(name, ..)`) we allow symbol table lookups for "members",
and direct accessing of these via the `<expr> '.' <identifier>` syntax.

TODO - Formal specification & inference rules

- `Nil` is a subtype of all types.
- `Bool`, `Int`, `Double`, `Bytes`, `ArrayValue`, `KeyValueList` are subtypes of `AnyValue`.
- A `Structural` type is mergable with a named type IFF the `fields` of the structural type
  are assignable to fields of the named type.

We have an ad-hoc type inference system in place.  When inferring the type of a `List`, it is able
to unify `Nil` => {special primitive types} => `AnyValue`.  This means lists are guaranteed to infer to either `List[AnyValue]` (heterogenous) or `List[A]` (homogenous).

### OTTL Compiler Phases.

We propose modifying OTTL to use the following phases (or more):

1. Lexer
2. Parser -> Initial Structure
3. Typer -> Adds types to Initial Structure
4. Simplifier -> Removes nodes and flattens out concepts
   - We can "erase" certain concepts, like structural literals, e.g.