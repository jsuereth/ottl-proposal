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


### Prototype Grammar

Example:

```
On <context>
Yield <expr>
( Where <expr> ) ?
```

We introduce new expressions:

#### New Binary Expressions

```
<expr> 'with' <expr>  # Merge/Assign operation
<expr> 'in' <expr>    # Contains expression
```

- Merge: Overrides fields in the left with fields found in the partial structure on the right
- Contains: Returns true if the expression on the left is found in the container on the right.
  TODO - what to do about key-values?

#### Structural Expressions

```
<structural-literal> := '{' <field-assignment>* '}'
<field-assignemtn> := <field-name> ':' <expr> (',')?
```