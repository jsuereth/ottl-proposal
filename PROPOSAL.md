# Proposal for OTTL improvements

A modest proposal for improving OTTL

## Context

Today OTTL is a scripting language for the OpenTelemetry Collector that simplifies/unifies common
OTLP transformation activities, like adding attributes to resources or data points. It is rapidly gaining in
popularity and provides an important baseline featureset for enabling OTLP pipelines. However, the
language is currently, purposefully, restricted. We believe that the current form of the language limits its usefulness for problems that customers need to adopt OTLP at scale. We propose an alternative scripting language that addresses these gaps.

## Review of today's OTTL

OTTL is an interpreted language that takes expressions like `set(attribute["service.name"], "my-service") where attribute["service.name"] == nil` and executes them on streams of OTLP data.

OTTL has the following design principles:

- OTTL is intended as a domain-specific language (DSL) for telemetry mutation and generation, and is not intended to be used as a general-purpose programming language.
- OTTL has been designed to work directly with `pdata`, but can operate on other data formats.

OTTL exposes the following to the runtime environment:

- An "execution scope" it will execute against, e.g. `metric`, `resource`, `log`, etc.
- A statement to execute which will manipulate a type of telemetry.
- A boolean expression which determines whether its statement should be executed.

Additionally, OTTL, today, loosely defines the [terms and paths](https://github.com/open-telemetry/opentelemetry-collector-contrib/blob/main/pkg/ottl/LANGUAGE.md#paths) it allows. We believe that OTTL can have a much greater impact if its behavior can be fully described by a specification. For example, it will be possible to implement OTTL in programming languages and environments other than the OTel collector.

Finally, OTTL provides a loose set of coercion rules on types, outlined as [comparison rules](https://github.com/open-telemetry/opentelemetry-collector-contrib/blob/main/pkg/ottl/LANGUAGE.md#comparison-rules).
Given OTTL doesn't enforce types, this describes the runtime checks and coercion that must happen on
any interaction.

## Collection of complaints

After transitioning from custom collector processors and previous transform processors, we've collected
feedback from developers and also known issues opened against OTTL.

- Artificial distinction between rvalues and statements (e.g. ParseJSON can be used as an rvalue, but replace_pattern has no value and can only write to a field)
- very limited condition support (statements can only have a single condition, structured as foo() where baz); I think I want a ternary operator
- inconsistent and surprising syntax limitations (parentheses cannot be safely added around any expression, Boolean values can't be used as conditions without appending == true, time values can't be stored in cache fields, etc)
- Many transformation functions have only trivial differences, and many use cases wind up adding new, subtly different, "built-in" functions to OTTL. e.g.
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
- Inconsistency in identifier lookups. trace_id.string and trace_id["string"] don't refer to the same thing even though body.string and body["string"] do.
- Dealing with lists (lack of "contains" or "in" operator): https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29289
- Identifying if a given expression *only* touches hierarchical components, e.g. looking at resource but not log when modifying logs: https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29016
- Statements do NOT include the context they operate on: https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29017

## Proposals

At a high level we propose the following:


- Supporting string-formatting literals, e.g. "I can reference {expression}s"
- Add a type system for better "prior to evaluation" error messages, including the ability to
  get error messages without running the collector. (e.g. Go, Rust, Typescript) 
- Allow operations to operate against structural data, preferably with a JSON-like feel. (e.g. [Jsonnet](https://jsonnet.org/) TypeScript, Dart)
  - Assign multiple values at the same time.
  - Have the visual structure mirrored in the code.
- Provide List comprehensions (e.g. Typescript, Python, Kotlin) that simplifies operating against
  lists of "KeyValueList" i.e. Attributes.
  - This should dramatically reduce need for built-in functions to be worthwhile.
  - This *should* replace need for: `limit`, `truncate_all`, `replace_*`, `keep_keys`, `delete_keys`.
- Pattern matching or "binding patterns", to simplify dealing with generic `AnyValue` attributes and log bodies, reducing the need to duplicate intent between `where` clause and statements.
- (optional) - Only reserve new keywords in "stateful" context.  (This would require migrating to a stateful lexer, but could help avoid reducing the available identifiers).

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
- A boolean expression which determines whether its statement should be executed.

However, this introduces a new notion, "Pattern" where you can both test if a particular value is of a type and extract that type for usage in the statement at the same time.

In addition to patterns, we also introduce new expressions.

#### Pattern Matching

We propose adding patterns to OTTL grammar.  A pattern defines both a filter that will participate
in what was previously only the `where` clause, but also defines new terms (names) that can directly reference typed values from more generic structures.

The pattern grammar is specified as:

```
<pattern> := 
      `WHEN` <expr> `IS` <extraction> ('IF' <expr> )?
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
WHEN log.body IS String(content) if content != Nil
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
} IF payload.some.nested.attribute == "SomeValue"
DROP
```

Note: If this proposal is accepted, we do think a separate, pattern matching specific proposal should be generated that would outline major use cases and refine syntax/support needed from built-in functions.

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

A new `merge` operation would provide similar value as `merge_maps` but execute generically on any
type with structure. When combined with [Structural Expressions](#structural-expressions), we can use this to flatten what would today be multiple OTTL statements. For Example:

```
set(metric.name, "name") where ...
set(metric.description, "new description") where ...
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


The `in` operator would provide a mechanism to check lists/maps/keyvallists, solving https://github.com/open-telemetry/opentelemetry-collector-contrib/issues/29289.



#### Structural Expressions

We propose adding new structural literals that allow users to define data overrides in a syntax that
matches the hierarchy they see.

```
<structural-literal> := '{' <field-assignment>* '}'
<field-assignment> := <field-name> ':' <expr> (',')?
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
- A `Structural` type is mergeable with a named type IFF the `fields` of the structural type
  are assignable to fields of the named type.

We have an ad-hoc type inference system in place.  When inferring the type of a `List`, it is able
to unify `Nil` => {special primitive types} => `AnyValue`.  This means lists are guaranteed to infer either `List[AnyValue]` (heterogeneous) or `List[A]` (homogenous).  Disallowing heterogeneous lists could be done as a post-type-check error.

### OTTL Compiler Phases.

We propose modifying OTTL to use the following phases (or more):

1. Lexer
2. Parser -> Initial Structure
3. Typer -> Adds types to Initial Structure
4. Simplifier -> Removes nodes and flattens out concepts
   - We can "erase" certain concepts, like structural literals, e.g.


## Motivating Example

We'd like to convert the JSON structure used in Google Cloud's structured logging to OTLP.

- See: https://cloud.google.com/logging/docs/structured-logging for the original format
- See: https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/logs/data-model-appendix.md#google-cloud-logging for translation requirements.


Here's an example log JSON body from the links above:

```json
{
  "severity":"ERROR",
  "message":"There was an error in the application.",
  "httpRequest":{
    "requestMethod":"GET"
  },
  "times":"2020-10-12T07:20:50.52Z",
  "logging.googleapis.com/insertId":"42",
  "logging.googleapis.com/labels":{
    "user_label_1":"value_1",
    "user_label_2":"value_2"
  },
  "logging.googleapis.com/operation":{
    "id":"get_data",
    "producer":"github.com/MyProject/MyApplication",
     "first":"true"
  },
  "logging.googleapis.com/sourceLocation":{
    "file":"get_data.py",
    "line":"142",
    "function":"getData"
  },
  "logging.googleapis.com/spanId":"000000000000004a",
  "logging.googleapis.com/trace":"projects/my-projectid/traces/06796866738c859f2f19b7cfb3214824",
  "logging.googleapis.com/trace_sampled":false
}
```

Here's an existing OTTL transformation on this JSON body to make it OTLP friendly.

```
context: log
statements:
- set(body, ParseJSON(body["message"])) where (body != nil and body["message"] != nil)
- merge_maps(attributes, body["logging.googleapis.com/labels"], "upsert") where body["logging.googleapis.com/labels"] != nil
- delete_key(body, "logging.googleapis.com/labels") where (body != nil and body["logging.googleapis.com/labels"] != nil)
- delete_key(cache, "__field_0") where (cache != nil and cache["__field_0"] != nil)
- set(cache["__field_0"], body["logging.googleapis.com/httpRequest"]) where (body != nil and body["logging.googleapis.com/httpRequest"] != nil)
- delete_key(body, "logging.googleapis.com/httpRequest") where (body != nil and body["logging.googleapis.com/httpRequest"] != nil)
- set(cache["value"], cache["__field_0"])
- set(attributes["gcp.http_request"], cache["value"]) where (cache != nil and cache["value"] != nil)
- delete_key(cache, "__field_0") where (cache != nil and cache["__field_0"] != nil)
- set(cache["__field_0"], body["logging.googleapis.com/logName"]) where (body != nil and body["logging.googleapis.com/logName"] != nil)
- delete_key(body, "logging.googleapis.com/logName") where (body != nil and body["logging.googleapis.com/logName"] != nil)
- set(cache["value"], cache["__field_0"])
- set(attributes["gcp.log_name"], cache["value"]) where (cache != nil and cache["value"] != nil)
- delete_key(cache, "__field_0") where (cache != nil and cache["__field_0"] != nil)
- set(cache["__field_0"], body["logging.googleapis.com/severity"]) where (body != nil and body["logging.googleapis.com/severity"] != nil)
- delete_key(body, "logging.googleapis.com/severity") where (body != nil and body["logging.googleapis.com/severity"] != nil)
- set(cache["value"], cache["__field_0"])
- set(severity_text, cache["value"]) where (cache != nil and cache["value"] != nil)
- delete_key(cache, "__field_0") where (cache != nil and cache["__field_0"] != nil)
- set(cache["__field_0"], body["logging.googleapis.com/sourceLocation"]) where (body != nil and body["logging.googleapis.com/sourceLocation"] != nil)
- delete_key(body, "logging.googleapis.com/sourceLocation") where (body != nil and body["logging.googleapis.com/sourceLocation"] != nil)
- set(cache["value"], cache["__field_0"])
- set(attributes["gcp.source_location"], cache["value"]) where (cache != nil and cache["value"] != nil)
- delete_key(cache, "__field_0") where (cache != nil and cache["__field_0"] != nil)
- set(cache["__field_0"], body["logging.googleapis.com/spanId"]) where (body != nil and body["logging.googleapis.com/spanId"] != nil)
- delete_key(body, "logging.googleapis.com/spanId") where (body != nil and body["logging.googleapis.com/spanId"] != nil)
- set(cache["value"], cache["__field_0"])
- set(span_id, cache["value"]) where (cache != nil and cache["value"] != nil)
- delete_key(cache, "__field_0") where (cache != nil and cache["__field_0"] != nil)
- set(cache["__field_0"], body["logging.googleapis.com/trace"]) where (body != nil and body["logging.googleapis.com/trace"] != nil)
- delete_key(body, "logging.googleapis.com/trace") where (body != nil and body["logging.googleapis.com/trace"] != nil)
- set(cache["value"], cache["__field_0"])
- set(trace_id, cache["value"]) where (cache != nil and cache["value"] != nil)
```

Now in the new OTTL:


```
on log
when log.body is aStringMessage(parsedJson(json))
yield log with {
  attributes: attributes with json["logging.googleapis.com/labels"] with {
    "gcp.log_name": json["logging.googleapis.com/logName"]
  },
  spanID: StringToSpanID(json["logging.googleapis.com/spanId"]),
  traceID: StringToTraceID(json["logging.googleapis.com/trace"]),
  severity_text: json["logging.googleapis.com/severity"],
  body: json with {
    "logging.googleapis.com/labels": nil,
    "logging.googleapis.com/logName": nil,
    "logging.googleapis.com/severity": nil,
    "logging.googleapis.com/sourceLocation": nil,
    "logging.googleapis.com/spanId": nil,
    "logging.googleapis.com/trace": nil,
    "logging.googleapis.com/labels": nil,
  },
}
```

This assumes a few built-in functions:

```go
func aStringMessage(body *AnyValue) Option[String]
func parsedJson(value *String) Option[AnyValue]
// We'd also use a regex/strip method to remove extraneous information prior to the span-id in the string.
func StringToSpanId(value* String) SpanID
func StringToTraceID(value* String) TraceID
```

And other features that may need syntactic changes:

- we need "upsert" "insert" and "update" versions of `with`, possibly `or` (update) for the attributes merge:

   ```attributes or json[".../labels"] or { "gcp.log_name": json[".../logName"] }```


- we assume we can assign `AnyVal` to strings here silently without failure.
  Note: Current OTTL does this via coercions.
- We can `with`` AnyVal to `Attributes` to join key-values. (Prototype currently does not allow)
