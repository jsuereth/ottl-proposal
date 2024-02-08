## Examples

### Perform transformation if field does not exist
Set attribute `test` to `"pass"` if the attribute `test` does not exist:
```yaml
transform:
  error_mode: ignore
  trace_statements:
    - context: span
      statements:
        # accessing a map with a key that does not exist will return nil. 
        - set(attributes["test"], "pass") where attributes["test"] == nil
``` 

proposal (note: this uses `or` instead of `with`):

```yaml
transform:
  error_mode: ignore
  trace_statements: |
    on span
    yield span or {
      attributes: {
        test: "pass"
      }
    }
```

### Rename attribute
There are 2 ways to rename an attribute key:

You can either set a new attribute and delete the old:

```yaml
transform:
  error_mode: ignore
  trace_statements:
    - context: resource
      statements:
        - set(attributes["namespace"], attributes["k8s.namespace.name"])
        - delete_key(attributes, "k8s.namespace.name") 
``` 

proposal:

```yaml
transform:
  error_mode: ignore
  trace_statements:
    - statements: |
        on resource
        yield resource with {
          attributes: {
            namespace: resource.attributes["k8s.namespace.name"],
            "k8s.namespace.name": nil,
          }
        }
``` 

Or you can update the key using regex:

```yaml
transform:
  error_mode: ignore
  trace_statements:
    - context: resource
      statements:
        - replace_all_patterns(attributes, "key", "k8s\\.namespace\\.name", "namespace")
``` 

proposal:

```yaml
transform:
  error_mode: ignore
  trace_statements:
    - statements: |
        on resource
        yield resource with {
          attributes: for (key, value) in resource.attributes
                      yield
                        if key == "k8s.namespace.name" 
                        then ("namespace", value)
                        else (key, value)
        }
```

Note: This assumes we have list compression (for-yield) concept *and* if-else expressions (like `<expr> ? <value> : <value>`)

An alternative syntax for list-comprehensions could follow python:

```
on resource
        yield resource with {
          attributes: [
                       (key, value) if key != "k8s.namespace.name" else ("namespace", value)
                       for (key,value) in resource.attributes
                      ]
        }
```


### Move field to attribute
Set attribute `body` to the value of the log body:

```yaml
transform:
  error_mode: ignore
  log_statements:
    - context: log
      statements: 
        - set(attributes["body"], body)
``` 

proposal:

```yaml
transform:
  error_mode: ignore
  log_statements:
    - statements: |
        on log
        yield log with {
          attributes: {
            body: log.body
          }
        }
```

### Combine two attributes
Set attribute `test` to the value of attributes `"foo"` and `"bar"` combined. 
```yaml
transform:
  error_mode: ignore
  trace_statements:
    - context: resource
      statements:
        # Use Concat function to combine any number of string, separated by a delimiter.
        - set(attributes["test"], Concat([attributes["foo"], attributes["bar"]], " "))
```

proposal:

```yaml
transform:
  error_mode: ignore
  trace_statements:
    - statements: |
        on resource
        yield resource with {
          "test":  "{attributes["foo"]} {attributes["bar"]}"
        }
``` 

Note: This uses string-formatting literals.

### Parsing JSON logs

Given the following json body

```json
{
  "name": "log",
  "attr1": "foo",
  "attr2": "bar",
  "nested": {
    "attr3": "example"
  }
}
```

add specific fields as attributes on the log:

```yaml
transform:
  error_mode: ignore
  log_statements:
    - context: log
      statements:
        # Parse body as JSON and merge the resulting map with the cache map, ignoring non-json bodies.
        # cache is a field exposed by OTTL that is a temporary storage place for complex operations.
        - merge_maps(cache, ParseJSON(body), "upsert") where IsMatch(body, "^\\{") 
          
        # Set attributes using the values merged into cache.
        # If the attribute doesn't exist in cache then nothing happens.
        - set(attributes["attr1"], cache["attr1"])
        - set(attributes["attr2"], cache["attr2"])
        
        # To access nested maps you can chain index ([]) operations.
        # If nested or attr3 do no exist in cache then nothing happens.
        - set(attributes["nested.attr3"], cache["nested"]["attr3"])
```
proposal:

```yaml
transform:
  error_mode: ignore
  log_statements:
    - statements: |
        on log
        when log.body is aStringMessage(ParsedJson(json))
        yield log or {
          attributes: {
            "attr1": json.attr1,
            "attr2": json.attr2,
            "nested.attr3": json.nested.attr3,
          }
        }
``` 

Note: Here we use `or` again to mean "merge if it doesn't exist".  We can
select a different operator, or use a syntax like:

```
maybe_insert(log, {
  attributes: {

  }
})
```

If we prefer function-looking syntax.

### Get Severity of an Unstructured Log Body

Given the following unstructured log body

```txt
[2023-09-22 07:38:22,570] INFO [Something]: some interesting log
```

You can find the severity using IsMatch:

```yaml
transform:
  error_mode: ignore
  log_statements:
    - context: log
      statements:
        - set(severity_number, SEVERITY_NUMBER_INFO) where IsString(body) and IsMatch(body, "\\sINFO\\s")
        - set(severity_number, SEVERITY_NUMBER_WARN) where IsString(body) and IsMatch(body, "\\sWARN\\s")
        - set(severity_number, SEVERITY_NUMBER_ERROR) where IsString(body) and IsMatch(body, "\\sERROR\\s")
```

proposal (this could be cleaned up):

```yaml
transform:
  error_mode: ignore
  log_statements:
    - statements: |
      on log
      when aStringMessage(body)
      yield log with {
        severity_number: 
          if IsMatch(body, "\\sINFO\\s") then SEVERITY_NUMBER_INFO
          else if IsMatch(body, "\\sWARN\\s") then SEVERITY_NUMBER_WARN
          else if IsMatch(body, "\\sERROR\\s") then SEVERITY_NUMBER_ERROR
          else nil
      }
```
Note: this also assumes an expression based if-else syntax `if <expr> then <expr> else <expr>`.  Alternative could be `<expr> ? <expr> : <expr>`.