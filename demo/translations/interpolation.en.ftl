interpolationHeadline = Interpolation

interpolationPreamble = This page is about the feature of interpolation: placing a placeholder in a string
  to later replace it with a runtime value.

syntaxHeadline = Syntax
jsonHeadline = .json
propertiesHeadline = .properties
fluentHeadline = .ftl

jsonSyntaxBody = JSON does not specify a placeholder syntax. We chose the familiar curly bracket syntax:
  If you want to insert a runtime value in <code _id="code">"Hello, !"</code> for the person you want to greet, you could write 
  <code _id="code">"Hello, { "{person}!" }"</code> for example. If you want an actual opening curly bracket, use backslash to escape it.

propertiesSyntaxBody = Properties does not specify a placeholder syntax either. Just like with JSON, we chose curly bracket syntax:
  If you want to insert a runtime value in <code _id="code">"Hello, !"</code> for the person you want to greet, you could write 
  <code _id="code">"Hello, { "{person}!" }"</code> for example. If you want an actual opening curly bracket, use quotation marks to escape it.

fluentSyntaxBody = Fluent uses curly bracket syntax with a twist for its interpolation syntax.
  Since you can use the syntax to signal other features as well, the variable you want to interpolate needs to be prefixed with a "$".
  If you want to insert a runtime value in <code _id="code">"Hello, !"</code> for the person you want to greet, you could write 
  <code _id="code">"Hello, { "{$person}!" }"</code> for example. If you want an actual opening curly bracket, use string literals <code _id="code">{ "{ \"...\" }" }</code> to escape it.

generatedCodeHeadline = Generated code
generatedCodeBody = When you inspect the generated code for the example input, you will notice different type signatures compared to
  the previous page. Instead of the usual <code _id="code">I18n -> String<code> signature, Travelm-Agency generated a function of type
  <code _id="code">I18n -> String -> String<code> for the <code>greeting</code> key and a function of type <code>I18n -> { "{ day : String, todo : String }" } -> String<code>
  for the <code>plan</code> key. Looking at their definitions, this makes a lot of sense. The <code>greeting</code> translation
  states that it will place one runtime value at the marked location and the <code>plan</code> translation does so for two.
  Any number of interpolations are supported. Feel free to try it out by adding new interpolations or removing them from the input file.

duplicateKeysHeadline = Duplicate interpolation keys
duplicateKeysBody = If you want to insert the same runtime value at multiple locations, just give them the same name.
  For example, <code _id="code">"{ "{person}, {person}" }"</code> will generate a <code _id="code">I18n -> String -> String<code>
  signature despite the fact that two placeholders have been specified and at runtime, calling the function with <code>Evan</code> will
  result in <code>Evan, Evan</code>

inconsistentKeysHeadline = Inconsistent interpolation keys
inconsistentKeysBody = Sometimes, one language needs more or less interpolated values than others, or in a different order.
  Travelm-Agency has no issues with that. You will always need to provide runtime values corresponding to the union of specified
  interpolation keys across all of your languages, the generated code will then take care of inserting the values in the correct places.




