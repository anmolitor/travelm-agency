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
