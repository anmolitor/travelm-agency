headline = Html
preamble = Previously, the return type for all the generated translation key functions was <code>String</code>. 
  But sometimes there are formatting and highlighting requirements for your texts that are frustrating to
  solve with just Strings. Coloring or emphasizing part of a sentence come to mind. Links inside of a sentence.
  While you may split up pre and post html string parts, you will run into ordering issues for some languages.
  That's why Travelm-Agency understands HTML and generates a function returning <code>List (Html Never)</code> instead, if it
  encounters any HTML tags.

basicsHeadline = Basics
basicsBody = To make Travelm-Agency generate HTML, all you need to do is wrap an HTML tag around a part of your translation.
  Currently, self-closing tags are not allowed. You may include HTML attributes, which will be added to your HTML element at runtime
  via <code>Html.Attributes.attribute</code>. Nested HTML tags also work as you would expect. HTML tags can be modified at runtime with more attributes, so you need to
  provide a list of attributes for each different html tag in your translation.

idHeadline = Identification
idBody = If you want to style a single html tag specifically, or multiple different html tags with the same attributes,
  you can use the special "_id" attribute on the respective tags.

securityHeadline = Security
securityBody = There is a good reason why Elm does not generate HTML from a String at runtime - cross-site scripting
  and similar techniques are a thing. In inline mode, the code is static and unchangeable, nullifying the security risks, in dynamic mode
  however, a parser is used that may create <b>ANY</b> HTML. Therefore, you need to be careful which contents you load into your I18n instance.
  If you just load your .json files from your own server, that should probably be fine. If you have a more complex setup, keep this risk in mind.

escapingHeadline = Escaping
escapingBody = Since Travelm-Agency understands HTML, the parser gets confused every time it finds a { "<" } symbol with a different meaning.
  Therefore, you have to escape the character with the usual techniques: Backslash <code>{ "\\<" }</code> for JSON, Quotes <code>{ "'<'" }</code> for Properties
  and String literals <code>{ "{ \"<\" }" }</code> for Fluent.