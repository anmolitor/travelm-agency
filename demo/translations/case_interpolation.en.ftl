headline = Fluent: Case interpolation

preamble = Also known as selectors in the Fluent handbook, we chose this name because it mimics (and compiles to)
  Elms <code>case .. of</code> statement. 

syntaxHeadline = Syntax
syntaxBody = All you need to do to get started is take a normal interpolation statement, insert a line-break after
  the variable name and indent the following lines by at least one space. Then, you provide match statements, line by line.
  The string to match against is written in square brackets and everything after that is the value.
  Make sure to mark exactly one case as the default case with a leading star <code>*</code>.

adviceHeadline = Advice
adviceBody = Do not rely on this feature too much. Matching against a custom data type in Elm code is often
  much cleaner. In the current example, we could have also created a data type <code>type Fruit = Apple | Banana | Other String</code>
  and three seperate translation keys for the respective cases.
    