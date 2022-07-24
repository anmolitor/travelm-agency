consistencyHeadline = Consistency
consistencyPreamble = The time has come where we actually work with multiple input files.
  But what are the consequences?

consistencyMissingKeysHeadline = Missing keys
consistencyMissingKeysBody = When working with other internalization solutions, translations are usually just simple key value maps.
  And when you have multiple of them, there is no check that they actually contain the same keys. 
  Things are different here - since we analyse the translations at compile time already, we might as well check for completeness.
  This is great to provide guarantees for production but is really annoying in development, most notably when you are using
  some sort of watch mode that automatically runs Travelm-Agency on file changes.
  The solution: The flag <code>--devMode</code> disables the completeness check and result in an empty string if the translation
  is requested.     

consistencyFallbackHeadline = Explicit fallbacks
consistencyFallbackBody = I mentioned that it is great to provide guarantees for production. However, as good agile software engineers
  we want to release early and often, but translations are often an external dependency outside of the development team.
  To combat this scenario, you may declare a fallback language in any translation file. Missing translations will then be pulled from
  the referenced file (which may itself declare a fallback language).
  The only limitation here is that the fallback graph needs to be acyclic.

consistencyFallbackSyntaxJson = Since JSON does not allow any comments, the fallback language needs to specified as a top-level, 
  reserved key named <code>"--fallback-language"</code>.

consistencyFallbackSyntaxProperties = In the .properties format, any line beginning with "#" is considered a comment.
  A fallback language may be specified with a comment of the form <code>fallback-language: en</code> 
  (where "en" is the language to use as a fallback in this example).

consistencyFallbackSyntaxFluent = In the Fluent format, any line beginning with "#" is considered a comment.
  A fallback language may be specified with a comment of the form <code>fallback-language: en</code> 
  (where "en" is the language to use as a fallback in this example).

