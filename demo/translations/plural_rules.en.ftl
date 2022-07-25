pluralRulesHeadline = Fluent: Plural Rules

pluralRulesPreamble = Some languages differ in their grammatic or wording based on
  the amount of something. English also has this phenomenon in a very basic form: 
  One <b>page</b>, Two <b>pages</b>. What if you could match on these plural forms?

-categories-link = https://www.unicode.org/cldr/cldr-aux/charts/30/supplemental/language_plural_rules.html

pluralRulesSyntaxHeadline = Syntax
pluralRulesSyntaxBody = Well it turns out you can! Simply wrap a <code>NUMBER</code>
  function call around you variable in a case interpolation. Now you won't be matching on specific
  numbers but on <a href="{-categories-link}">categories</a>. The valid categories are 
  'zero', 'one', 'two', 'few', 'many' and 'other'.  

pluralRulesIntlHeadline = Intl API
pluralRulesIntlBody = Just a heads-up - you will need to include the intl-proxy package for this feature as well.
  Look at the NumberFormat/DateFormat page for reasoning and instructions.