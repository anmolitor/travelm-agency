dateFormatHeadline = Fluent: Formatting dates

-intl-link = https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
-proxy-link = https://www.npmjs.com/package/intl-proxy

dateFormatPreamble = Dates can be presented in even more different ways than numbers can.
  Order of year/month/day differs depending on the language, and if you want to actually display the full
  weekday/month... you are going to have a bad time.

dateFormatIntlHeadline = Elm and the Intl APIs
dateFormatIntlBody = Luckily the browsers <a href="{-intl-link}">Intl API</a> helps us out once again. 
  Once again, due to Elms restrictions, you need to npm install <a href="{-proxy-link}">intl-proxy</a> 
  and pass it into your Elm application as a flag. The generated code will force you to pass this when creating the
  initial <code>I18n</code> instance, at which point you never need to use it explicitely in your program again.

dateFormatCompileTimeHeadline = Compile time execution
dateFormatCompileTimeBody = You may pass a string literal instead of a variable into the DATETIME function,
  which executes the function at compile-time and inlines the resulting formatted date. The date needs to be in
  ISO-8601 format for this to work.  

dateFormatOptionsHeadline = Additional options
dateFormatOptionsBody = The NumberFormat.format method can take a variety of different options which are documented
  <a href="{-intl-link}">here</a>. To use them, just add them as comma-seperated arguments to the DATETIME function call.
  For example, <code>DATETIME($date, dateStyle: "full")</code> will pass the argument to the Intl API, which will then
  show weekday and month in textual form (for example "Sunday, 20 December 2020").  