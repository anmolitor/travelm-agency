headline = Fluent: Formatting numbers

-intl-link = https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Intl/NumberFormat
-proxy-link = https://www.npmjs.com/package/intl-proxy

preamble = Did you know that different languages format numbers in different ways?
  Here is the number "one-thousand-two-hundred-point-five" in English: <code>1,200.5</code>
  and in German: <code>1.200,5</code>. Displaying the wrong format on your website might confuse your users.

intlHeadline = Elm and the Intl APIs
intlBody = Formatting numbers in a variety of different ways is a lot of work.
  Thankfully, the <a href="{-intl-link}">Intl API</a> has this covered. Unfortunately, Elm does not provide
  first class access to this API. The tradional interop ways like Flags, Ports and Web components do not work well for
  this usecase either. Instead, we use the interaction between JSON decoders and ES6 proxies to provide synchronous access.
  For this to work, you need to npm install <a href="{-proxy-link}">intl-proxy</a> and pass it into your Elm application
  as a flag. The generated code will force you to pass this when creating the initial <code>I18n</code> instance,
  at which point you never need to use it explicitely in your program again.

optionsHeadline = Additional options
optionsBody = The NumberFormat.format method can take a variety of different options which are documented
  <a href="{-intl-link}">here</a>. To use them, just add them as comma-seperated arguments to the NUMBER function call.
  For example, <code>NUMBER($num, style: "percent")</code> will pass the argument to the Intl API, which will then (in English) essentially
  multiply your number by 100 and add a '%' sign.