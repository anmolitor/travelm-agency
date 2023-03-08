headline = Fluent: Formatting numbers

-intl-link = https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Intl/NumberFormat
-proxy-link = https://www.npmjs.com/package/intl-proxy

preamble = Wusstest du schon dass verschiedene Sprachen Zahlen unterschiedlich formatieren?
  Hier ist die Zahl "Ein-Tausend-Zwei-Hundert-Komma-Fünf" in Deutsch <code>1.200,5</code>
  und in Englisch <code>1,200.5</code>. Das falsche Format auf deiner Website anzuzeigen könnte deine Nutzer verwirren.

intlHeadline = Elm und die Intl APIs
intlBody = Zahlen in verschiedenen Arten zu formatieren ist eine Menge Arbeit.
  Zum Glück deckt die <a href="{-intl-link}">Intl API</a> dies ab. 
  Unglücklicherweise bietet Elm keinen einfachen Zugriff auf diese API.
  Die traditionellen Interop Wege wie Flags, Ports und Web Components funktionieren für diesen Usecase leider alle nicht besonders gut.
  Stattdessen verwenden wir die Interaktion zwischen JSON Decoders und ES6 Proxies um synchronen Zugiff zu erlauben.
  Damit dies funktioniert, wirst du <a href="{-proxy-link}">intl-proxy</a> per npm installieren und in deine Elm Applikation
  als Flag geben müssen. Der generierte Code wird dich dazu zwingen den Proxy zu übergeben
  wenn du die initiale <code>I18n</code> Instanz erstellst, danach solltest du den Proxy nie wieder explizit in
  deiner Applikation brauchen.

optionsHeadline = Zusätzliche Optionen
optionsBody = Die NumberFormat.format Methode kann eine Menge verschiedener Optionen erhalten, die <a href="{-intl-link}">hier</a>
  dokumentiert sind. Um sie zu verwenden, Um sie zu benutzen, kannst du sie als kommaseparierte Argumente in
  den NUMBER Funktionsaufruf geben. Der Aufruf <code>NUMBER($num, style: "percent")</code> wird z.B. das Argument an die Intl API
  weiterleiten, welche dann (in Deutsch) im Wesentlichen deine Zahl mit 100 multipliziert und ein '%' Zeichen ergänzt.
 