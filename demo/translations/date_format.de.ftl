headline = Fluent: Ein Datum formatieren

-intl-link = https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
-proxy-link = https://www.npmjs.com/package/intl-proxy

preamble = Ein Datum kann in noch viel mehr Wegen angezeigt werden als eine Zahl.
  Reihenfolge von Jahr/Monat/Tag unterscheidet sich je nach Sprache und dann ist auch noch die Frage ob man
  die Kurz- oder Langschreibweise für Wochentage/Monate nutzt... von Hand macht das keinen Spaß.

intlHeadline = Elm und die Intl APIs
intlBody = Zum Glück hilft uns hier wieder die <a href="{-intl-link}">Intl API</a> des Browsers. 
  Auch hier gibt es durch Elms Restriktionen keinen Weg ohne <a href="{-proxy-link}">intl-proxy</a> zu installieren
  und in deine Applikation als Flag zu übergeben. Der generierte Code wird dich dazu zwingen den Proxy zu übergeben
  wenn du die initiale <code>I18n</code> Instanz erstellst, danach solltest du den Proxy nie wieder explizit in
  deiner Applikation brauchen.

compileTimeHeadline = Ausführung zur Compilezeit
compileTimeBody = Du kannst ein ISO-8601 formatiertes String Literal statt einer Variable in die DATETIME Funktion geben.
  Damit wird die Funktion zur Compilezeit ausgeführt und inlined das resultierende formatierte Datum.

optionsHeadline = Zusätzliche Optionen
optionsBody = Die NumberFormat.format Methode kann eine Menge verschiedener Optionen erhalten, die
  <a href="{-intl-link}">hier</a> dokumentiert sind. Um sie zu benutzen, kannst du sie als kommaseparierte Argumente in
  den DATETIME Funktionsaufruf geben. Der Aufruf <code>DATETIME($date, dateStyle: "full")</code> z.B. wird das Argument
  an die Intl API weiterleiten, welche dann Wochentag und Monat in textueller Form anzeigt (also z.B. "Sunday, 20 December 2020").
 