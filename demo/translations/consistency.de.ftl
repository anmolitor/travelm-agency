headline = Konsistenz
preamble = Die Zeit ist gekommen mit mehreren Input Dateien zu arbeiten! Aber was sind die Konsequenzen?

missingKeysHeadline = Fehlende Übersetzungen
missingKeysBody = In anderen Lösungen für Internationalisierung sind Übersetzungen oft einfach Key Value Maps.
  Und wenn es mehrere davon gibt, gibt es keine Checks dass diese die gleichen Keys enthalten.
  Hier ist es anders - da wir die Übersetzungen schon zur Compilezeit analyisieren, können wir auch direkt auf Vollständigkeit
  prüfen. Das ist zwar super für die Produktion aber nervig für Entwicklung, besonders bei der Benutzung eines Watch Modes, der
  Travelm-Agency bei jeder Dateiänderung ausführt.
  Die Lösung: Die Flag <code>--devMode</code> schaltet die Vollständigkeitsprüfung aus und führt zu einem leeren String
  wenn die Übersetzung angefragt wird.

fallbackHeadline = Explizite Fallbacks
fallbackBody = Wie schon erwähnt ist es eine gute Idee Eigenschaften für die produktiv laufende Software garantieren zu können.
  Doch als gute agile Softwareentwickler wollen wir früh und oft Software ausliefern. Übersetzungen sind häufig auch eine externe Abhängigkeit
  außerhalb des Entwicklungsteams. Um diese Szenario zu behandeln gibt es die Möglichkeit in jeder Übersetzungsdatei eine Fallback Sprache zu deklarieren.
  Fehlende Übersetzungen werden dann von der referenzierten Datei gezogen (die selbst wieder auf eine weitere Datei verweisen kann).
  Die einzige Limitation ist dass der Fallback Graph azyklisch sein muss. 

fallbackSyntaxJson = Da JSON keine Kommentare erlaubt muss die Fallback Sprache in einem top-level Key deklariert werden: 
  dem reservierten Key <code>"--fallback-language"</code>.

fallbackSyntaxProperties = Im .properties Format ist jede Zeile die mit "#" beginnt ein Kommentar.
  Eine Fallback Sprache kann mit einem Kommentar der Form <code>fallback-language: en</code> deklariert werden
  (wobei "en" die Sprache ist auf die zurückgefallen werden soll).

fallbackSyntaxFluent = Im Fluent Format ist jede Zeile die mit "#" beginnt ein Kommentar.
  Eine Fallback Sprache kann mit einem Kommentar der Form <code>fallback-language: en</code> deklariert werden
  (wobei "en" die Sprache ist auf die zurückgefallen werden soll).

