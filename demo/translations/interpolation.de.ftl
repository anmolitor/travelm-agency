headline = Interpolation

preamble = Diese Seite ist über das Feature Interpolation: Die Ersetzung eines Platzhalters innerhalb eines
  Textes durch einen Wert zur Laufzeit.

jsonSyntaxBody = JSON gibt keine Syntax für Platzhalter vor. Wir haben die bekannte Curly Bracket Syntax gewählt:
  Wenn du beispielsweise einen Wert in <code>"Hello, !"</code> einfügen willst für die Person die du grüßen möchtest, kannst du 
  <code>"Hello, { "{person}!" }"</code> schreiben. Für eine echte öffnende geschweifte Klammer kann man Backslash zum Escapen nutzen.

propertiesSyntaxBody = Properties gibt auch keine Syntax für Platzhalter vor. Wie bei JSON haben wir Curly Bracket Syntax gewählt:
  Wenn du beispielsweise einen Wert in <code>"Hello, !"</code> einfügen willst für die Person die du grüßen möchtest, kannst du 
  <code>"Hello, { "{person}!" }"</code> schreiben. Für eine echte öffnende geschweifte Klammer kann man Anführungszeichen zum Escapen nutzen.

fluentSyntaxBody = Fluent nutzt als Interpolationssyntax Curly Bracket Syntax mit einem Twist.
  Da die Syntax auch für andere Features genutzt werden kann muss die Variable die interpoliert werden soll mit einem "$" begonnen werden.
  Wenn du beispielsweise einen Wert in <code>"Hello, !"</code> einfügen willst für die Person die du grüßen möchtest, könntest du
  <code>"Hello, { "{$person}!" }"</code> schreiben. Für eine echte öffnende geschweifte Klammer nutze Fluents String Literals <code>{ "{ \"...\" }" }</code> zum Escapen.

generatedCodeHeadline = Generierter Code
generatedCodeBody = Wenn du den generierten Code für den Beispiel Input inspizierst, könnten dir die unterschiedlichen
  Typsignaturen verglichen mit der vorigen Seite auffallen. Statt der gewöhnlichen <code>I18n -> String</code> Signatur,
  hat Travelm-Agency eine Funktion mit dem Typ <code>I18n -> String -> String</code> für die Übersetzung für <code>greeting</code>
  und ein Funktion mit dem Typ <code>I18n -> { "{ day : String, todo : String }" } -> String</code>
  für die Übersetzung für <code>plan</code> generiert.
  Wenn man sich ihre Definitionen in der Inputdatei ansieht, macht das eine Menge Sinn.
  Die <code>greeting</code> Übersetzung signalisiert dass ein Wert an die markierte Stelle platziert wird und die
  <code>plan</code> platziert gleich zwei. Beliebige Anzahlen von interpolierten Werten sind unterstützt.
  Du kannst gerne ausprobieren wie sich der Compiler verhält wenn du Platzhalter im Eingabefenster hinzufügst oder löschst.

duplicateKeysHeadline = Doppelte Platzhalter Schlüssel
duplicateKeysBody = Wenn du den gleichen Wert an mehreren Stellen platzieren willst, gib ihnen einfach den gleichen Namen.
  Zum Beispiel: <code>"{ "{person}, {person}" }"</code> wird eine Funktion mit der Signatur <code>I18n -> String -> String</code> generieren
  obwohl zwei Platzhalter spezifiziert wurden. Wenn die Funktion zur Laufzeit mit <code>Evan</code> aufgerufen wird,
  kommt <code>Evan, Evan</code> raus.

inconsistentKeysHeadline = Inkonsistente Platzhalter
inconsistentKeysBody = Mnnchmal brauchen verschiedene Sprachen mehr oder weniger Platzhalter als andere, oder in einer anderen Reihenfolge.
  Travelm-Agency hat damit kein Problem. Für eine Übersetzung werden zur Laufzeit immer eine Anzahl Werte korrepondierend zur Vereinigung der
  spezifizierten Platzhalter über alle Sprachen hinweg benötigt. Der generierte Code kümmert sich dann darum die Werte an die korrekten
  Stellen zu platzieren.
