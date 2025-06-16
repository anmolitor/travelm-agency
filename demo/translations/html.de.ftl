headline = Html
preamble = Bisher war der Rückgabetyp für alle generierten Übersetzungsfunktionen <code>String</code>.
  Aber manchmal gibt es Formattierungs- oder Hervorhebungsanforderungen an Texte die frustrierend mit Strings zu lösen sind.
  Ein gutes Beispiel ist einen Teil eines Satzes zu Färben oder Fett anzuzeigen. Obwohl du Vor- und Nach-HTML String Teile in
  verschiedene Übersetzungen aufteilen kannst, wirst du Probleme mit der Reihenfolge in manchen Sprachen haben.
  Deshalb versteht Travelm-Agency HTML Syntax und generiert Funktionen mit dem Rückgabetyp <code>List (Html msg)</code> wenn
  es HTML Tags in einer Übersetzung findet.

basicsHeadline = Basics
basicsBody = Damit HTML aus einer Übersetzung generiert wird, musst du nur einen HTML Tag um einen Teil deiner
  Übersetzung packen. Selbstschließende Tags sind aktuell nicht erlaubt. 
  Du kannst HTML Attribute verwenden, die dann zur Laufzeit zu deinem Element per <code>Html.Attributes.attribute</code> hinzugefügt werden.
  Verschachtelte HTML Tags sollten auch wie erwartet funktionieren.
  Damit die HTML Tags zur Laufzeit mit zusätzlichen Attributen modifiziert und konfiguriert werden können musst du eine Liste
  von Attributen für jeden unterschiedlichen HTML Tag in einer Übersetzung übergeben.

idHeadline = Identifizierung
idBody = Wenn du einen bestimmten HTML Tag stylen möchtest oder mehrere unterschiedliche HTML Tags mit den selben Attributen,
  kannst du das spezielle "_id" Attribut in den jeweiligen Tags verwenden.

securityHeadline = Sicherheit
securityBody = Es gibt gute Gründe wieso Elm nicht HTML aus Strings zur Laufzeit generiert - z.B. Cross-Site Scripting.
  Im Inline Mode ist der Code statisch und nicht veränderbar, womit kein Risiko entsteht.
  Im Dynamic Mode wird hingegen ein Parser genutzt um letztendlich beliebiges HTML zur Laufzeit zu erstellen.
  Daher solltest du vorsichtig sein welche Inhalte du in deine I18n Instanz lädst.
  Wenn du deine Übersetzungen von deinem eigenen Server lädst sollte das vermutlich kein Problem sein.
  Bei komplexeren Setups sollte dir das Risiko aber bewusst sein.

escapingHeadline = Escaping
escapingBody = Da Travelm-Agency HTML versteht, ist der Parser verwirrt wenn er ein { "<" } symbol findet was nicht für HTML gedacht war.
  Daher muss dieser Char mit den üblichen Techniken escaped werden: Backslash <code>{ "\\<" }</code> für JSON, Quotes <code>{ "'<'" }</code> für Properties
  und String Literale <code>{ "{ \"<\" }" }</code> für Fluent.

customizingHeadline = Anpassungen
customizingBody = Travelm-Agency ermöglicht es, einige seiner Ausgaben anzupassen. Insbesondere durch das Setzen von <code>--custom_html_module</code>
  kann das importierte Html-Modul geändert werden, das der generierte Code deklariert. Dies setzt automatisch auch
  <code>--custom_html_attributes_module</code> auf das Html-Modul + .Attributes, da dies der De-facto-Standard ist:
  <a href="https://package.elm-lang.org/packages/miniBill/elm-html-with-context/latest/">elm-html-with-context</a>,
  <a href="https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/">elm-css</a> und natürlich das Standard
  <a href="https://package.elm-lang.org/packages/elm/html/latest/">elm/html</a> verwenden diese Struktur.
  Du kannst <code>--custom_html_attributes_module</code> aber auch explizit angeben, falls andere Bibliotheken oder dein eigener
  elm/html-Wrapper sich in dieser Hinsicht unterscheiden.
  Die Schnittstelle, die von einer benutzerdefinierten Html-Implementierung benötigt wird, ist in diesen Test-Dateien spezifiziert:
  <a href="https://github.com/anmolitor/travelm-agency/tree/main/gen_test_cases/Util/CustomHtml.elm">CustomHtml</a>
  <a href="https://github.com/anmolitor/travelm-agency/tree/main/gen_test_cases/Util/CustomHtmlAttributes.elm">CustomHtmlAttributes</a>