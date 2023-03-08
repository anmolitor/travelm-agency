headline = Einführung
preamble = Willkommen zum Travelm-Agency Tutorial. Die folgenden Seiten sind Dokumentation und Einführung in Travelm-Agency in einem interaktiven Playground.
explanationHeadline = Was ist Travelm-Agency?
explanationBody = Travelm-Agency ist ein Compiler der die Arbeit mit internationalisierten Texten in der Programmiersprache Elm
  erleichtert. Anstatt Elm Dateien zu schreiben die alle Texte einer Sprache beinhalten mit Funktionen die abhängig von der aktuellen Sprache
  den korrekten Text zurückgeben, kannst du auch Texte in einem von mehreren unterstützten Datenformaten schreiben.
  Travelm-Agency liest dann diese Dateien ein und generiert den passenden Elm Code für dich.
advantagesHeadline = Vorteile gegen über handgeschriebenen Code
advantageReadabilityHeadline = Lesbarkeit von Übersetzungen
advantageReadabilityBody = Texte sind in einem dafür ausgelegten Format wie <code>.properties</code> or <code>.json</code>
  viel lesbarer. Natürlich sind Texte auch in einer Elm Datei völlig in Ordnung, aber wenn Konzepte wie Interpolation ins Spiel kommen
  schadet das oft der Lesbarkeit, während Travelm-Agency diese Probleme auf Compiler Ebene beheben kann.
advantageTypeSafetyHeadline = Typsicherheit
advantageTypeSafetyBody = Der größte Vorteil von Elm gegenüber anderen Web Sprachen ist die große Sicherheit zur Compilezeit.
  Im Falle von Internationalisierung geht diese Sicherheit oft verloren für die Einfachheit der Entwicklung.
  Für jede Übersetzung eine eigene Funktion schreiben ist nerviger Boilerplate Code, entweder wechselt man zu einem <code>Dict String String</code>
  und/oder verwendet eine Platzhalter Syntax die nicht garantiert dass alle notwendigen Parameter auch zur Laufzeit gesetzt sind.
  Travelm-Agency übernimmt den nervigen Boilerplate Teil und stellt sicher dass
  <ul _id="list"><li _id="item">Alle Übersetzungen in jeder Sprache definiert sind (oder einen expliziten Fallback haben)</li><li _id="item">Platzhalter und ähnliche Konzepte zu Parametern der aufrufenden Funktionen werden.</li></ul>
advantagePerformanceHeadline = Performanz
advantagePerformanceBody = Elms Bundle Größe ist insgesamt relativ klein aber kann schnell wachsen wenn es viele Texte in vielen
  verschiedenen Sprachen auf einer Seite gibt. Die ganzen inaktiven Übersetzungen zu laden ist einen Großteil der Zeit unnötig.
  Mit Travelm-Agency kannst du zwischen <code>inline</code> und <code>dynamic</code> mode mit nur einem Kommandozeilen Argument 
  und nur wenigen Code Anpassungen wechseln, und damit deine Übersetzungen zur Laufzeit nachladen lassen.
  Da die Umstellung so einfach ist, kannst du Bundle Größen benchmarken und Ladezeiten vergleichen um zu entscheiden,
  welche Lösung für deine Anwendung am besten passt - und möglicherweise später wechseln.
disadvantagesHeadline = Nachteile
disadvantageProgrammabilityHeadline = Programmierbarkeit
disadvantageProgrammabilityBody = Wenn du deine Übersetzungen in Elm schreibst, hast du eine vollständige Programmiersprache
  zur Verfügung. Eigene Datentypen, Pattern matching, Hilfsfunktionen, und so weiter. Mit einem Compiler kann man nicht mehr
  so einfach mit dem Code interagieren und ist stattdessen an das generierte Interface gebunden.
  Meiner Meinung nach ist das vollkommen ok, da Travelm-Agency genug "programmierbare" Teile bietet,
  wie Interpolation, Matchen auf interpolierten Werten und automatische Html Generierung mit überschreibbaren Attributen.
disadvantageToolchainHeadline = Build Komplexität
disadvantageToolchainBody = Frontend Entwicklung ist voll von Build Tools, Bundlers, Code Generatoren überall.
  Elm selbst kompiliert schließlich zu JS. Es kann einschüchternd sein noch mehr Tools wie dieses hier zu seiner Liste hinzuzufügen.
  Der Compiler ist verhältnismäßig schnell und einfach zum Projekt hinzuzufügen, falls Probleme auftreten gerne ein Issue erstellen.
tutorialHowtoHeadline = Wie das Tutorial funktioniert
tutorialHowtoBody = Dieses Tutorial führt dich durch die Features von Travelm-Agency. Auf jeder Seite werden ein oder mehrere
  Eingabefenster und ein oder mehrere Ausgabefenster sichtbar sein, die dir ein bestimmtes Feature zeigen, mit einem zugehörigen Erklärungstext.
  Du kannst den Text in den Eingabefenstern verändern und beobachten wie sich die Ausgabe verändert.
tutorialMobileAdditional = Du scheinst auf einem Mobilgerät zu sein. Obwohl du nicht gleichzeitig Text und Beispiele sehen kannst,
  solltest du nach rechts scrollen können und die Eingabe- und Ausgabefenster zu sehen.
textsFeatureHeadline = Einfache Texte
textsFeatureBody = Es klingt vielleicht merkwürdig dies ein Feature zu nennen aber dies ist der meistgenutzte Übersetzungstyp.
  Beobachte wie Texte in Pattern Matching Funktionen für Inline Mode kopiert werden und in ein JSON Array für Dynamic Mode.
