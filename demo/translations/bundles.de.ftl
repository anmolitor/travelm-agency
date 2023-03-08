headline = Übersetzungs-Bundles
preamble = Bisher ging es immer um eine Übersetzungsdatei pro Sprache. In großen Applikationen könnten wir davon profitieren
  nicht alle Übersetzungen für eine Sprache auf einmal zu laden, sondern nur die Übersetzungen für die aktuelle Seite oder
  für die aktuellen Baum von Seiten zu laden. Dieses Feature ist nur in <code>Dynamic</code> Mode relevant,
  da in <code>Inline</code> Mode sowieso alle Übersetzungen zusammen im Elm Bundle landen.

considerationsHeadline = Überlegungen
considerationsBody = Stell sicher dass du das Feature brauchst bevor du es benutzt.
  Es verringert die Compilezeit Garantien für deine Applikation da du daran denken musst die richtigen Bundles auf den richtigen Seiten einzubinden.
  Wenn eine Übersetzung noch nicht geladen wurde, gibt die jeweilige Funktion einen leeren String zurück.
  Aktuell kompiliert Travelm-Agency alle Übersetzungsschlüssel in eine flache Liste von Exports, was nicht wirklich dabei hilft die
  Dateien von denen sie kommen auseinander zu halten. Präfixe können helfen, daher bietet
  Travelm-Agency eine <code>--prefix_file_identifier</code> Flag um den Bundle Name automatisch vor jede Funktion packen zu lassen.
  Wenn es dann eine Übersetzung "headline" in der Datei "summary.en.ftl" gibt, wird eine Funktion mit dem Namen "summaryHeadline" generiert.
  Falls du feststellst dass Bundling einen großen Einfluss auf eine gute Renderzeit hat, lass es mich wissen. Ich bin offen für Pull Requests und
  Ideen den generierten Code zu verbessern.

exploreHeadline = Entdecke
exploreBody = Der Editor lässt dich auf dieser Seite neue Dateien erstellen! Füge neue Bundles hinzu
  und beobachte wie die Ausgabe sich ändert.
