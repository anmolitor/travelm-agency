-static-terms = "Statische Terme"
staticTermKey = Beispiel 1: {-static-terms} sind unterstützt.
-dynamic-terms = "Dynamische, {$adjective} Terme"
dynamicTermKey = "Beispiel 2: {-dynamic-terms} sind unterstützt.
-nested-terms-1 = Terme
-nested-terms-2 = Verschachtelte {-nested-term-1}
nestedTermKey = "Beispiel 3: {-nested-term-2} sind unterstützt solange es keine zirkuläre Abhängigkeit gibt.

attributes = Beispiel 4: Attribute sind unterstützt
  .title = Attribute
  .withVarAndTerm = { $var } { -static-terms }

dateTimeFun = Beispiel 5: DATETIME Funktion wird unterstützt: {DATETIME($date, dateStyle: "full")}
numberFun = Beispiel 6: NUMBER Funktion wird unterstützt: {NUMBER($num, style: "percent")}
