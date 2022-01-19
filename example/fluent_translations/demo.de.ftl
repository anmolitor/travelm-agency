
-static-terms = Statische Terme
staticTermKey = Beispiel 1: {-static-terms} sind unterstützt.
-dynamic-terms = Dynamische, {$adjective} Terme
dynamicTermKey = Beispiel 2: {-dynamic-terms} sind unterstützt.
-nested-terms-1 = Terme
-nested-terms-2 = Verschachtelte {-nested-terms-1}
nestedTermKey = Beispiel 3: {-nested-terms-2} sind unterstützt solange es keine zirkuläre Abhängigkeit gibt.

attributes = Beispiel 4: Attribute sind unterstützt
  .title = Attribute
  .withVarAndTerm = { $var } { -static-terms }

dateTimeFun = Beispiel 5: DATETIME Funktion wird unterstützt: {DATETIME($date, dateStyle: "full")}
numberFun = Beispiel 6: NUMBER Funktion wird unterstützt: {NUMBER($num, style: "percent")}

-datetime = {DATETIME($date)}
-number = {NUMBER($num)}
compileTimeDatesAndNumbers = Beispiel 7: DATETIME und NUMBER Funktion mit bekanntem Werten werden zur Compilezeit ausgewertet:
   {-datetime(date: "2022-01-18T10:30:44.807Z")}
   {-datetime(date: 100000)}
   {-number(num: 500000)}

matchOnStrings = {$gender ->
  [male] He wants his break now
  *[female] She wants her break now
  }

matchOnNumbers = { NUMBER($amount) ->
  [one] I drank a single beer
  *[other] I drank many beers
  }