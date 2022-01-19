-static-terms = termes statiques
staticTermKey = Exemple 1 : {-static-terms} sont pris en charge.
-dynamic-terms = termes {$adjective} dynamique
dynamicTermKey = "Exemple 2 : {-dynamic-terms} sont pris en charge.
-nested-terms-1 = termes
-nested-terms-2 = {-nested-terms-1} imbriqués
nestedTermKey = Exemple 3 : {-nested-terms-2} sont pris en charge tant qu'il n'y a pas de dépendance circulaire.

attributes = Exemple 4 : Les attributs sont pris en charge
  .title = Attribute
  .withVarAndTerm = { $var } { -static-terms }

dateTimeFun = Exemple 5 : DATETIME fonction est prise en charge: {DATETIME($date)}
numberFun = Exemple 6 : NUMBER fonction est prise en charge: {NUMBER($num, style: "percent")}

-datetime = {DATETIME($date)}
-number = {NUMBER($num)}
compileTimeDatesAndNumbers = Exemple 7 : les fonctions DATETIME et NUMBER avec des valeurs connues sont évaluées au moment de la compilation:
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
  