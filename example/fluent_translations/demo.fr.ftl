-static-terms = "termes statiques"
staticTermKey = Exemple 1 : {-static-terms} sont pris en charge.
-dynamic-terms = "termes {$adjective} dynamique"
dynamicTermKey = "Exemple 2 : {-dynamic-terms} sont pris en charge.
-nested-terms-1 = termes
-nested-terms-2 = {-nested-term-1} imbriqués
nestedTermKey = "Exemple 3 : {-nested-term-2} sont pris en charge tant qu'il n'y a pas de dépendance circulaire.

attributes = Exemple 4 : Les attributs sont pris en charge
  .title = Attribute
  .withVarAndTerm = { $var } { -static-terms }

dateTimeFun = Exemple 5 : DATETIME fonction est prise en charge: {DATETIME($date)}
numberFun = Exemple 6 : NUMBER fonction est prise en charge: {NUMBER($num, style: "percent")}
