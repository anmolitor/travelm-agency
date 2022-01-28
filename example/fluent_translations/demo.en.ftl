-static-term = static term
staticTermKey = Example 1: {-static-term}s are supported.
-dynamic-term = dynamic, {$adjective} term
dynamicTermKey = Example 2: {-dynamic-term}s are supported.
-nested-term-1 = term
-nested-term-2 = nested {-nested-term-1}
nestedTermKey = Example 3: {-nested-term-2}s are supported as long as there is no circular dependency.

attributes = Example 4: Attributes are supported
  .title = Attributes
  .withVarAndTerm = { $var } { -static-term }

dateTimeFun = Example 5: DATETIME function is supported: {DATETIME($date, timeStyle: "long", dateStyle: "full")}
numberFun = Example 6: NUMBER function is supported: {NUMBER($num, style: "percent")}

-datetime = {DATETIME($date)}
-number = {NUMBER($num)}
compileTimeDatesAndNumbers = Example 7: DATETIME und NUMBER functions with known values are evaluated at compile time:
   {-datetime(date: "2022-01-18T10:30:44.807Z")}
   {-datetime(date: 100000)}
   {-number(num: 500000)}

matchOnGender = {$gender ->
  [male] He wants his break now
  *[female] She wants her break now
  }

displayGender = {$gender ->
  [male] Male
  [female] Female
  *[other] {$gender}
  }  

matchOnNumbers = { NUMBER($amount) ->
  [one] I drank a single beer
  *[other] I drank {$amount} beers
  }
  