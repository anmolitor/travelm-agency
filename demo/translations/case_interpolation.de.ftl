headline = Fluent: Case Interpolation

preamble = Auch bekannt als Selektoren im Fluent Handbook, haben wir diesen Namen gewählt, da es Elm's
  <code>case .. of</code> Statement imitiert (und auch dazu kompiliert wird). 

syntaxHeadline = Syntax
syntaxBody = Alles was du tun musst ist ein normales Interpolations-Statement zu nehmen, einen Zeilen Umbruch nach dem
  Variablennamen hinzufügen und die darauf folgenden Zeilen um mindestens ein Leerzeichen einzurücken.
  Dann kannst du pro Zeile ein Match Statement: Der String auf den gematcht werden soll kommt in eckige Klammern
  und alles danach ist der assoziierte Wert.
  Stelle sicher dass du genau einen Fall als Default markierst, indem du die Zeile mit einem Stern <code>*</code> anfängst.

adviceHeadline = Tipp
adviceBody = Versuche dieses Feature selten zu nutzen. Gegen einen eigenen Union Type in Elm zu matchen ist oft viel sauberer.
  In dem aktuellen Beispiel hätten wir auch einen Datentyp <code>type Fruit = Apple | Banana | Other String</code> erstellen können
  mit drei seperaten Übersetzungen.
