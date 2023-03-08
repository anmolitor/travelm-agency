headline = Sprachen
preamble = Wenn du deine Anwendung ohne travelm-agency bauen würdest, wie würdest du die aktuelle Sprache typisieren.
  Ich würde wahrscheinlich einen Union Typ nutzen mit einem Konstruktor pro Sprache. Dieser Code wird auch von
  Travelm-Agency generiert.

conversionsHeadline = Umwandlungen
conversionsBody = Ein Union Typ ist zwar nett, aber zumindest Serialisierung und Deserialisierung zu einem <code>String</code> ist notwendig um etwas sinnvolles damit anzufangen.
  Travelm-agency generiert <code>languageToString</code>
  und <code>languageFromString</code> für dich! Die coole Sache über letztere Methode ist, dass das Matching nicht exakt sondern präfix-basiert ist.
  Also keine Angst, <code>en-US</code> wird trotzdem zu <code>Just En</code> geparsed.

activeLanguageHeadline = Welche Sprache ist aktiv?
activeLanguageBody = Ist das nicht eine triviale Frage? Die Sprache die der User ausgewählt hat. Die Sprache deren Übersetzungen ich sehen kann.
  Moment. Was passiert wenn ich Übersetzungen dynamisch nachlade und der Request noch nicht fertig ist?
  Normalerweise gibt es zwei interessante Sprachen zu einem beliebigen Zeitpunkt.
  Die Sprache in die unsere Anwendung sein sollte, und die Sprache in der unsere Anwendung gerade ist.
  Daher werden für diese beiden Sprachen Funktionen generiert: <code>currentLanguage</code> and <code>arrivedLanguage</code>.
  Wenn mehr Logik als das notwendig ist, solltest du vermutlich die Sprache zusätzlich in deinem eigenen Model speichern
  (oder ein Issue aufmachen, vielleicht haben andere Nutzer das gleiche Problem).
