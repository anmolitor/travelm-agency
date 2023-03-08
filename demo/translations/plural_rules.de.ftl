headline = Fluent: Plural Regeln

preamble = Manche Sprachen unterscheiden sich in ihrer Grammatik oder im Wortlaut basierend auf der Zahl von etwas. 
  Deutsch hat dieses Phänomen auch in einer sehr einfachen Form:
  Ein <b>Baum</b>, Zwei <b>Bäume</b>. Wäre es nicht cool wenn du auf diese Plural Formen matchen könntest?

-categories-link = https://www.unicode.org/cldr/cldr-aux/charts/30/supplemental/language_plural_rules.html

syntaxHeadline = Syntax
syntaxBody = Es stellt sich heraus: klar kannst du! Verpacke einfach einen <code>NUMBER</code> Funktionsaufruf um eine Variable
  innerhalb einer Case Interpolation. Jetzt matchst du nicht auf spezifische Zahlen, sondern auf <a href="{-categories-link}">Kategorien</a>.
  Die validen Kategorien sind 'zero', 'one', 'two', 'few', 'many' und 'other'.

intlHeadline = Intl API
intlBody = Nur eine Vorwarnung - du wirst die intl-proxy Abhängigkeit auch für diese Funktionalität brauchen.
  Die NumberFormat/DateFormat Seiten haben eine ausführliche Erklärung weshalb und was zu tun ist.