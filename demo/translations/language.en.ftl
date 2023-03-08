headline = Languages
preamble = When you build your application without travelm-agency, how would you type the current language?
  I would probably use a union type with one constructor for each of my languages. So this is the
  code that travelm-agency generates.

conversionsHeadline = Conversions
conversionsBody = Having a union type is nice, but you need to be able to at least serialize and deserialize 
  from a <code>String</code> to do anything useful with it. Travelm-agency generates <code>languageToString</code>
  and <code>languageFromString</code> for you! The cool thing about the latter is that it does not match exactly but based on a prefix.
  So no worries, <code>en-US</code> will still get parsed into <code>Just En</code>.

activeLanguageHeadline = Which language is active?
activeLanguageBody = Isn't this trivial? The one which the user selected. The one whose translations I see on screen.
  Wait. What happens if I load the language files dynamically and the request hasn't completed yet?
  Usually there are two interesting languages at any given point in time.
  The language which we want our application to be in, and the language the application is in right now.
  We generate accessors for these two with <code>currentLanguage</code> and <code>arrivedLanguage</code>.
  You can watch the difference live when you throttle your Network Speed in the DevTools and then switch the language by clicking one of the flags in the top middle.
  If you need fancier logic than this, you probably need to store the language in your own model in addition
  (or you can open an issue, maybe other users have your problem as well).