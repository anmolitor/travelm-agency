introHeadline = Introduction
introPreamble = Welcome to the Travelm-Agency tutorial. Here, you get to see all the features of Travelm-Agency, presented in examples
  in a playground setting.
introExplanationHeadline = What is Travelm-Agency?
introExplanationBody = Travelm-Agency is a compiler that simplifies working with internationalized texts in the Elm programming language.
  Instead of writing Elm files containing all of your texts for all languages, and writing functions to access the
  correct texts depending on the current language, you can write your texts in one of several data formats.
  Travelm-Agency then reads in these files and generates the Elm code you may have otherwise written yourself.
advantagesHeadline = Advantages over handwritten code
advantageReadabilityHeadline = Readability of translations
advantageReadabilityBody = Texts are much more readable in a format like .properties or .json.
  Don't get me wrong, texts are also perfectly fine in an Elm file, but when you add in things like interpolation
  and more advanced concepts, readability tends to suffer, while Travelm-Agency can tackle these features at a compiler level.
advantageTypeSafetyHeadline = Type safety
advantageTypeSafetyBody = The biggest advantage when using Elm over other web solutions is the compile time safety.
  For internationalisation, that safety often goes out the window for ease of development. Functions for each translation key
  are annoying to write boilerplate code, you either go towards a Dict String String and/or use some special placeholder syntax
  which does not guarantee all placeholders are filled at runtime.
  Travelm-Agency does the annoying boilerplate part for you and makes sure that
  - All keys are defined in all languages (or have an explicit fallback)
  - Placeholders and similar concepts get converted to parameters for the respective accessor functions
advantagePerformanceHeadline = Performance
advantagePerformanceBody = Elm's bundle size is generally pretty small, but can get bloated if you have lots of texts in
  different languages on your site. Loading all the inactive languages is a waste most of the time.
  With Travelm-Agency you can switch from "inline" to "dynamic" mode with just a command line argument and a few small code changes
  and have your translations loaded on the fly instead. Since the switch is so simple, you can benchmark bundle sizes and load times
  and decide yourself which model suits your application best and possibly switch later. 
disadvantagesHeadline = Disadvantages
disadvantageProgrammabilityHeadline = Programmability
disadvantageProgrammabilityBody = When writing translations in Elm, you have the full power of Elm at your disposal.
  You can define your own data types, pattern match, define helper functions, all that stuff.
  When using a compiler, you cannot interact on a low level with the code, you are bound to the exposed interface.
  I personally think that is completely fine since Travelm-Agency offers enough "programmable" pieces
  like interpolation, string matching on an interpolated value and automatic html generation with overwritable attributes.
disadvantageToolchainHeadline = Build complexity
disadvantageToolchainBody = Frontend development is full of build tools and bundlers, with code generators left and right
  (Elm itself compiles to JS after all). It can be intimidating to add even more tools like this one to the list.
  The compiler is generally pretty fast and easy to add to your project, feel free to open an issue if you had any problems.

tutorialHowtoHeadline = How this tutorial works
tutorialHowtoBody = This tutorial guides you through the features of Travelm-Agency. On each page will be one or multiple input
  windows and one or multiple output windows showing you a particular feature with an accompanying explanation. You may edit
  the text in the input windows and observe how the output files change.

textsFeatureHeadline = Simple texts
textsFeatureBody = It may be weird to call this a feature, but this is the most common translation type that you will need.
  Observe how texts get copied into language records for inline mode and into a JSON array for dynamic mode.
