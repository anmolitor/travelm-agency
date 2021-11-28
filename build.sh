# !/bin/bash

elm make --optimize --output=elm.tmp.js src/Main.elm
uglifyjs elm.tmp.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | node_modules/uglify-js/bin/uglifyjs --mangle --output dist/elm.min.js
rm elm.tmp.js