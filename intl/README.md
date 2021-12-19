# Intl Proxy

This is a package intended to be used combined with the accompanying JS/Elm package.
You shouldn't need to use this package directly at all, but instead use the `Travelm-Agency` package
to generate the code for you. 

It uses a hack to achieve limited synchronous JS interop (with the browsers Intl API).
As it is intended to be used as a workaround and as a code generation target,
the API of this package is not particularly typesafe or well designed.