# Getting Started

To play with one of the examples, start the REPL and load the appropriate namespace.

    lein repl
    user=> (use 'clojure-actors.ping-pong)
    user=> (ping-pong 10)
    user=> (use 'clojure-actors.sleeping-barber)
    user=> (sleeping-barber 10)
    user=> (use 'clojure-actors.dining-philosophers)
    user=> (dining-philosophers 5 3)
