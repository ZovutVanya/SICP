#lang racket

#| Eva Lu Ator types to the interpreter the expression |#

(car ''abracadabra) ; 'quote

#| To her surprise, the interpreter prints back quote. Explain. |#

#| The interpreter expands 'abracadabra to (quote abracadabra) |#
(display ''abracadabra) ; (quote abracadabra)
(car '(quote abracadabra)) ; 'quote

