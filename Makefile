RACKET = /usr/bin/racket
all: elliptic.rkt
	@$(RACKET) $<

ut:
	@$(RACKET) -t ut.rkt

.PHONY: all ut
