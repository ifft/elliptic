RACKET = /usr/bin/racket

ut:
	@$(RACKET) -t ut_1.rkt
	@$(RACKET) -t ut_2.rkt
	@$(RACKET) -t ut_hash.rkt

.PHONY: all ut
