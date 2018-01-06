RACKET = /usr/bin/racket

ut:
	@$(RACKET) -t ut_1.rkt
	@$(RACKET) -t ut_2.rkt
	@$(RACKET) -t ut_hash.rkt
	@$(RACKET) -t ut_crypto.rkt

.PHONY: all ut
