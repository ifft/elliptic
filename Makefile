RACKET = /usr/bin/racket

ut:
	@$(RACKET) -t ut_1.rkt
	@$(RACKET) -t ut_2.rkt
	@$(RACKET) -t ut_hash.rkt
	@$(RACKET) -t ut_crypto.rkt

rmd: rmd160.o rmd.c
	gcc -o rmd rmd160.o rmd.c
	

.PHONY: all ut
