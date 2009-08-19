LEAST_LIB=lib/least
FERMAL_LIB=lib/fermal
CACHE_LIB=lib/cache
MOCHIWEB_LIB=lib/mochiweb

libs:
	cd $(LEAST_LIB) && make && cd - && \
	cd $(FERMAL_LIB) && erl -make && cd - && \
	cd $(CACHE_LIB) && erl -make && cd - && \
	cd $(MOCHIWEB_LIB) && make
	
run:
	erl -pa $(MOCHIWEB_LIB)/ebin \
		-pa $(LEAST_LIB)/ebin \
		-pa $(FERMAL_LIB)/ebin \
		-pa $(CACHE_LIB)/ebin \
		-boot start_sasl \
		-s least start