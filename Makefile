LEAST_LIB=lib/least
FERMAL_LIB=lib/fermal
CACHE_LIB=lib/cache

libs:
	cd $(LEAST_LIB) && erl -make
	cd $(FERMAL_LIB) && erl -make
	cd $(CACHE_LIB) && erl -make