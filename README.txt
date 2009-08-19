Meex - Last.fm extended API

artist.meex
-----------
Get artists by formula

e.g. http://least.folli.es/2.0/?method=artist.meex&formula=asf:radiohead^thom yorke&api_key=... 

Params:
	formula (Required): The formula in question
	api_key (Required): a Meex api key (not really _required_ for the time being, but expect it to be)
	limit (Optional): limit the number of artists returned
		
		
FORMULAS
========

Artist Similarity Formula (asf:)
--------------------------------
Select a set of artists by their similarities with other artists

* union:
	artist1 * artist2 
	Set of artists similar to artist1 OR artist2 (OR both)
	Example:
		thom yorke * jonny greenwood
	
^ intersection:
	artist1 ^ artist2
	Set of artists similar to artist1 AND artist2
	Example:
		thom yorke ^ jonny greenwood
	
- difference
	artist1 - artist2
	Set of artists similar to artist1 BUT NOT similar to artist2
	Example:
		radiohead - thom yorke

	
