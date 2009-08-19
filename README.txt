Meex - Last.fm extended API

artist.meex
-----------
Get artists by formula

e.g. http://meex.folli.es/2.0/artists?formula=asf:radiohead^thom yorke&api_key=... 

http://meex.folli.es/2.0/artists/asf/radiohead*queen/

Params:
	formula (Required): The formula in question
	
Response (Json format):
[
	{"name": ".....", "mbid":"....", "url":"url"},
	...
]
Name is the artist name,
Url is last.fm info page on the artist
		
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
		
Multiple operations can be combined in a single formula, using parenthesis:

	radiohead - (thom yorke * jonny greenwood)
	
	(R.E.M + U2) / (Pearl Jam + Coldplay)

	
