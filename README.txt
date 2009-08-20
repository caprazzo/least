Meex - Last.fm extended API

artist.meex
-----------
Get artists by formula

http://meex.folli.es/2.0/artists/asf/<formula>/

Path components:
	formula (Required): The formula in question
	?format: json | xml
	
Response (Json format):
{"similarartists":
	{
		"artist":[
			{
				"name":"Sonny & Cher",
				"mbid":"3d6e4b6d-2700-458c-9722-9021965a8164",
				"match":"100",
				"url":"www.last.fm\/music\/Sonny%2B%2526%2BCher",
				"image":[
						{"#text":"http:\/\/userserve-ak.last.fm\/serve\/34\/4987379.jpg","size":"small"},
						{"#text":"http:\/\/userserve-ak.last.fm\/serve\/64\/4987379.jpg","size":"medium"},
						{"#text":"http:\/\/userserve-ak.last.fm\/serve\/126\/4987379.jpg","size":"large"},
						{"#text":"http:\/\/userserve-ak.last.fm\/serve\/252\/4987379.jpg","size":"extralarge"},
						{"#text":"http:\/\/userserve-ak.last.fm\/serve\/500\/4987379\/Sonny++Cher.jpg","size":"mega"}
				],
				"streamable":"1"
			},
			...
		]
	}


Response (Xml Format)
<similarartists formula="...">
	<artist>
		<name>Venetian Snares</name>
		<mbid>56abaa47-0101-463b-b37e-e961136fec39</mbid>
		<match>100</match>
		<url>/music/Venetian+Snares</url>
		<image>http://userserve-ak.last.fm/serve/160/211799.jpg</image>
	</artist>
        <artist>
...
Name is the artist name,
Url is last.fm info page on the artist
		
FORMULAS
========

Artist Similarity Formula (asf)
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
	
	(R.E.M * U2) ^ (Pearl Jam * Coldplay)

	
