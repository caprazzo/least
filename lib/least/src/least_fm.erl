-module(least_fm).

-export([artist_getSimilar/2]).


-include_lib("xmerl/include/xmerl.hrl").


-define(SERVER, ?MODULE).
-define(API_KEY, "81ea54e9ad93005af93da6d65e25e7d1").
-define(API_URL, "http://ws.audioscrobbler.com/2.0/?method=").
-define(LIMIT, "&limit=3").

-record(artist, {
	name,
	mbid,
	match,
	url,
	images=[],
	streamable
}).

artist_getSimilar(Artist, Callback) ->
	Url = ?API_URL ++ "artist.getsimilar&artist=" ++ Artist ++ "&api_key=" ++ ?API_KEY ++ ?LIMIT, 
	{ok, Body} = fetch(Url),
	parse(Body, fun({ok,Artists}) -> Callback({ok, Artists}) end).

fetch(Url) ->
	case http:request(Url) of
		{ ok, { {_Http, 200, "OK"}, _Headers, Body }} ->
			{ok, Body};
		{ ok, { {_Http, _StatusCode, _StatusText}, _Headers, _Body }} ->
			 {error, {_Http, _StatusCode, _StatusText}}
	end.

attr(#xmlElement{attributes=A}) -> attr(A);
attr([#xmlAttribute{name=size, value=V}]) -> V.

val([]) -> "";
val(#xmlElement{}=El) -> val(El#xmlElement.content);
val([#xmlText{value=V}]) -> V.

parse(SimilarArtistsXml, Callback) ->
	io:format("Xml:~p~n",[SimilarArtistsXml]),
	Event = fun
		(#xmerl_event{event=started, data=#xmlElement{name=artist}}, GlobalState) ->
			{none, Acc} = xmerl_scan:user_state(GlobalState),
			Artist = #artist{},
		xmerl_scan:user_state({Artist,Acc}, GlobalState);
		(#xmerl_event{event=ended, data=#xmlElement{name=artist}}, GlobalState) ->
			{Artist, Acc} = xmerl_scan:user_state(GlobalState),
			xmerl_scan:user_state({none,[Artist|Acc]}, GlobalState);
		(_Event, GlobalState) ->
			GlobalState	
	end,
	Hook =  fun
		(#xmlElement{name=name}=El, GS) ->
			 {Artist, Acc} = xmerl_scan:user_state(GS),
			 {El, xmerl_scan:user_state({Artist#artist{name=val(El)},Acc}, GS)};
		(#xmlElement{name=mbid}=El, GS) ->
			 {Artist, Acc} = xmerl_scan:user_state(GS),
			 {El, xmerl_scan:user_state({Artist#artist{mbid=val(El)},Acc}, GS)};
		(#xmlElement{name=match}=El, GS) ->
			 {Artist, Acc} = xmerl_scan:user_state(GS),
			 {El, xmerl_scan:user_state({Artist#artist{match=val(El)},Acc}, GS)};
		(#xmlElement{name=url}=El, GS) ->
			 {Artist, Acc} = xmerl_scan:user_state(GS),
			 {El, xmerl_scan:user_state({Artist#artist{url=val(El)},Acc}, GS)};
		(#xmlElement{name=streamable}=El, GS) ->
			 {Artist, Acc} = xmerl_scan:user_state(GS),
			 {El, xmerl_scan:user_state({Artist#artist{streamable=val(El)},Acc}, GS)};	
		(#xmlElement{name=image}=El, GS) ->
			 {Artist, Acc} = xmerl_scan:user_state(GS),
			 Images = Artist#artist.images,
			 Url = {url, val(El)},
			 Size = {size, attr(El)},
			 {El, xmerl_scan:user_state({Artist#artist{images=[[Size,Url]|Images]},Acc}, GS)};				
		(Entity, Globalstate) ->
			 {Entity, Globalstate}
	 end,
	Close = fun(GS) ->
		Callback({ok, xmerl_scan:user_state(GS)})
	end,
	
	xmerl_scan:string(SimilarArtistsXml, [{event_fun, Event}, {hook_fun, Hook}, {close_fun, Close}, {user_state,{none,[]}}]),
	ok.
	


