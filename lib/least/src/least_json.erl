-module(least_json).

-compile([export_all]).

go() ->
	{ok, [Data]} = file:consult('sample.txt'),
	Short = lists:sublist(Data, 3),
	%lists:foldl(fun(El, Acc) -> io:format("El: ~p~n---~n", [El]) end, [], Short).
	Rt = lists:foldl(fun({artist, Dx}, Acc) -> [{struct, minimize_artist(Dx)}|Acc] end, [], Short),
	io:format("Rt: ~p~n",[Rt]),
	Px = iolist_to_binary(mochijson2:encode(Rt)),
	io:format("Px:~p~n", [Px]).

minimize_artist(Artist) ->
	[{name, proplists:get_value("name", Artist)},
	 {mbid, proplists:get_value("mbid", Artist)},
	 {url, proplists:get_value("url", Artist)}].

test() ->
	Data = {struct, [{strKey, <<"strVal">>}, {intKey, 10}, {arrayKey, [1, 2, 3]}]},
	mochijson2:encode(Data).
