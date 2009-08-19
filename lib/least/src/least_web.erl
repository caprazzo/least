-module(least_web).

-define(OK, <<"ok">>).

-export([start/0,stop/0, dispatch_requests/1]).

start() ->
	process_flag(trap_exit, true),
	least_resolver:start_link(),
	inets:start(),
	mochiweb_http:start([{port, 4321}, {loop, fun dispatch_requests/1}]).

stop() ->
  mochiweb_http:stop().

dispatch_requests(Req) ->
	Params = Req:parse_qs(),
	% Formula = proplists:get_value("formula", Params),
	Path = Req:get(path),
	case string:str(Path, "/2.0/artists/asf/") of
		0 -> 
			{ok, Index} = file:read_file("index.html"),
			Req:respond({200, [{"Content-Type", "text/html"}], Index});
		Any ->
			PathLen = string:len(Path),
			Formula = re:replace(string:sub_string(Path, string:len("/2.0/artists/asf/")+1),"/$","",[{return,list}]),
			{data, ResolvedFormula, Result} = resolve(Formula),
			Req:respond({200, [{"Content-Type", "text/javascript"}], response_json(Result)})
	end.
	%io:format("RX:~p~n",[Rx]),
	%Rq = re:split(Path, "/formula/", [{return,list}]),
	%io:format("SPLIT: ~p~n", [Formula]),
	%handle(Formula, Req).

handle(undefined, HttpRequest) ->
	{ok, Index} = file:read_file("index.html"),
	HttpRequest:respond({200, [{"Content-Type", "text/html"}], Index});

handle(Formula, HttpRequest) ->
	io:format("Formula:~p~n",[Formula]),
	{data, ResolvedFormula, Result} = resolve(Formula),
	HttpRequest:respond({200, [{"Content-Type", "text/javascript"}], response_json(Result)});

handle(Unknown, Req) ->
  Req:respond({404, [{"Content-Type", "text/plain"}], subst("Unknown action: ~s", [Unknown])}).

response_json(Result) ->
	Rt = lists:foldl(fun(Artist, Acc) -> [{struct, minimize_artist(Artist)}|Acc] end, [], Result),
	iolist_to_binary(mochijson2:encode(Rt)).

minimize_artist({Name, {artist, Props}}) ->
	[{name, Name},
	 {mbid, proplists:get_value("mbid", Props)},
	 {url, proplists:get_value("url", Props)}].

resolve(Formula) ->
	{ok, Expression} = least_parse:string(Formula),
	Result = least_resolver:resolve(Expression),
	Result.

error(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Body}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).


clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.

dump_data(Path, Data) ->
	file:write_file(Path,io_lib:fwrite("~p.\n",[Data])).
