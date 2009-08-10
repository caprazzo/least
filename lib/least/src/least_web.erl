-module(least_web).

-define(OK, <<"ok">>).

-export([start/0,stop/0, dispatch_requests/1]).

start() ->
	least_resolver:start_link(),
	mochiweb_http:start([{port, 4321}, {loop, fun dispatch_requests/1}]).

stop() ->
  mochiweb_http:stop().

dispatch_requests(HttpRequest) ->
	{Action,_,_}=P = mochiweb_util:urlsplit_path(HttpRequest:get(path)),
	{ok, RE} = re:compile("/formula/"),
	[_, Formula] = re:split(Action, RE),
	io:format("P:~p J:~p ~n",[P, binary_to_list(Formula)]),
	F = binary_to_list(Formula),
	{data, F, Result} = resolve(binary_to_list(Formula)),
	HttpRequest:respond({200, [{"Content-Type", "text/plain"}], mochijson2:encode(Result)}).

resolve(Formula) ->
	{ok, Expression} = least_parse:string(Formula),
	Result = least_resolver:resolve(Expression),
	io:format("Result: ~p~n",[Result]),
	Result.

handle("/formula/", Req) ->
  Params = Req:parse_qs(),
  Nick = proplists:get_value("nick", Params),
  mucc:crash(Nick),
  success(Req, ?OK);

handle(Unknown, Req) ->
  Req:respond({404, [{"Content-Type", "text/plain"}], subst("Unknown action: ~s", [Unknown])}).

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
