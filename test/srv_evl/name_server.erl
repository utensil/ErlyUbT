-module(name_server).
-export([init/0, add/2, whereis/1, exception/1, handle/2]).
-import(server3, [rpc/2]).

%% client routines
add(Name, Place) ->
   rpc(name_server, {add, Name, Place}).
whereis(Name) ->
   rpc(name_server, {whereis, Name}).
exception(N) ->
   rpc(name_server, {exception, N}).

%% callback routines
init() -> 
    dict:new().

handle({add, Name, Place}, Dict) ->
    {ok, dict:store(Name, Place, Dict)};

handle({whereis, Name}, Dict) ->
    {dict:find(Name, Dict), Dict};
% to test if the server can handle exception
handle({exception, N}, _) ->
    case N of
        1 -> n;
        2 -> throw(n);
        3 -> exit(n);
        4 -> {'EXIT', n};
        5 -> erlang:error(n);
        0 -> 1/N
   end.