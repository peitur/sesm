-module( node ).

-export([listen/0]).

listen() ->
        io:format("Listning on ~p ~n", [self()]),
        register( listner, self() ),
	listen( start ).	

listen( exit ) -> 
	io:format(" closing ...~n"),
	unregister( listner ),
	ok;

listen( Msg ) ->
	receive 
		Info ->
			io:format("Received: ~p ~n", [Info]),
			listen( Info )
	end.
