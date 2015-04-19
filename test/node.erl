-module( node ).


% lists:foreach( fun( X ) -> lists:foreach( fun( N ) -> N ! {hello, X} end, Noden ) end, Messages ).
-define( NODE_CONFIG,
  	{ sesm_service, [
  		'nodeaa@slimjim.bredbandsbolaget.se',
  		'nodeaa@slimjack.bredbandsbolaget.se',
  		'nodeaa@slimjane.bredbandsbolaget.se',
  		'nodeaa@slimjen.bredbandsbolaget.se'
  	]}
).

-export([listen/0, detect/0, service/0, service/2]).

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

service() ->
	service( ?NODE_CONFIG ).

service( { Service, NodeList } ) ->
	service( Service, NodeList ).

service( Service, List ) ->
	service( Service, List, [] ).

service( Service, [], Res ) ->
	Res;

service( Service, [ Node | List ], Res ) ->
	io:format("Testing ~p ~n", [Node]),
	case Service:ping( Node ) of
		pong ->
			service( Service, List, [Node | Res] );
		Other ->
			service( Service, List, Res )
	end.




detect() ->
	detect( ?NODE_CONFIG ).

detect( { _, NodeList } ) when is_list( NodeList ) ->
	detect( NodeList );

detect( NodeList ) ->
	sesm_net:detect_nodes( NodeList ).