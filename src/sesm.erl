-module( sesm ).

-export( [start/0, stop/0] ).


start() ->
	application:start( sesm ).

stop() ->
	application:stop( sesm ).
