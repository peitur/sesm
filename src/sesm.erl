-module( sesm ).

-export( [start/0, stop/0] ).


start() ->
	io:format("Starting monitor srvice... ~n"),
	application:start( sasl ),
	application:start( sesm ),
	ok.

stop() ->
	application:stop( sesm ).
