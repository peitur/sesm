-module( sesm ).

-export( [start/0, stop/0] ).

-export( [get_monitor_list/0, get_qqueue_list/0, get_qqueue_size/0] ).
-export( [get_nodelist/0]).


start() ->
	io:format("Starting monitor srvice... ~n"),
	application:start( sasl ),
	application:start( sesm ),
	ok.

stop() ->
	application:stop( sesm ).


get_monitor_list( ) ->
	sesm_service:get_monitor_list().

get_qqueue_list() ->
	sesm_service:get_qqueue_list().

get_qqueue_size() ->
	sesm_service:get_qqueue_size().




get_nodelist() ->
	sesm_service:get_nodelist().