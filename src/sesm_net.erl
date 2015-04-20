-module( sesm_net ).

-export([
		detect_nodes/1
	]).

-include( "../include/sesm.hrl" ).





detect_nodes( QouList ) ->
	detect_nodes( QouList, [] ).

detect_nodes( [], DetList ) ->
	DetList;

detect_nodes( [Node|List], DetList ) ->
	try net_adm:ping( Node ) of
		pong -> 
			detect_nodes( List, [Node|DetList] );
		pang -> 
%			error_logger:warning_msg( "[~p] WARN: Node ~p was not available (pang) ~n", [?MODULE, Node]),
			detect_nodes( List, DetList )
	catch Error:Reason ->
			error_logger:warning_msg( "[~p] ERROR: Pinging ~p failed with ~p:~p ~n", [?MODULE, Node, Error, Reason]),
			detect_nodes( List, DetList )			
	end.
