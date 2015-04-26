-module( ip ).


-compile([export_all]).

-define( DEF_MASK, "24" ).


dprint( Str ) -> 
	try ip:ip_to_bin( Str ) of
		{ok, Arr, Mask} ->
			io:format("MASK: ~p : ~p ~n", [Mask, Arr] ),
			lists:foreach( fun( E ) ->  io:format(" ~p ", [ E ] ) end ,  Arr ),

			BitStr = lists:concat( Arr ),
			io:format( ">> ~p ~n", [ BitStr ] )
	catch 
		Error:Reason ->
			io:format("~p : ~p ~n", [Error, Reason])
	end.

ip_to_bin( Str ) ->

	case re:split( Str, "[/]", [{return, list}] ) of
		[IpStr] -> 
			Arr = re:split( IpStr, "[.]", [ {return, list} ] ),
			Mask = erlang:integer_to_binary( erlang:list_to_integer( ?DEF_MASK ) ),
			{ok, convert_ip_to_bin( Arr, Mask ), Mask };
			
		[IpStr, Mask0] -> 
			Arr = re:split( IpStr, "[.]", [ {return, list} ] ),
			Mask = erlang:integer_to_binary( erlang:list_to_integer( Mask0 ) ),
			{ok, convert_ip_to_bin( Arr, Mask ), Mask };

		Other -> erlang:error(notipaddress)
	end.




convert_ip_to_bin( List, Mask ) -> 
	convert_ip_to_bin( List, Mask, [] ).

convert_ip_to_bin( [], Mask, Ret ) -> 
	lists:reverse(Ret);

convert_ip_to_bin( [Part|List], Mask, Ret ) ->
	Bin = erlang:integer_to_binary( erlang:list_to_integer( Part ) ),
	convert_ip_to_bin( List, Mask, [ Bin | Ret ] ).

