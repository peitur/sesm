#!/usr/bin/env escript

-define( PROC, "/proc" ).

main( Args ) ->
	case get_pricesslist( ?PROC ) of
		{ok, ProcList } ->

			lists:foreach( fun(E) -> io:format( "Process: ~p ~n", [ proc_stat( ?PROC++"/"++E++"/stat" ) ] ) end, ProcList );
		{error, Reason } ->
			error_list:error_msg( "[~p] ERROR Error loading process list : ~p ~n" , [?MODULE, Reason] )
	end.


filter_by( List, Pattern, By ) ->
	filter_by_opt( List, Pattern, By, [] ).

filter_by( List, Pattern, By, Opt ) ->
	filter_by_opt( List, Pattern, By, Opt, [] ).


filter_by_opt( [], _, _, , _, Ret ) ->
	Ret;

filter_by_opt( List, Pattern, By, Opt, Ret ) when is_list( By ) ->
	filter_by_opt( List, Pattern, list_to_atom( By ), Opt, Ret );

filter_by_opt( [Item|List], Pattern, By, Opt, Ret ) ->
	Subject = proplists:get_value( By, Item, undefined ),
	case is_same( Subject, Pattern ) of
		true ->
			filter_by_opt( List, Pattern, By, Opt, [Item|Ret] );
		false ->
			filter_by_opt( List, Pattern, By, Opt, Ret )
	end.





filter_process( List, Search ) ->
	filter_process(List, Search, []).

filter_process( [], _, Ret ) -> Ret;

filter_process( [Item|List], Search, Ret ) ->
	Name = proplists:get_value( name, Item, undefined ),
	case is_same( Name, Search ) of
		true ->
			filter_process( List, Search, [Item|Ret] );
		false ->
			filter_process( List, Search, Ret )
	end.

filter_process_rx( List, Pattern ) ->
	filter_process_rx( List, Pattern, [] ).

filter_process_rx( [], _ , Ret ) ->
	Ret;

filter_process_rx( [Item|List], Pattern, Ret ) ->
	Name = proplists:get_value( name, Item, undefined ),
	case re:run( Name, Pattern, [global] ) of
		match -> 
			filter_process_rx( List, Pattern, [Item|Ret] );
		{match, _} ->
			filter_process_rx( List, Pattern, [Item|Ret] );
		Other ->
			filter_process_rx( List, Pattern, Ret )
	end.


is_same( Str, Str ) -> true;
is_same( _Str1, _Str2 ) -> false.

parse_stat_line( Line ) ->
	List = re:split( Line, "\s+", [{return, list}] ),

	Pid = lists:nth(1,List),
	Name = lists:nth(2,List),
	State = lists:nth(3,List),
	ParentPid = lists:nth(4,List),

	[
		{pid, Pid},
		{name, re:replace( Name, "[\(\)]", "", [global, {return, list}] )},
		{state, State},
		{ppid, ParentPid}
	].

proc_stat( File ) ->
	case file:open( File, [read] ) of
		{ok, Handle} ->
			case file:read_line( Handle ) of
				{ok, Data} ->
					{ok, parse_stat_line( Data ) };
				eof ->
					file:close( Handle );
				{error, Reason} ->
					file:close( Handle )
			end;
		{error, Reason} ->
			error_message:error_msg("", [] )
	end.

get_pricesslist( ) ->
	get_pricesslist( ?PROC ).

get_pricesslist( Path ) ->
	case file:list_dir( Path ) of
		{ok, FileList } -> 
			case proc_list( FileList ) of
				[] -> 
					{ok, []};
				List ->
					{ok, List}
			end;
		{error, Reason} -> {error, Reason}
	end.


proc_list( List ) ->
	proc_list( List, [] ).

proc_list( [], Ret ) -> Ret;

proc_list( [File|List], Ret ) ->
	case re:run( File, "^[0-9]+$", [global] ) of
		match ->
			proc_list( List, [File|Ret ] );
		{match, _ } -> 	proc_list( List, [File|Ret ] );
		nomatch -> proc_list( List, Ret );
		{error, Reason} ->
			error_list:warning_msg( "[~p] WARN File list item ~p : ~p ~n", [?MODULE, File, Reason] )
	end.
	