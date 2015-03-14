#!/usr/bin/env escript

-define( PROC, "/proc" ).

main( Args ) ->
	case get_pricesslist( ?PROC ) of
		{ok, ProcList } -> 
			io:format(">>> ~p ~n", [ProcList] );
		{error, Reason } ->
			error_list:error_msg( "[~p] ERROR Error loading process list : ~p ~n" , [?MODULE, Reason] )
	end.


	

get_pricesslist( ) ->
	get_pricesslist( ?PROC ).

get_pricesslist( Path ) ->
	case file:list_dir( Path ) of
		{ok, FileList } -> 
			case proc_list( FileList ) of
				[] -> 
					io:format( "Empty~n");
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
	