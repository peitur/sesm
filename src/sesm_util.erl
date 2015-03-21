-module( sesm_util ).


-export( [filter_process/3, proc_stat/1, get_pid_stat/1, get_processlist/0, get_processlist/1] ).
-export( [filter_parent/2, filter_name/2, filter_alias/2] ).


-define( PROC, "/proc" ).


filter_parent( List, Search ) ->
	filter_process( List, ppid, Search ).

filter_name( List, Search ) ->
	filter_process( List, name, Search ).

filter_alias( List, Search ) ->
	filter_process( List, alias, Search ).


filter_process( List, Type, Search ) ->
	filter_process(List, Type, Search, []).

filter_process( [], _, _, Ret ) -> Ret;

filter_process( [Item|List], Type, Search, Ret ) ->
	Name = proplists:get_value( Type, Item, undefined ),
	case is_same( Name, Search ) of
		true ->
			filter_process( List, Type, Search, [Item|Ret] );
		false ->
			filter_process( List, Type, Search, Ret )
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


get_pid_stat( ProcPid ) ->
	case filelib:is_dir( ?PROC++"/"++ProcPid ) of
		true -> proc_stat( ?PROC++"/"++ProcPid++"/stat" );
		false -> {error, nopid}
	end.


proc_stat( File ) ->
	case file:open( File, [read] ) of
		{ok, Handle} ->
			case file:read_line( Handle ) of
				{ok, Data} ->
					{ok, parse_stat_line( Data ) };
				eof ->
					file:close( Handle ),
					{error, eof};		
				{error, Reason} ->
					file:close( Handle ),
					{error, Reason}
			end;
		{error, Reason} ->
			error_message:error_msg( "[~p] ERROR Could not open file ~p : ~p ~n", [?MODULE, File, Reason] ), 
			{error, Reason}
	end.

get_processlist( ) ->
	get_processlist( ?PROC ).

get_processlist( Path ) ->
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
	