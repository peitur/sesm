-module( sesm_monitor ).
-behaviour(gen_server).


-include("../include/sesm.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3, start_monitor/3, stop/1, stop/2]).
-export([get_pid/1, get_ppid/1, get_current/1, get_expected/1, get_starttime/1]).
-export([ validate/1, validate/2]).

% application:start( sasl ).
% application:start(sesm).
% sesm_monitor_sup:start_monitor( self(), [{name,"apache2"}], [] ).

start_link( ParentPid, Conf, Options ) ->
	gen_server:start_link( ?MODULE, [ParentPid, Conf, Options], [] ).

start_monitor( ParentPid, Conf, Options ) ->
	sesm_monitor_sup:start_monitor( ParentPid, Conf, Options ).

stop( Pid ) ->
	stop( Pid, normal ).

stop( Pid, Reason ) ->
	gen_server:call( Pid, {stop, Reason} ).



validate( Pid ) ->
	validate( Pid, [] ).

validate( Pid, Options ) ->
	gen_server:call( Pid, {validate, Options}).



get_pid( Pid ) -> gen_server:call(Pid, {get, pid} ).
get_ppid( Pid ) -> gen_server:call(Pid, {get, ppid} ).
get_expected( Pid ) -> gen_server:call(Pid, {get, expected} ).
get_current( Pid ) -> gen_server:call(Pid, {get, current} ).
get_starttime( Pid ) -> gen_server:call(Pid, {get, starttime} ).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { parent, name, title, pid = undefined, ppid = undefined, expected = running, current, start_time, conf, timeout } ).



%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([ ParentPid, Conf, Options ]) ->
	erlang:process_flag( trap_exit, true ),

	case proplists:get_value( title, Conf, undefined ) of
		undefined -> 
			{stop, badarg};

		Title ->
			Name = proplists:get_value( name, Conf, Title ),

			ExpState = proplists:get_value( expected, Conf, ?MON_STATE ),
			StartTime = erlang:now(),
			Timeout = proplists:get_value( timeout, Conf, ?MON_TIMEOUT ),

		    {ok, #state{ parent = ParentPid, conf = Conf, start_time = StartTime, name = Name, title = Title, expected = ExpState, current = down, timeout = Timeout }, 0 }
	end.

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

handle_call( {get, pid}, _From, State ) ->
	{ reply, {ok, State#state.pid }, State, State#state.timeout };

handle_call( {get, ppid}, _From, State ) ->
	{ reply, {ok, State#state.ppid }, State, State#state.timeout };

handle_call( {get, expected}, _From, State ) ->
	{ reply, {ok, State#state.expected }, State, State#state.timeout };

handle_call( {get, current}, _From, State ) ->
	{ reply, {ok, State#state.current }, State, State#state.timeout };

handle_call( {get, starttime}, _From, State ) ->
	{ reply, {ok, State#state.start_time }, State, State#state.timeout };



handle_call( {stop, Reason}, _From, State ) ->
	{ stop, Reason, ok, State, State#state.timeout };

handle_call( _Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, State#state.timeout}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast( _Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================




%% Initial timeout, used when pid has not yet been descided
handle_info( timeout, #state{ name = Name, pid = undefined, expected = Expected, current = Current } = State ) ->
	case sesm_util:get_processlist( ?PROC ) of
		{ok, ProcList } ->

			case sesm_util:filter_by( ProcList, "1", ppid ) of
				{error, Reason} ->
				
					{noreply, State#state{ pid = undefined, current = down }, State#state.timeout };
				
				DaemonList ->

					case x_monitor_detect( Name, DaemonList ) of
						undefined ->
							% expected state, current state, new state
							io:format( "<<<< Down ~p ~n", [ Name ] ) ,
							{noreply, State#state{ pid = undefined, current = down }, State#state.timeout };
						ProcItem ->
							io:format( ">>>> Detected ~p ~n", [ Name ] ) ,
							Pid = proplists:get_value( pid, ProcItem ),
							ParentPid = proplists:get_value( ppid, ProcItem ),
							{noreply, State#state{ pid = Pid, ppid = ParentPid, current = up }, State#state.timeout }
					end

			end;
		{error, Reason} ->
			{noreply, State, State#state.timeout }
	end;


handle_info( timeout, #state{ name = Name, pid = Pid  } = State ) ->
	case x_verify_pid( Pid, Name ) of
		true -> {noreply, State, State#state.timeout }; % service pid has been kept
		false -> {noreply, State#state{ pid = undefined, ppid = undefined }, 0 } % state has changed or been restarted, use a state detectoin tp descide,
	end;


handle_info( _Info, State) ->
    {noreply, State, State#state.timeout}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate( Reason, _State) ->
	error_logger:info_msg("[~p] INFO: Terinating monitor :~p ~n", [?MODULE, Reason] ),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change( _OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

x_monitor_detect( _Name, [] ) ->
	undefined;

x_monitor_detect( Name, [Item|List] ) ->
	case proplists:get_value( name, Item, undefined ) of
		Name -> Item;
		undefined -> x_monitor_detect( Name, List ); 
		Other -> x_monitor_detect( Name, List )
	end.


x_verify_pid( Pid, Name ) when is_list( Pid ) ->
	x_verify_pid( list_to_integer( Pid ), Name );

x_verify_pid( Pid, Name ) ->
	case sesm_util:proc_stat( Pid ) of
		{error, Reason} -> false;
		{ok, Item} -> 
			case sesm_util:is_same( Name, proplists:get_value( name, Item, undefined ) ) of
				true -> true;
				false -> false
			end
	end.


