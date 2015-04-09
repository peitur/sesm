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
-export([monitor_timer/2]).

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
	gen_server:cast( Pid, {validate, Options}).



get_name( Pid ) -> gen_server:call(Pid, {get, name} ).
get_title( Pid ) -> gen_server:call(Pid, {get, title} ).
get_pid( Pid ) -> gen_server:call(Pid, {get, pid} ).
get_ppid( Pid ) -> gen_server:call(Pid, {get, ppid} ).
get_expected( Pid ) -> gen_server:call(Pid, {get, expected} ).
get_current( Pid ) -> gen_server:call(Pid, {get, current} ).
get_starttime( Pid ) -> gen_server:call(Pid, {get, starttime} ).

get_config( Pid ) -> gen_server:call(Pid, {get, config} ).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { parent, name, title, pid = undefined, ppid = undefined, expected = running, current, start_time, conf, timeout, timerpid = undefined } ).



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
init([ ParentPid, Conf, _Options ]) ->
	erlang:process_flag( trap_exit, true ),

	case proplists:get_value( title, Conf, undefined ) of
		undefined -> 
			{stop, badarg};

		Title ->
			Name = proplists:get_value( name, Conf, Title ),

			ExpState = proplists:get_value( expected, Conf, ?MON_STATE ),
			StartTime = erlang:now(),
			Timeout = proplists:get_value( timeout, Conf, ?MON_TIMEOUT ),
			TimerPid = spawn_link( ?MODULE, monitor_timer, [self(), Timeout] ),


		    {ok, #state{ parent = ParentPid, conf = Conf, start_time = StartTime, name = Name, title = Title, expected = ExpState, current = down, timeout = Timeout, timerpid = TimerPid }, 0 }
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

handle_call( {get, config}, _From, State ) ->
	{ reply, {ok, State#state.conf }, State };

handle_call( {get, name}, _From, State ) ->
	{ reply, {ok, State#state.name }, State };

handle_call( {get, title}, _From, State ) ->
	{ reply, {ok, State#state.title }, State };

handle_call( {get, pid}, _From, State ) ->
	{ reply, {ok, State#state.pid }, State };

handle_call( {get, ppid}, _From, State ) ->
	{ reply, {ok, State#state.ppid }, State };

handle_call( {get, expected}, _From, State ) ->
	{ reply, {ok, State#state.expected }, State };

handle_call( {get, current}, _From, State ) ->
	{ reply, {ok, State#state.current }, State };

handle_call( {get, starttime}, _From, State ) ->
	{ reply, {ok, State#state.start_time }, State };



handle_call( {stop, Reason}, _From, State ) ->
	{ stop, Reason, ok, State };

handle_call( _Request, _From, State ) ->
    Reply = ok,
    {reply, Reply, State }.


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
handle_cast( {validate, _Options}, State ) ->
	{noreply, State, 0};

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
handle_info( timeout, #state{ name = Name, pid = undefined, expected = _Expected, current = _Current } = State ) ->
	case sesm_util:get_processlist( ?PROC ) of
		{ok, ProcList } ->

			case sesm_util:filter_by( ProcList, "1", ppid ) of
				{error, Reason} ->
					error_logger:error_msg( "[~p] ERROR: Filter fails ~p : ~p ~n", [?MODULE, Name, Reason] ),
					{noreply, State#state{ pid = undefined, current = down } };
				
				DaemonList ->

					case x_monitor_detect( Name, DaemonList ) of
						undefined ->
							% expected state, current state, new state
							io:format( "<<<< Down ~p ~n", [ Name ] ) ,
							{noreply, State#state{ pid = undefined, current = down } };
						ProcItem ->
							io:format( ">>>> Detected ~p ~n", [ Name ] ) ,
							Pid = proplists:get_value( pid, ProcItem ),
							ParentPid = proplists:get_value( ppid, ProcItem ),
							{noreply, State#state{ pid = Pid, ppid = ParentPid, current = up } }
					end

			end;
		{error, Reason} ->
			{noreply, State }
	end;


handle_info( timeout, #state{ name = Name, pid = Pid  } = State ) ->
	case x_verify_pid( Pid, Name ) of
		true -> {noreply, State }; % service pid has been kept
		false -> {noreply, State#state{ pid = undefined, ppid = undefined }, 0 } % state has changed or been restarted, use a state detectoin tp descide,
	end;


handle_info( {'EXIT', Pid, Reason}, #state{ name = Name, timerpid = Pid } = State ) ->
	error_logger:info_msg("[~p] INFO: Lost 'EXIT' timer for monitor ~p : ~p ~n", [?MODULE, Name, Reason] ),
	TimerPid = spawn_link( ?MODULE, monitor_timer, [self(), State#state.timeout ] ),
	{noreply, State#state{ timerpid = TimerPid} };


handle_info( {'EXIT', Pid, Reason}, #state{ name = Name } = State ) ->
	error_logger:info_msg("[~p] INFO: Lost 'EXIT' child for monitor ~p ~p: ~p ~n", [?MODULE, Name, Pid, Reason] ),
	{noreply, State };




handle_info( {'DOWN', MonitorReference, Process, Pid, Reason}, #state{ name = Name, timerpid = Pid } = State ) ->
	error_logger:info_msg("[~p] INFO: Lost 'DOWN' timer for monitor ~p ref:~p proc: ~p pid: ~p reason: ~p ~n", [?MODULE, Name, MonitorReference, Process, Pid, Reason] ),
	TimerPid = spawn_link( ?MODULE, monitor_timer, [self(), State#state.timeout ] ),
	{noreply, State#state{ timerpid = TimerPid} };

handle_info( {'DOWN', MonitorReference, Process, Pid, Reason}, #state{ name = Name } = State ) ->
	error_logger:info_msg("[~p] INFO: Lost 'DOWN' child for monitor ~p ref:~p proc: ~p pid: ~p reason: ~p ~n", [?MODULE, Name, MonitorReference, Process, Pid, Reason] ),
	{noreply, State };



handle_info( _Info, State) ->
    {noreply, State }.


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




monitor_timer( ParentPid, Timeout ) ->
	receive
		{timeout, NewTimeout} ->
			monitor_timer( ParentPid, NewTimeout );
		Other ->
			monitor_timer( ParentPid, Timeout )			
		after Timeout ->
			sesm_monitor:validate( ParentPid ),
			monitor_timer( ParentPid, Timeout )
	end.