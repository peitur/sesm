-module( sesm_service ).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export( [start_link/0, start_link/1, stop/0, stop/1, stop/2] ).
-export( [add_service/2, remove_service/2, modify_config/2] ).
-export( [get_processes/0, get_processes/1, get_processes/2] ).
-export( [monitor_query/5] ).
-export( [get_monitor_list/0, get_monitor_list/1, get_qqueue_list/0, get_qqueue_list/1, get_qqueue_size/0] ).

-include("../include/sesm.hrl").

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { monitor_map, service_config, query_queue = [], timeout, quorum = [] }).

start_link() ->
	start_link( [] ).

start_link( Options ) ->
	gen_server:start_link( {local, ?MODULE}, ?MODULE, Options, [] ).

stop( ) ->
	stop( ?MODULE, normal ).

stop( Reason ) ->
	stop( ?MODULE, Reason ).

stop( Node, Reason ) ->
	gen_server:call( Node, {stop, Reason } ).





add_service( Name, Options ) ->
	gen_server:call( ?MODULE, {add, service, Name, Options} ).

remove_service( Name, Options ) ->
	gen_server:call( ?MODULE, {stop, service, Name, Options} ).

modify_config( Name, Options ) ->
	gen_server:call( ?MODULE, {modify, config, Name, Options} ). 





get_processes() ->
	get_processes( ?MODULE , all ).

get_processes( Filter ) ->
	get_processes( ?MODULE, Filter ).

get_processes( Node, Filter ) ->
	gen_server:call( Node, {get, processes, Filter} ). 



get_monitor_list() ->
	gen_server:call( ?MODULE, {get, monitor, all } ).

get_monitor_list( Node ) ->
	gen_server:call( Node, {get, monitor, all } ).



get_qqueue_list() ->
	gen_server:call( ?MODULE, {get, queue, list } ).

get_qqueue_list( Node ) ->
	gen_server:call( Node, {get, queue, list } ).


get_qqueue_size() ->
	gen_server:call( ?MODULE, {get, queue, size } ).


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
init( Options ) ->
	erlang:process_flag( trap_exit, true ),

	Quorum = case proplists:get_value( quorum, application:get_all_env( sesm ), undefined ) of
		undefined -> [];
		[{ Proc, NodeList }] ->
			lists:map( fun( Node ) -> {Proc, Node } end, NodeList )
	end,

	case  proplists:get_value( service, application:get_all_env( sesm ), undefined ) of
		undefined ->
			error_logger:error_msg("[~p] ERROR: Missing Service Config (undefined)", [ ?MODULE ]),
			{stop, missing_config};
		Config ->
			{ok, #state{ service_config = Config, query_queue = [], quorum = Quorum }, ?MON_STARTIME }
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
handle_call( {get, monitor, all}, From, #state{ monitor_map = Map, query_queue = Q } = State ) ->

	Ref = erlang:make_ref(),
	Pid = erlang:spawn( ?MODULE, monitor_query, [ status, Ref, self(), Map, [] ]),
	io:format("#### Stared ~p ~n", [Pid] ),

    { noreply, State#state{ query_queue = [{Ref, From, Pid}|Q] } };

handle_call( {get, queue, size}, _From, #state{query_queue = Q} = State ) ->
	{reply, {ok, erlang:length( Q ) }, State};

handle_call( {get, queue, list}, _From, #state{query_queue = Q} = State ) ->
	{reply, {ok, Q}, State};

handle_call( {stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call(Request, From, State) ->
    Reply = undefined,
    {reply, Reply, State}.


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
handle_cast(Msg, State) ->
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
handle_info( timeout, #state{ service_config = Config } = State ) ->

	case x_monitor_init( Config ) of
		{error, Reason} -> 
			{noreply, State };
		InitMap -> 	
			{noreply, State#state{ monitor_map = InitMap } }
	end;	




handle_info( {'EXIT', Pid, Reason}, #state{ monitor_map = Map } = State ) ->
% register monitor pid changes in map
	error_logger:info_msg("[~p] INFO: Lost 'EXIT' monitor ~p : ~p ~n", [?MODULE, Pid, Reason] ),
	{noreply, State#state{ monitor_map = lists:keydelete( Pid, 2, Map ) } };

handle_info( {'DOWN', MonitorReference, Process, Pid, Reason}, #state{ monitor_map = Map } = State ) ->
% register monitor pid changes in map
	error_logger:info_msg("[~p] INFO: Lost 'DOWN' monitor ref:~p proc: ~p pid: ~p reason: ~p ~n", [?MODULE, MonitorReference, Process, Pid, Reason] ),
	{noreply, State#state{ monitor_map = lists:keydelete( Pid, 2, Map ) } };


handle_info( {reply, status, Ref, Ret}, #state{ query_queue = Q } = State ) ->
	case lists:keysearch( Ref, 1, Q ) of
		false -> {noreply, State};
		{value, {Ref, From, QueuePid}} -> 
			Q2 = lists:keydelete( Ref, 1, Q ),
			gen_server:reply( From, Ret ),
			{noreply, State#state{ query_queue = Q2 } }
	end;

handle_info(Info, State) ->
    {noreply, State}.



	

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
	error_logger:info_msg("[~p] INFO: Terinating service :~p ~n", [?MODULE, Reason] ),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================






x_check_monitor( SysProc, SysName ) ->
	case sesm_util:proc_stat( sesm_util:get_procstat( SysProc) ) of
		{ok, Stat} -> {ok, Stat};
		{error, Reason} -> {error, Reason}
	end.


x_pidlist_running( List ) ->
	x_pidlist_running( List, [], [] ).

x_pidlist_running( [], RetRunning, RetMissing ) ->
	{ok, [RetRunning, RetMissing]};

x_pidlist_running( [{SysProc, SysName} | List], RetRunning, RetMissing ) ->
	case x_check_monitor( SysProc, SysName ) of
		{ok, State} ->
			x_pidlist_running( List, [{SysProc, SysName}|RetRunning], RetMissing );
		{error, Reason} ->
			x_pidlist_running( List, RetRunning, [{SysProc, SysName}|RetMissing] )
	end.




% -spec x_monitor_init( ConfList ) -> Map.

x_monitor_init( ConfList ) ->
	 x_monitor_init( ConfList, [] ).

x_monitor_init( [], Ret ) ->
	lists:reverse( Ret );

x_monitor_init( [ {Sign, Conf}|List], Ret ) ->
	case sesm_monitor:start_monitor( self(), [{title, Sign}|Conf], [] ) of
		{ok, Pid} ->
			x_monitor_init( List, [{Sign, Pid}|Ret] );
		{error, Reason} ->
			error_logger:error_msg( "[~p] ERROR: Error starting monitor for ~p : ~p ~n", [?MODULE, Sign, Reason] ),
			x_monitor_init( List, Ret )
	end.





monitor_query( status, Ref, ReplyTo, BroadcastList, Options ) ->

	Ret = lists:map( fun( {Sign,Pid} ) -> 
						case sesm_monitor:get_current( Pid ) of
							{ok, Current} -> {Sign, Current};
							{error, Reason} -> {Sign, {error, Reason}}
						end
					end, BroadcastList ),

	ReplyTo ! {reply, status, Ref, Ret};


monitor_query( _Type, Ref, ReplyTo, _BroadcastList, _Options ) ->
	ReplyTo ! {ok, Ref, {error, badargs}}.








 % {undef,
 % 	[{sesm_service,x_monitor_query,
 % 		[ status, #Ref<0.0.0.5562> , <0.53.0> , [{sasl,<0.55.0>},{postgresql,<0.56.0>},{mysql,<0.57.0>},{apache,<0.58.0>}] , [] ],
 % 		[]
 % 	}]
 % }