-module( sesm_service_net ).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/3, start_service_listner/3, stop/1, stop/2]).

%% ====================================================================
%% API functions
%% ====================================================================


start_link( Parent, Type, Options ) ->
	gen_server:start_link( {local, ?MODULE}, ?MODULE, [Parent, Type, Options] ).

start_service_listner( Parent, Type, Options ) ->
	sesm_service_net_sup:start_service_net( Parent, Type, Options ).

stop( Pid ) ->
	stop( Pid, normal ).

stop( Pid, Reaon ) ->
	gen_server:call( Pid, {stop, Reaon} ).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { parent, type, options, handle = undefined }).

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

init([Parent, Type, Options]) ->
    {ok, #state{ parent = Parent, type = Type, options = Options }, 0}.


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

handle_call( {stop, Reason}, From,  #state{ handle = undefined } = State) ->
	gen_tcp:close( State#state.handle ),
	{stop, Reason, ok, State#state{ handle = undefined } };

handle_call( {stop, Reason}, From, State) ->
	{stop, Reason, ok, State};

handle_call(Request, From, State) ->
    Reply = ok,
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
handle_info( {tcp, Socket, Message}, State ) ->
	
	{noreply, State};

handle_info( timeout, #state{ type = remote, handle = undefined} = State ) ->
	{noreply, State};

handle_info( timeout, #state{ type = local, handle = undefined} = State ) ->
	case proplists:get_value( port, State#state.options, undefined ) of
		undefined -> {stop, {error, missingport} };
		Port ->
			case gen_tcp:listen( Port, [binary] ) of
				{error, Reason} -> 
					error_logger:error_msg( "[~p] ERROR: Opening listner port ~p : ~p ~n", [?MODULE, Port, Reason] ),
					{stop, {error, Reason}};
				{ok, Listner} ->
					case gen_tcp:accept( Listner, infinity ) of
						{error, Reason} -> {stop, {error, Reason}};
						{ok, Socket} -> 

							{noreply, State#state{ handle = Socket }, 0 }
					end						
			end
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


x_health_status( Pid ) ->
	try sesm_monitor:get_status( Pid ) of
		{ok, ServiceStatus } -> ServiceStatus
	catch 
		Error:Reason -> {error, Reason}
	end.