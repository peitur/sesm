-module( sesm_monitor_sup ).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link/1, start_child/3]).


start_link() ->
	start_link( [] ).

start_link( Options ) ->
	supervisor:start_link( {local, ?MODULE}, ?MODULE, Options ).

start_child( Pid, Conf, Options ) ->
	supervisor:start_child( ?MODULE, [ Pid, Conf, Options] ).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    AChild = {'sesm_monitor',{'sesm_monitor',start_link,[]},
	      permanent,2000,worker,['sesm_monitor']},
    {ok,{{simple_one_for_one,0,1}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

