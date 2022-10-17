%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%    日志系统监察树
%%% @end
%%% Created : 15. 8月 2022 19:17
%%%-------------------------------------------------------------------
-module(mgr_logger_sup).
-author("zhuhaolin").

-behaviour(supervisor).
%% API
-export([
    start/2
]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc Starts the supervisor
-spec(start(_Type::term(), _Args::term()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start(_Type, _Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([]) ->
    MaxRestarts = 5,
    MaxSeconds = 60,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSeconds
    },
    ModuleName = mgr_logger,
    Fun =
        fun(TypeListener, Acc) ->
            Child = #{
                id => TypeListener,
                start => {ModuleName, start_link, [TypeListener]},
                restart => permanent,
                shutdown => 2000,
                type => worker,
                modules => [ModuleName]
            },
            [Child | Acc]
        end,
    Children = lists:foldl(Fun, [], [info_listener, error_listener, crash_listener]) ,
    {ok, {SupFlags, Children}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
