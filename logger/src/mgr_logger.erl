%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%    日志系统管理进程
%%% @end
%%% Created : 15. 8月 2022 19:17
%%%-------------------------------------------------------------------
-module(mgr_logger).
-author("zhuhaolin").

-behaviour(gen_server).
%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Type::atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(TypeListener) ->
    gen_server:start_link({local, TypeListener}, ?MODULE, [TypeListener], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mgr_log_state{}} | {ok, State :: #mgr_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([TypeListener]) ->
    process_flag(trap_exit, true),
    {ok, #mgr_log_state{
        type_listener = TypeListener
    }}.
%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mgr_log_state{}) ->
    {reply, Reply :: term(), NewState :: #mgr_log_state{}} |
    {reply, Reply :: term(), NewState :: #mgr_log_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mgr_log_state{}} |
    {noreply, NewState :: #mgr_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mgr_log_state{}} |
    {stop, Reason :: term(), NewState :: #mgr_log_state{}}).
handle_call(_Request, _From, State = #mgr_log_state{}) ->
    {reply, ok, State}.
%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mgr_log_state{}) ->
    {noreply, NewState :: #mgr_log_state{}} |
    {noreply, NewState :: #mgr_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mgr_log_state{}}).
handle_cast(_Request, State = #mgr_log_state{}) ->
    {noreply, State}.
%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mgr_log_state{}) ->
    {noreply, NewState :: #mgr_log_state{}} |
    {noreply, NewState :: #mgr_log_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mgr_log_state{}}).
handle_info(Info, State = #mgr_log_state{}) ->
    {noreply, NewState} = api_logger_handler:do_handle_info(Info, State),
    {noreply, NewState}.
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mgr_log_state{}) -> term()).
terminate(_Reason, _State = #mgr_log_state{}) ->
%%    api_logger_handler:crashlog(?MODULE, ?LINE, "日志系统终止，原因", [_Reason]),
    ok.
%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mgr_log_state{},
    Extra :: term()) ->
    {ok, NewState :: #mgr_log_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mgr_log_state{}, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
