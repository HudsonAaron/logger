%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%    日志系统业务模块
%%% @end
%%% Created : 15. 8月 2022 19:44
%%%-------------------------------------------------------------------
-module(api_logger_handler).
-author("zhuhaolin").
%% API
-export([
    infolog/3,
    infolog/4,
    errlog/3,
    errlog/4,
    crashlog/3,
    crashlog/4,
    
    do_handle_info/2
]).

-include("logger.hrl").

infolog(Mod, Line, Format) ->
    write_log(info_listener, Mod, Line, Format).
infolog(Mod, Line, Format, Data) ->
    write_log(info_listener, Mod, Line, Format, Data).
errlog(Mod, Line, Format) ->
    write_log(error_listener, Mod, Line, Format).
errlog(Mod, Line, Format, Data) ->
    write_log(error_listener, Mod, Line, Format, Data).
crashlog(Mod, Line, Format) ->
    write_log(crash_listener, Mod, Line, Format).
crashlog(Mod, Line, Format, Data) ->
    write_log(crash_listener, Mod, Line, Format, Data).

%% 写日志
write_log(TypeListener, Mod, Line, Format) ->
    write_log(TypeListener, Mod, Line, Format, []).
write_log(TypeListener, Mod, Line, Format, Data) ->
    TypeListener ! {write_log, Mod, Line, Format, Data}.

%% 获取文件名-前缀
get_filename(info_listener, Id) ->
    get_filename("infolog", Id);
get_filename(error_listener, Id) ->
    get_filename("errlog", Id);
get_filename(crash_listener, Id) ->
    get_filename("crashlog", Id);
%% 获取文件名
get_filename(Prefix, Id) ->
    {ok, Path} = api_sys:get_logs_path(),
    {{Y, M0, D0}, _} = erlang:localtime(),
    M = io_lib:format("~2..0w", [M0]),
    D = io_lib:format("~2..0w", [D0]),
    FileName = [Path, Prefix, "_", Y, "_", M, "_", D, "_", Id, ".log"],
    lists:concat(FileName).

%% 格式化日志
write_file(FIO, Format, Args) ->
    io:format("~n~w-~w-~w  ~w:~w:~w[~w:~w]~n" ++ Format, Args),
    io:format(FIO, "~n~w-~w-~w  ~w:~w:~w[~w:~w]~n" ++ Format, Args),
    ok.

%% 写日志回调
do_handle_info({write_log, Mod, Line, Format, Data},
    State = #mgr_log_state{type_listener = TypeListener, id = Id, filename = "", file_io = OldFIO}) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    FileName = get_filename(TypeListener, Id + 1),
    case filelib:is_file(FileName) of
        true -> %% 文件已存在
            case OldFIO of
                undefined -> skip;
                _ -> file:close(OldFIO)
            end,
            NewState = State#mgr_log_state{id = Id + 1, filename = FileName, file_io = undefined},
            do_handle_info({write_log, Mod, Line, Format, Data}, NewState);
        _ -> %% 文件不存在
            {ok, FIO} = file:open(FileName, [append, {encoding, utf8}]),
            Args = [Y, M, D, H, I, S, Mod, Line | Data],
            write_file(FIO, Format, Args),
            NewState = State#mgr_log_state{id = Id + 1, filename = FileName, file_io = FIO},
            {noreply, NewState}
    end;
do_handle_info({write_log, Mod, Line, Format, Data},
    State = #mgr_log_state{filename = FileName, file_io = OldFIO}) ->
    FileSize = filelib:file_size(FileName),
    {ok, MaxBytes} = api_sys:get_logs_mf_maxbytes(),
    case FileSize >= MaxBytes of
        true -> %% 超过文件size限制
            case OldFIO of
                undefined -> skip;
                _ -> file:close(OldFIO)
            end,
            NewState = State#mgr_log_state{filename = "", file_io = undefined},
            do_handle_info({write_log, Mod, Line, Format, Data}, NewState);
        _ -> %% 未超过
            {{Y, M, D}, {H, I, S}} = erlang:localtime(),
            case OldFIO of
                undefined -> {ok, FIO} = file:open(FileName, [append, {encoding, utf8}]);
                _ -> FIO = OldFIO
            end,
            Args = [Y, M, D, H, I, S, Mod, Line | Data],
            write_file(FIO, Format, Args),
            {noreply, State#mgr_log_state{file_io = FIO}}
    end.