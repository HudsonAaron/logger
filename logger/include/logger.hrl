%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%    日志头文件
%%% @end
%%% Created : 16. 8月 2022 9:55
%%%-------------------------------------------------------------------
-author("zhuhaolin").

%% 日志
-record(mgr_log_state, {
    type_listener = undefined,             %% 日志类型
    id = 0,                                %% id
    filename = "",                         %% 当前正在写的文件名
    file_io = undefined                    %% 写文件io
}).