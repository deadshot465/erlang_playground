%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2022 19:23
%%%-------------------------------------------------------------------
-author("chehui.chou").
-record(todo_list, {auto_id = 1 :: integer(), entries = #{}}).