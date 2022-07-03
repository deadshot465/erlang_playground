%%%-------------------------------------------------------------------
%%% @author chehui.chou
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2022 15:22
%%%-------------------------------------------------------------------
-module(todo_list).
-author("chehui.chou").
-include("todo_list_record.hrl").

%% API
-export([new/0, add_entry/2, delete_entry/2, update_entry/2, update_entry/3, list_entries/1]).

-spec new() -> #todo_list{auto_id :: 1, entries :: #{}}.
new() -> #todo_list{}.

-spec add_entry(#todo_list{auto_id :: integer(), entries :: map()}, map()) -> #todo_list{auto_id :: integer(), entries :: map()}.
add_entry(ToDoList = #todo_list{auto_id = NextId, entries = Entries}, Entry) ->
  NewEntry = Entry#{id => NextId},
  NewEntries = maps:put(NextId, NewEntry, Entries),
  ToDoList#todo_list{auto_id = NextId + 1, entries = NewEntries}.

-spec delete_entry(#todo_list{auto_id :: integer(), entries :: map()}, _) -> #todo_list{auto_id :: integer(), entries :: map()}.
delete_entry(ToDoList = #todo_list{entries = Entries}, Key) ->
  NewEntries = maps:remove(Key, Entries),
  ToDoList#todo_list{entries = NewEntries}.

-spec update_entry(#todo_list{auto_id :: integer(), entries :: map()}, #{'id' := _, _ => _}) -> #todo_list{auto_id :: integer(), entries :: map()}.
update_entry(ToDoList, NewEntry = #{id := EntryId}) ->
  update_entry(ToDoList, EntryId, fun(_) -> NewEntry end).

-spec update_entry(#todo_list{auto_id :: integer(), entries :: map()}, _, fun((_) -> any())) -> #todo_list{auto_id :: integer(), entries :: map()}.
update_entry(ToDoList = #todo_list{entries = Entries}, EntryId, UpdaterFun) ->
  case maps:get(EntryId, Entries) of
    OldEntry ->
      #{id := OldEntryId} = OldEntry,
      NewEntry = #{id := OldEntryId} = UpdaterFun(OldEntry),
      NewEntries = maps:put(OldEntryId, NewEntry, Entries),
      ToDoList#todo_list{entries = NewEntries}
  end.

-spec list_entries(#todo_list{auto_id :: integer(), entries :: map()}) -> [any()].
list_entries(#todo_list{entries = Entries}) ->
  lists:map(fun({_, Entry}) -> Entry end, maps:to_list(Entries)).