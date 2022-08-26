%%

%%
-module(player).
-define(Undefined, undefined).

-record(eq, {key = {player_name, eq_name}, player_name = "", eq_name = "", attribute = []}).
-record(eq_pos, {key = {player_name, pos}, player_name = "", pos = 0, eq_name = "", attribute = []}).
-record(attribute, {player_name = "", attribute_point = [], attribute_value = []}).

%%%====================================================================
%%% API functions
%%%====================================================================
-export([do_msg/2, logout/1]).
-export([]).

do_msg(Msg, Name) ->
  case get_name() of
    ?Undefined -> set_name(Name);
	Name -> ok;
	_ ->
	  EraseFun = fun(Name) -> erase(Name) end,
      lists:foreach(EraseFun, get_all_dic_name()),
	  set_name(Name)
  end,
  do_msg_detail(Msg).
  
logout(Name) ->
  case get_name() of
    Name -> 
	  EraseFun = fun(Name) -> erase(Name) end,
      lists:foreach(EraseFun, get_all_dic_name());
	_ -> skip
  end.
  
%% on_load() -> ok.
  

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_all_dic_name() -> [player_name, player_eq_list, player_attribute, player_eq_pos_list].
%% 进程字典
get_name() -> get(player_name).
set_name(Name) -> put(player_name, Name).

%% 未装配的装备  [#eq]
get_eq_list() -> get(player_eq_list).
set_eq_list(List) -> put(player_eq_list, List).

%% 已装配的装备 [#eq_pos]
get_eq_pos_list() -> get(player_eq_pos_list).
set_eq_pos_list(EqPosList) -> put(player_eq_pos_list, EqPosList).

%% 属性及属性点 #attribute
get_attribute() -> get(player_attribute).
set_attribute(Attribute) -> put(player_attribute, Attribute).


do_msg_detail(Msg) ->
  Msg.
  
%% 穿装备

%% 脱装备

























