%%

%%
-module(sun_client).
-define(Server, sun_gen_server).
-define(Client, ?MODULE).
-define(Undefined, undefined).

-define(LocalFile, local_file).
-define(EtsLocalFile, ets_local_file).

-record(eq, {key = {player_name, eq_name}, player_name = "", eq_name = "", attribute = []}).
-record(eq_pos, {key = {player_name, pos}, player_name = "", pos = 0, eq_name = "", attribute = []}).
-record(attribute, {player_name = "", attribute_point = [], attribute_value = []}).
-record(map, {key = {player_name, map_cfg_id}, player_name = "", map_cfg_id = 0, pos_x = 0, pos_y = 0, blood = 0}).

-record(player_dict, {walk_map_id = 0, pos = {0, 0}}).

-define(MapX_Min, 1).
-define(MapX_Max, 9).
-define(MapY_Min, 1).
-define(MapY_Max, 9).
-define(Y, 20320,32,32). %% 你

%%%====================================================================
%%% API functions
%%%====================================================================
-export([start/0, get_account/0, logout/0, save/1, log_out/1]).
-export([do_local_save_start/1, get_client_ets_name/1]).

start() -> start_detail().

get_account() -> get_name().
logout() -> erase(sun_client_name).

%% 保存进度
save(NameMsg) -> save_detail(NameMsg).

%% 供server调用，存储本地文件
log_out(Name) -> log_out_detail(Name).

get_client_ets_name(Name) ->
  list_to_atom(Name ++ atom_to_list(?EtsLocalFile)).

%% 不重要，还没有实现
%%change_name(Name) -> do.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_detail() ->
 case whereis(?Server) of
   ?Undefined -> 
     sun_gen_server:start(),
	 start_success;
   _ -> already_start
 end,
 StartStr = 
 case get_name() of
   ?Undefined ->
     List = io:get_line("系统: 请输入你的名字: "),
     Name = lists:droplast(List),
     Str = do_local_save_start(Name),
     sun_gen_server:send_2_me({?Client, login, Name}),
     set_name(Name),
     Str;
   Name -> 
     Str = "系统: 你好, " ++ Name ++ "! 你想做什么？~n",
     str_format(Str)
 end,
 StartStr1 = "使用历史角色进入游戏（1）  新建角色（2）~n",
 str_format_not_end(StartStr),
 str_format_not_end(StartStr1),
 NextList = sun_util:get_player_operation(),
 NextRequest = lists:droplast(NextList),
 handle_request_start(NextRequest).
 
handle_request_start("1") ->
  Name = get_name(),
  Reply = sun_gen_server:call_me({?Client, Name, enter_game_use_old}),
  %% 是否存在历史角色
  case Reply of
    {enter_game_use_old_success, Attribute, EqList, EqPosList} ->
      set_attribute(Attribute),
      set_eq_list(EqList),
      set_eq_pos_list(EqPosList),
      init_player_dict(#player_dict{walk_map_id = 1, pos = {1, 1}}),
      sun_util:print_main_panel(Name),
      handle_request();
	_ ->
	   StartStr1 = "没有历史角色  请新建角色（2）~n请输入2~n",
       sun_util:io_format_prefix_not_end(StartStr1),
       NextList = sun_util:get_player_operation(),
       NextRequest = lists:droplast(NextList),
       handle_request_start(NextRequest)
  end;
handle_request_start("2") ->
  Name = get_name(),
  case sun_gen_server:get_player_attribute(Name) of
    [] -> handle_request_start(2);
	[Attr] ->
	  EqList = sun_gen_server:get_player_eq_list(Name),
	  EqPosList = sun_gen_server:get_player_eq_pos_list(Name),
	  sun_util:io_format_prefix_not_end("已存在历史角色，输入任意键，进入游戏~n"),
	  sun_util:get_player_operation(),
	  set_attribute(Attr),
    set_eq_list(EqList),
	  set_eq_pos_list(EqPosList),
    init_player_dict(#player_dict{walk_map_id = 1, pos = {1, 1}}),
    sun_util:print_main_panel(Name),
    handle_request()
  end;
handle_request_start(2) ->
  %% 生成属性点，随机，只显示属性点
  Name = get_name(),
  Reply = sun_gen_server:call_me({?Client, Name, enter_game_use_new}),
  %% AttrList = [1,2,3,4,5,6,7],
  %% PointList = [1,2,3,4,5,6],
  case Reply of
    {enter_game_use_new_success, Attribute} ->
      sun_util:io_format_prefix_not_end("已随机生成属性点~n"),
	  PointList = Attribute#attribute.attribute_point,
	  print_player_attribute_point(PointList),
	  OldRemainTimes = case get(sun_client_retain_create_role_times) of ?Undefined -> 3; V -> V end,
	  NewRemainTimes = OldRemainTimes - 1,
	  put(sun_client_retain_create_role_times, NewRemainTimes),
      sun_util:io_format_prefix_not_end("进入游戏(1)   重新生成属性点,还剩" ++ integer_to_list(NewRemainTimes) ++ "次机会(2)~n"),
      NextList = sun_util:get_player_operation(),
	  NextRequest = lists:droplast(NextList),
	  case NextRequest of
	    "1" ->
        erase(sun_client_retain_create_role_times),
        set_attribute(Attribute),
        set_eq_list([]),
        set_eq_pos_list([]),
        init_player_dict(#player_dict{walk_map_id = 1, pos = {1, 1}}),
        sun_util:print_main_panel(Name),
        handle_request();
		"2" -> handle_request_start(2);
		_ -> handle_request_start(2)
	  end;
	{enter_game_use_new_max_times, Attribute} ->
	  sun_util:io_format_prefix_not_end("生成属性点次数已用尽！~n"),
	  PointList = Attribute#attribute.attribute_point,
	  print_player_attribute_point(PointList),
    sun_util:io_format_prefix_not_end("输入任意键，进入游戏~n"),
	  sun_util:get_player_operation(),
	  erase(sun_client_retain_create_role_times),
    set_attribute(Attribute),
    set_eq_list([]),
	  set_eq_pos_list([]),
    init_player_dict(#player_dict{walk_map_id = 1, pos = {1, 1}}),
    sun_util:print_main_panel(Name),
	  handle_request();
	_ -> 
	  erase(sun_client_retain_create_role_times),
	  start_detail()
  end;
handle_request_start(_) ->
  StartStr1 = "使用历史角色进入游戏（1）  新建角色（2）~n请输入1或2~n",
  sun_util:io_format_prefix_not_end(StartStr1),
  NextList = sun_util:get_player_operation(),
  NextRequest = lists:droplast(NextList),
  handle_request_start(NextRequest).


handle_request() ->
  NextList = sun_util:get_player_operation(),
  NextRequest = lists:droplast(NextList),
  handle_panel_request(NextRequest).

 

get_all_dic_name() -> [sun_client_name, sun_client_eq_list, sun_client_attribute,
  sun_client_eq_pos_list, sun_client_walk_map, sun_client_walk_pos].
%% name存在进程字典  字符串
get_name() -> get(sun_client_name).
set_name(Name) -> put(sun_client_name, Name).

%% 当前界面
get_panel() -> get(sun_client_panel).
set_panel(Panel) -> put(sun_client_panel, Panel).

%% 未装配的装备  [#eq]
get_eq_list() -> get(sun_client_eq_list).
set_eq_list(List) -> put(sun_client_eq_list, List).

%% 已装配的装备 [#eq_pos]
get_eq_pos_list() -> get(sun_client_eq_pos_list).
set_eq_pos_list(EqPosList) -> put(sun_client_eq_pos_list, EqPosList).

%% 属性及属性点 #attribute
get_attribute() -> get(sun_client_attribute).
set_attribute(Attribute) -> put(sun_client_attribute, Attribute).

%%
get_walk_map() -> get(sun_client_walk_map).
set_walk_map(MapId) -> put(sun_client_walk_map, MapId).

%%
get_walk_pos() -> get(sun_client_walk_pos).
set_walk_pos(Pos) -> put(sun_client_walk_pos, Pos).

%% 当前进度 ets表中查
%%get_process() -> 0. %%get(sun_client_process).
%%put_process(Process) -> ok. %%put(sun_client_process, Process).

%% 初始化进程字典
init_player_dict(#player_dict{walk_map_id = Id, pos = Pos}) ->
  set_walk_map(Id),
  set_walk_pos(Pos),
  ok.

print_player_attribute_point(PointList) ->
  Name = "属性点",
  NameStr = "~n===========" ++ Name ++ "======================~n~n",
  io:format(NameStr,[]),
  io:format("体质:~p  力量:~p  魔力:~p  耐力:~p  敏捷:~p  幸运:~p~n", PointList),
  NameStr1 = "~n===========" ++ Name ++ "======================~n~n",
  io:format(NameStr1,[]).

%% 还有后续，避免输出ok
str_format_not_end(Str) -> sun_util:io_format_not_end(Str).
str_format(Str) -> Str.
%%str_format_prefix(Str) -> sun_util:str_format_prefix(Str).

%% 根据当前所在界面，确定玩家请求
handle_panel_request(Request) ->
  Panel = get_panel(),
  case Panel of
    ?Undefined -> handle_panel_request_main_panel(Request);
	attribute_panel -> handle_panel_request_attribute_panel(Request);
	bag_panel -> handle_panel_request_bag_panel(Request);
	map_panel -> handle_panel_request_map_panel(Request);
	_ -> handle_panel_request_main_panel(Request)
  end.

%% 主界面
handle_panel_request_main_panel("1") ->
  %% 玩家属性
  case get_attribute() of
    #attribute{attribute_point = PointList, attribute_value = Attr} ->
	  sun_util:print_player_attribute(Attr, PointList);
	_ -> 
	  {Attr, PointList} = sun_util:get_default_attribute(),
	  sun_util:print_player_attribute(Attr, PointList)
  end,
  set_panel(attribute_panel),
  Request = handle_undefine_request(),
  handle_panel_request_attribute_panel(Request);
handle_panel_request_main_panel("2") ->
  %% 地图
  MapData = cfg_map:get_main_map(),
  sun_util:print_map_panel(MapData),
  set_panel(map_panel),
  Request = handle_undefine_request(),
  handle_panel_request_map_panel(Request);
handle_panel_request_main_panel("3") ->
  %% 背包
  Fun1 = fun(#eq_pos{pos = Pos, eq_name = EqName}, Ret) ->
           sun_util:list_store(Pos, EqName ++ sun_util:format_button_num(Pos), Ret)
         end,
  EqPosList = lists:foldl(Fun1, lists:duplicate(6, "空"), get_eq_pos_list()), %% 
  Fun2 = fun(#eq{eq_name = Name}, {Pos, Ret}) ->  {Pos + 1, Name ++ sun_util:format_button_num(Pos) ++ Ret} end,
  {_, EqList} = lists:foldl(Fun2, {7, "~n"}, get_eq_list()), %% 
  BagData = {EqPosList, EqList},
  sun_util:print_bag_panel(BagData),
  set_panel(bag_panel),
  Request = handle_undefine_request(),
  handle_panel_request_bag_panel(Request);
handle_panel_request_main_panel("4") ->
  %% 走动
  Map = calc_print_map(),
  sun_util:print_walk_panel(Map, true),
  set_panel(walk_panel),
  Request = handle_undefine_request(),
  handle_panel_request_walk_panel(Request);
handle_panel_request_main_panel("save") ->
%%  save_detail(),
  MyName = get_name(),
  sun_gen_server:send_2_me({?Client, save, MyName}),
  Request = handle_undefine_request(),
  handle_panel_request_main_panel(Request);
handle_panel_request_main_panel("logout") ->
  MyName = get_name(),
  sun_gen_server:send_2_me({?Client, logout, MyName}),
  EraseFun = fun(Name) -> erase(Name) end,
  lists:foreach(EraseFun, get_all_dic_name()),
  io:format("系统: 退出成功！", []);
handle_panel_request_main_panel(_) ->
  Request = handle_undefine_request(),
  handle_panel_request_main_panel(Request).
  
%% 属性界面
handle_panel_request_attribute_panel("r") ->
  return_main_panel();
handle_panel_request_attribute_panel(_) ->
  Request = handle_undefine_request(),
  handle_panel_request_attribute_panel(Request).

%% 背包界面
handle_panel_request_bag_panel("r") ->
  return_main_panel();
handle_panel_request_bag_panel(_) ->
  Request = handle_undefine_request(),
  handle_panel_request_bag_panel(Request).
  
%% 地图界面
handle_panel_request_map_panel("r") ->
  return_main_panel();
handle_panel_request_map_panel(_) ->
  Request = handle_undefine_request(),
  handle_panel_request_map_panel(Request).

%% 走动界面
handle_panel_request_walk_panel("1") ->
  return_main_panel();
handle_panel_request_walk_panel("w") -> %% 上
  on_walk(w),
  Request = handle_undefine_request(),
  handle_panel_request_walk_panel(Request);
handle_panel_request_walk_panel("s") -> %% 下
  on_walk(s),
  Request = handle_undefine_request(),
  handle_panel_request_walk_panel(Request);
handle_panel_request_walk_panel("a") -> %% 往左走
  on_walk(a),
  Request = handle_undefine_request(),
  handle_panel_request_walk_panel(Request);
handle_panel_request_walk_panel("d") -> %% 往右走
  on_walk(d),
  Request = handle_undefine_request(),
  handle_panel_request_walk_panel(Request);
handle_panel_request_walk_panel(_) ->
  Request = handle_undefine_request(),
  handle_panel_request_walk_panel(Request).

%% 等待玩家输入操作
handle_undefine_request() ->
  List = sun_util:get_player_operation(),
  _Request = lists:droplast(List).
  
%% 返回主界面
return_main_panel() ->
  Name = get_name(),
  sun_util:print_main_panel(Name),
  set_panel(?Undefined),
  Request = handle_undefine_request(),
  handle_panel_request_main_panel(Request).
  
%%handle_request(Str) ->
%%  case get_name() of
%%   ?Undefined -> start_detail();
%%   Name ->
%%     Reply = sun_gen_server:call_me({?Client, Name, Str}),
%%	 str_format_not_end(str_format_prefix(Reply ++ "~n")),
%%	 NextList = sun_util:get_player_operation(),
%%	 NextRequest = lists:droplast(NextList),
%%	 handle_request(NextRequest)
%%  end.

do_local_save_start(MyName) ->
%%  try
%%      case dets:open_file(?LocalFile) of
%%        {ok, Table} -> Table;
%%        _ ->
%%          dets:open_file(?LocalFile, [])
%%      end
%%  catch
%%      _:_  -> dets:open_file(?LocalFile, [])
%%  end.
  MyEtsLocalFile = list_to_atom(MyName ++ atom_to_list(?EtsLocalFile)),
  case ets:info(MyEtsLocalFile) of
    ?Undefined ->
      ets:new(MyEtsLocalFile, [named_table, public]),
      LocalFile = atom_to_list(?LocalFile),
      MyLocalFile = list_to_atom(MyName ++ LocalFile),
      dets:open_file(MyLocalFile, []),
      dets:to_ets(MyLocalFile, MyEtsLocalFile),
      dets:close(MyLocalFile),
      case ets:first(MyEtsLocalFile) of
        '$end_of_table' ->
          %% 玩法说明
          PlayIntroduce = "例如：~n使用历史角色进入游戏（1）  新建角色（2）~n输入1，即进入游戏；输入2，即新建角色~n",
          sun_util:print_panel("游戏说明", PlayIntroduce),
          io:get_line("输入任意键，进入下一步");
        _ ->
          sun_gen_server:call_me({?Client, MyName, do_local_save_start}),
          skip
      end;
    _ -> already_exist
  end,
  Str = "系统: 你好, " ++ MyName ++ "! 你想做什么？~n",
  str_format(Str).


save_detail({MyName, Msg}) ->
  MyEtsLocalFile = list_to_atom(MyName ++ atom_to_list(?EtsLocalFile)),
  ets:insert(MyEtsLocalFile, Msg),
  sun_util:io_format_prefix_not_end("保存成功~n").

log_out_detail(MyName) ->
  MyEtsLocalFile = list_to_atom(MyName ++ atom_to_list(?EtsLocalFile)),
  case ets:info(MyEtsLocalFile) of
    ?Undefined -> skip;
    _ ->
      LocalFile = atom_to_list(?LocalFile),
      MyLocalFile = list_to_atom(MyName ++ LocalFile),
      dets:open_file(MyLocalFile, []),
      dets:from_ets(MyLocalFile, MyEtsLocalFile),
      dets:close(MyLocalFile)
  end.

%%%===================================================================
%%% 函数
%%%===================================================================
%% YourName需要注册
%%send(Msg, YourName) -> sun_gen_server:send_2_me({?Client, YourName, Msg}).

on_walk(Direction) ->
  case calc_walk_pos(Direction) of
    {_NewX, _NewY} = NewPos ->
      set_walk_pos(NewPos),
      Map = calc_print_map(),
      sun_util:print_walk_panel(Map, true);
    _ -> skip
  end.

%% 更新进程字典walk_pos + walk_map_id不在此处
%% 正常直角坐标系
calc_walk_pos(w) -> %% 上
  {PosX, PosY} = get_walk_pos(),
  case PosY < ?MapY_Max of
    true ->
      {PosX, PosY + 1};
    _ -> null
  end;
calc_walk_pos(s) -> %% 下
  {PosX, PosY} = get_walk_pos(),
  case PosY > ?MapY_Min of
    true ->
      {PosX, PosY - 1};
    _ -> null
  end;
calc_walk_pos(a) -> %% 左
  {PosX, PosY} = get_walk_pos(),
  case PosX > ?MapX_Min of
    true ->
      {PosX - 1, PosY};
    _ -> null
  end;
calc_walk_pos(d) -> %% 右
  {PosX, PosY} = get_walk_pos(),
  case PosX < ?MapX_Max of
    true ->
      {PosX + 1, PosY};
    _ -> null
  end;
calc_walk_pos(_Pos) ->
  null.

%%
calc_print_map() ->
  MapId = get_walk_map(),
  {PosX, PosY} = Pos = get_walk_pos(),
  MapLength = cfg_map:get_move_map_length(),
  RealMapId = case MapId rem MapLength of
                0 -> MapLength;
                V -> V
              end,
  {Name, Map, EntrancePos, ExitPos} = cfg_map:get_move_map(RealMapId),
  {NewName, NewMap, NewPosX, NewPosY} = case Pos of
                         EntrancePos when RealMapId =/= 1 -> %% 达到入口
                           NewMapId = MapId - 1,
                           set_walk_map(NewMapId),
                           NewRealMapId = case NewMapId rem MapLength of
                                            0 -> MapLength;
                                            V1 -> V1
                                          end,
                           {NewName1, NewMap1, NewEntrancePos, _} = cfg_map:get_move_map(NewRealMapId),
                           set_walk_pos(NewEntrancePos),
                           {X, Y} = NewEntrancePos,
                           {NewName1, NewMap1, X, Y};
                         ExitPos -> %% 达到出口
                           NewMapId = MapId + 1,
                           set_walk_map(NewMapId),
                           NewRealMapId = case NewMapId rem MapLength of
                                            0 -> MapLength;
                                            V2 -> V2
                                          end,
                           {NewName1, NewMap1, NewEntrancePos, _} = cfg_map:get_move_map(NewRealMapId),
                           set_walk_pos(NewEntrancePos),
                           {X, Y} = NewEntrancePos,
                           {NewName1, NewMap1, X, Y};
                         _ ->
                           {Name, Map, PosX, PosY}
                       end,
  %% Map前缀有4个元素 ~n~n
  %% 一行 9*3+4=31个元素
  Nth = 4 + (?MapY_Max - NewPosY) * 31+ (NewPosX - 1) * 3,
  NewName ++ walk_pos_add_map(NewMap, Nth, []).

walk_pos_add_map([H1| Map], Nth, NeedReverse) when Nth > 0 ->
  walk_pos_add_map(Map, Nth - 1, [H1 | NeedReverse]);
walk_pos_add_map([_H1, _H2, _H3| Map], _Nth, NeedReverse) ->
  my_common_fun:sort_append(NeedReverse, [?Y | Map]);
walk_pos_add_map( _, _Nth, _NeedReverse) -> "". %% bug

