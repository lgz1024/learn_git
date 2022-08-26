%%

%%
-module(sun_util).
-compile(export_all).

%%%====================================================================
%%% API functions
%%%====================================================================

%%my_cd() -> cd("C:/D盘/my_sun").
  %%c(sun_client),
  %%c(sun_gen_server).
  

%% 加前缀，还有后续，避免输出ok
io_format_prefix_not_end(Str) -> io_format_not_end(str_format_prefix(Str)).

%% 还有后续，避免输出ok
io_format_not_end(Str) -> 
  %% 40个 - 
  LineStr = "----------------------------------------~n",
  io:format(LineStr, []),
  io:format(Str, []),
  LineStr1 = "----------------------------------------~n",
  io:format(LineStr1, []).

%% 避免输出ok
io_format_end(Str) -> io:format(Str ++ "  --", []), sun_server.

%% 只是加了前缀，没有打印输出
str_format_prefix(Str) -> "系统: " ++ Str.

now() -> os:system_time(second).

log_error(Class, Error) -> 
io:format("Class:~p~nExceptionReason:~p~nStack:~p~n", [Class, Error,
erlang:get_stacktrace()]).

%% 9x9打印
print9x9() ->
  print9x9(9).
print9x9(0) -> ok;
print9x9(N) ->
  io:format("1 2 3 4 5 6 7 8 9~n", []),
  print9x9(N - 1).


%% 主面板打印
print_main_panel(Name) -> %% 4个空格
  Str = "名称："++ Name ++ "~n~n属性(1)    地图(2)~n~n背包(3)    走动(4)~n~n退出游戏(logout)    保存进度(save)~n",
  print_panel("主界面", Str).

%% 属性面板打印 AttrList:长度7  PointList:长度6 
print_player_attribute(AttrList, PointList) -> %% 两个空格
  Str = "~n===========玩家属性======================~n~n",
  io:format(Str,[]),
  io:format("血量:~p  物理攻击力:~p  法术攻击力:~p  魔法值:~p  速度:~p~n物理防御力:~p  法术防御力:~p~n", AttrList),
  io:format("--------------------------------------------~n", []),
  io:format("体质:~p  力量:~p  魔力:~p  耐力:~p  敏捷:~p  幸运:~p~n", PointList),
  io:format("--------------------------------------------~n", []),
  io:format("~n返回主界面(r)~n",[]),
  io:format(Str,[]).
  
  
%% 背包面板打印
print_bag_panel({EqPosList, ItemNameListStr}) ->
  Button = "返回主界面(r)~n~n",
  MergeFun = fun(Str, {Pos, Ret}) -> {Pos + 1, Ret ++ get_eq_pos_name(Pos) ++ Str} end,
  {_, EqPosStr} = lists:foldl(MergeFun, {1, []}, EqPosList),
  ItemNameStr = "~n--------------------------------------------~n~n"
    ++ ItemNameListStr,
  NewStr = Button ++ EqPosStr ++ ItemNameStr,
  print_panel("背包", NewStr).
  
%% 地图面板打印
print_map_panel(Str) ->
  Button = "返回主界面(r)~n~n",
  NewStr = Button ++ Str,
  print_panel("地图", NewStr).
  
%% 走动面板打印
print_walk_panel(Map, Flag) -> %% 四个空格
  Button1 = case Flag of true -> "返回主界面(1)~n~n"; _ -> "" end,
  Button2 = "~n~n上(w)    下(s)    左(a)    右(d)~n",
  NewStr = Button1 ++ Map ++ Button2,
  print_panel("走动", NewStr).
  
print_panel(Name, Str) ->
  NameStr = "~n===========" ++ Name ++ "======================~n~n",
  io:format(NameStr,[]),
  io:format(Str, []),
  NameStr1 = "~n===========" ++ Name ++ "======================~n~n",
  io:format(NameStr1,[]).


%% 获取玩家操作
get_player_operation() -> %% 45个空格
  io:get_line("                                             ").

%% ================================装备start===========================================

%% ================================装备end===========================================


%% ================================属性start===========================================

%% 固定顺序 1-体质 2-力量 3-魔力 4-耐力 5-敏捷 6-幸运
get_random_point_list() ->
  NumMax = 10,
  XingYunMax = 2,
  TiZhi = rand:uniform(NumMax), %% 1-N
  LiLiang = rand:uniform(NumMax),
  MoLi = rand:uniform(NumMax),
  NaiLi = rand:uniform(NumMax),
  %% 敏捷上限
  MinJieMax = 50 - TiZhi - LiLiang - MoLi - NaiLi,
  MinJie = rand:uniform(MinJieMax),
  %% 幸运
  FixXingYun = ceil(3.5 - (TiZhi + LiLiang + MoLi + NaiLi + MinJie)/10),
  XingYun = rand:uniform(XingYunMax) - 1 + FixXingYun,
  [TiZhi, LiLiang, MoLi, NaiLi, MinJie, XingYun].

%% 固定顺序 1-血量  2-物理攻击力  3-法术攻击力  4-魔法值  5-速度 6-物理防御力 7-法术防御力
%% 仅属性点
%% 固定顺序 1-体质 2-力量 3-魔力 4-耐力 5-敏捷 6-幸运
calc_attribute([P1, P2, P3, P4, P5, P6]) -> 
  A1 = P1 * 5,
  A2 = P2,
  A3 = P3,
  A4 = P3 * 5,
  A5 = ceil(P1/10 + P2/5 + P5),
  A6 = P4,
  A7 = ceil(P3/5 + P4/5),
  [A1, A2, A3, A4, A5, A6, A7].
%% 装备 + 属性点等
calc_attribute() -> [].

get_default_attribute() ->
  {lists:seq(1,7), lists:seq(1,6)}.
  
%% ================================属性end===========================================


%% ================================NPC start===========================================

%% ================================NPC end===========================================


%% ================================地图 start===========================================

%% 地图 城市 国家 宇宙
%% 地图 npc住所（需一个个探索得知）传送阵 地图、玩家视野（等级）

%% 出生点 新手村中心 {Type, CfgId} Type - npc  物体 怪物
%% 固定地图  随机地图

%% ================================地图 end===========================================

%% ================================玩家移动 start===========================================

%% ================================玩家移动 end===========================================


list_store(1, V, [H | List]) -> [V | List];
list_store(N, V, [H | List]) -> [H | list_store(N - 1, V, List)];
list_store(_, V, []) -> [].

format_button_num(1) -> "(1)";
format_button_num(2) -> "(2)";
format_button_num(3) -> "(3)";
format_button_num(4) -> "(4)";
format_button_num(5) -> "(5)";
format_button_num(6) -> "(6)";
format_button_num(7) -> "(7)";
format_button_num(8) -> "(8)";
format_button_num(9) -> "(9)";
format_button_num(10) -> "(10)";
format_button_num(11) -> "(11)";
format_button_num(12) -> "(12)";
format_button_num(13) -> "(13)";
format_button_num(14) -> "(14)";
format_button_num(15) -> "(15)";
format_button_num(16) -> "(16)";
format_button_num(17) -> "(17)";
format_button_num(18) -> "(18)";
format_button_num(19) -> "(19)";
format_button_num(20) -> "(20)".

get_eq_pos_name(1) -> "武器：";
get_eq_pos_name(2) -> "    帽子："; %% 4个空格
get_eq_pos_name(3) -> "~n~n衣甲：";
get_eq_pos_name(4) -> "    饰品：";
get_eq_pos_name(5) -> "~n~n腰带：";
get_eq_pos_name(6) -> "    鞋子：".







