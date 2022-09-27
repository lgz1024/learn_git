%%%-------------------------------------------------------------------
%%% @author 罗光志
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 7月 2022 23:17
%%%-------------------------------------------------------------------
-module(my_common_fun).
-author("罗光志").

%% API
-compile(export_all).

-spec binary_find(term(), tuple()) -> non_neg_integer() | false.
binary_find(Value, Tuple) ->
  Length = tuple_size(Tuple),
  if element(1, Tuple) < Value andalso Value < element(Length, Tuple) ->
    binary_find_logic(Value, Tuple, 2, Length - 1);
    element(1, Tuple) == Value -> 1;
    element(Length, Tuple) == Value -> Length;
    true -> false
  end.
binary_find_logic(Value, Tuple, Left, Right) when Left < Right ->
  Mid = (Left + Right) div 2,
  if element(Mid, Tuple) == Value -> Mid;
    element(Mid, Tuple) > Value -> binary_find_logic(Value, Tuple, Left, Mid - 1);
    true -> binary_find_logic(Value, Tuple, Mid + 1, Right)
  end;
binary_find_logic(Value, Tuple, Pos, Pos) ->
  if element(Pos, Tuple) == Value -> Pos;
    true -> false
  end;
binary_find_logic(_Value, _Tuple, _Left, _Right) -> false.

-spec binary_find_value(term(), tuple()) -> term() | false.
%% KeyValueTuple :: {{Key, Value}}  return :: Value
binary_find_value(Key, KeyValueTuple) ->
  Length = tuple_size(KeyValueTuple),
  StartKey = get_key(1, KeyValueTuple),
  EndKey = get_key(Length, KeyValueTuple),
  if StartKey < Key andalso Key < EndKey ->
    binary_find_value_logic(Key, KeyValueTuple, 2, Length - 1);
    StartKey == Key -> get_value(1, KeyValueTuple);
    EndKey == Key -> get_value(Length, KeyValueTuple);
    true -> false
  end.
get_key(Pos, KeyValueTuple) -> element(1, element(Pos, KeyValueTuple)).
get_value(Pos, KeyValueTuple) -> element(2, element(Pos, KeyValueTuple)).
binary_find_value_logic(Key, KeyValueTuple, Left, Right) when Left < Right ->
  Mid = (Left + Right) div 2,
  MidKey = get_key(Mid, KeyValueTuple),
  if MidKey == Key -> get_value(Mid, KeyValueTuple);
    MidKey > Key -> binary_find_value_logic(Key, KeyValueTuple, Left, Mid - 1);
    true -> binary_find_value_logic(Key, KeyValueTuple, Mid + 1, Right)
  end;
binary_find_value_logic(Key, KeyValueTuple, Pos, Pos) ->
  PosKey = get_key(Pos, KeyValueTuple),
  if PosKey == Key -> get_value(Pos, KeyValueTuple);
    true -> false
  end;
binary_find_value_logic(_Key, _KeyValueTuple, _Left, _Right) -> false.

-spec exchange_sort(list()) -> list().
exchange_sort([H | List]) ->
  exchange_sort_logic(List, H, [], [], false);
exchange_sort([]) -> [].
exchange_sort_logic([H | List], WaitExchange, Retain, SmallReverseRet, _ExchangeFlag) when H > WaitExchange ->
  exchange_sort_logic(List, WaitExchange, [H | Retain], SmallReverseRet, true);
exchange_sort_logic([H | List], WaitExchange, Retain, SmallReverseRet, ExchangeFlag) ->
  exchange_sort_logic(List, H, [WaitExchange | Retain], SmallReverseRet, ExchangeFlag);
%% exchange_sort_logic([], WaitExchange, [], SortRet, true) -> not_exit
exchange_sort_logic([], WaitExchange, [RetainH | Retain], SortRet, true) ->
  exchange_sort_logic(Retain, RetainH, [], [WaitExchange | SortRet], false);
exchange_sort_logic([], WaitExchange, Retain, SortRet, false) ->
  sort_append([WaitExchange | SortRet], Retain).
%% 1, 9,8,7,6,5,4,3,2 -> 2 3,4,5,6,7,8,9 1 -> 9 8,7,6,5,4,3 2,1 ->

-spec select_sort(list()) -> list().
select_sort([H | List]) ->
  select_sort_logic(List, H, [], []);
select_sort([]) -> [].
select_sort_logic([H | List], Max, Retain, SortRet) when H > Max ->
  select_sort_logic(List, H, [Max | Retain], SortRet);
select_sort_logic([H | List], Max, Retain, SortRet)->
  select_sort_logic(List, Max, [H | Retain], SortRet);
select_sort_logic([], Max, [RetainH | Retain], SortRet) ->
  select_sort_logic(Retain, RetainH, [], [Max | SortRet]);
select_sort_logic([], Max, [], SortRet) ->
  [Max | SortRet].

-spec quickly_sort(list()) -> list().
quickly_sort([Point | List]) ->
  {SmallList, BigList, PointList} = quickly_sort_logic(List, [], [], [Point]),
  ReverseSortSmallList = quickly_sort_reverse(SmallList),
  SortBigList = quickly_sort(BigList),
  sort_append(ReverseSortSmallList, sort_append(PointList, SortBigList));
quickly_sort([]) -> [].
quickly_sort_logic([H | List], SmallList, BigList, [Point | _] = PointList) when H < Point ->
  quickly_sort_logic(List, [H | SmallList], BigList, PointList);
quickly_sort_logic([Point | List], SmallList, BigList, [Point | _] = PointList) ->
  quickly_sort_logic(List, SmallList, BigList, [Point | PointList]);
quickly_sort_logic([H | List], SmallList, BigList, PointList) ->
  quickly_sort_logic(List, SmallList, [H | BigList], PointList);
quickly_sort_logic([], SmallList, BigList, PointList) -> {SmallList, BigList, PointList}.

quickly_sort_reverse([Point | List]) ->
  {SmallList, BigList, PointList} = quickly_sort_logic(List, [], [], [Point]),
  ReverseSortSmallList = quickly_sort_reverse(SmallList),
  SortBigList = quickly_sort(BigList),
  sort_append(SortBigList, sort_append(PointList, ReverseSortSmallList));
quickly_sort_reverse([]) -> [].

%% sort_append([3,2,1], [4,5,6]) -> [1,2,3,4,5,6]
%% sort_append([4,5,6], [3,2,1]) -> [6,5,4,3,2,1]
sort_append([H | List], Ret) -> sort_append(List, [H | Ret]);
sort_append([], Ret) -> Ret.

%% 养成类玩法
%% 玩家首次上线 加载数据库 缓存到db_ets，记录加载数据库的时间(3天后过期，玩家再次上线需加载数据库)
%% on_load 从db_ets中加载
%% on_line
%% 穿戴|卸下等 功能是否开启 是否存在于背包中 是否存在配置错误 是否满足养成限制

%% 副本类玩法
%% 玩家进入地图，创建地图
%% 副本结束，清理地图
%% 副本结束条件：达到副本结束时间、玩家退出地图（主动|掉线）、达到副本结算条件
%% 涉及进程：玩家进程、地图进程

%% 多人地图类玩法  玩家能pk玩家   玩家之间不能pk
%% 根据pk模式（阵营模式 | 和平模式 | ...）和 玩家阵营 后端判断是否能对其他玩家造成伤害（技能命中）
%% 涉及进程：玩家进程、地图进程、玩法进程
%% 玩家进程：请求进入地图（离开地图） 给地图进程（玩法进程）发消息
%% 地图进程：玩家进入地图 | 玩家离开地图 | boss受到伤害 | boss死亡（结算） | 玩家死亡 | on_tick(定时发送玩家排名)
%% | 处理玩法进程发来的消息（如活动结束）| 其他地图进程发来的消息（如清除缓存的历史玩家伤害数据）
%% 玩法进程：on_tick(活动开始、结束)、赛季重置、创建地图进程（刷怪）

%% 联服玩法
%% 主服on_tick(同步活动开始、结束给从服)、主服赛季重置
%% 联服开启 创建地图
%% 联服关闭 主服结算、清理地图
%% 节点连接 主服同步活动数据给从服，从服同步本地玩家信息给主服
%% 节点断开连接

%% 商店 个人商店 全服商店 （永久商店 周期商店）
%% 个人商店 只涉及玩家进程
%% 个人商店数据库表记录商店id,玩家id，主键（玩家id，商店id），玩家已购买的商品列表（[{商品id,数量}]）
%% 玩家上线给客户端同步商店数据，玩家购买商品
%% 个人周期商店（每周重置等）固定时间重置（如每周一零点重置），玩家周一零点在线，则在线重置商店数据，玩家不在线则上线重置；
%% 相对时间重置（每隔一周重置）个人商店数据库表增加一个字段记录上次重置时间，玩家在线时，定时检测 now >= 重置间隔时间+上次重置时间；
%% 玩家重新上线，定时检测不影响；每次重置时，存入的上次重置时间 = 上上次重置时间 + n*重置时间间隔（n一般为1）

%% link 捕获退出信号 process_flag(process, Pid) monitor Down消息
%% 数据库连接池 管理连接 复用
%% 登录流程
my_quickly_sort([H | List]) ->
  {SmallList, BigList, PointList} = my_quickly_sort_logic(List, [], [], [H]),
  SortReverseSmallList = my_quickly_sort_reverse(SmallList),
  SortBigList = my_quickly_sort(BigList),
  my_sort_append(SortReverseSmallList, my_sort_append(PointList, SortBigList));
my_quickly_sort([]) -> [].
my_sort_append([H | List], RetList) -> my_sort_append(List, [H | RetList]);
my_sort_append([], RetList) -> RetList.

my_quickly_sort_reverse([H | List]) ->
  {SmallList, BigList, PointList} = my_quickly_sort_logic(List, [], [], [H]),
  SortReverseSmallList = my_quickly_sort_reverse(SmallList),
  SortBigList = my_quickly_sort(BigList),
  my_sort_append(SortBigList, my_sort_append(PointList, SortReverseSmallList));
my_quickly_sort_reverse([]) -> [].

my_quickly_sort_logic([H | List], SmallList, BigList, [Point | _] = PointList) when H < Point ->
  my_quickly_sort_logic(List, [H | SmallList], BigList, PointList);
my_quickly_sort_logic([Point | List], SmallList, BigList, [Point | _] = PointList) ->
  my_quickly_sort_logic(List, SmallList, BigList, [Point | PointList]);
my_quickly_sort_logic([H | List], SmallList, BigList, PointList) -> %% H > Point
  my_quickly_sort_logic(List, SmallList, [H | BigList], PointList);
my_quickly_sort_logic([], SmallList, BigList, PointList) -> {SmallList, BigList, PointList}.

fun_test([H | List]) -> fun_test([E || E <- List, E < H]) ++ [H] ++ fun_test([E || E <- List, E >= H]);
fun_test([]) -> [].
%% 玩家 位移同步 使用技能同步 广播周围
%% 技能命中算伤害时  检测是否敌对阵营（是否PK地图）

%% 副本刷怪（副本初始化时，玩家进入地图，250ms定时检测中） 怪物移动 广播周围

%% 玩家怪物长距离移动，每250ms，移动路点列表中的一个点，判断移动速度是否足够，成功到达这个点并广播周围

%% 动态规划
%% 正整数 a、b、c,  a<b<c<n, 正整数k1、k2、k3 使k1*a + k2*b + k3*c = Max <= n, 求最大的Max
%% 初始剩余K3 = n div c
%% K3 * c + Max(a,b, n - K3*c) (K3 n div 3 -> 0)(求Max)
%% K2 = (n - K3 *c) div b, K2 * b + Max(a, n - K3*c - K2*b)
%% a - (n -K3*c - K2*b) rem a

%% (n - n div c * c) div b + (n - (n div c - 1) * c) div b + (n - c) div b + n div b
%% (c/b + 1) + (2c/b + 1) + (n div c * c/b + 1) + ((n/c + 1)*c/b + 1) (log_c_n + 1)
%% log_c_n + 1 +   c/b*(1 + n/c + 1)/2 * (log_c_n + 1) n_log_n
%% 1  n/b  n/b * log_c_n

%% Max(a,b, n) n div b + 1
%% 1 + 2 + n/b + 1

%% N > 0
dp_max(A, B, C, N) when A > B andalso A > C ->
  N_A = case N / A of
          Integer when is_integer(Integer) -> Integer;%% return
          _ -> N div A + 1
        end,
  ResultList = [dp_max(B, C, N - A * Num) || Num <- lists:seq(1, N_A), N - A * Num > 0],
  lists:max(ResultList); %% []
dp_max(A, B, C, N) when B > A andalso B > C ->
  dp_max(B, A, C, N);
dp_max(A, B, C, N) when C > A andalso C > B ->
  dp_max(C, B, A, N);
dp_max(A, A, C, N) when A > C -> dp_max(A, C, N);
dp_max(A, B, A, N) when A > B -> dp_max(A, B, N);
dp_max(A, B, B, N) when B > A -> dp_max(B, A, N);
dp_max(A, _, _, N) -> N - N rem A.


%% N > 0
dp_max(A, B, N) when A > B ->
  N_A = case N / A of
          Integer when is_integer(Integer) -> Integer;%% return
          _ -> N div A + 1
        end,
  ResultList = [dp_max(B, N - A * Num) || Num <- lists:seq(1, N_A), N - A * Num > 0],
  lists:max(ResultList);%% []
dp_max(A, B, N) when A < B ->
  dp_max(B, A, N);
dp_max(A, _, N) -> N - N rem A.

dp_max(A, N) -> N - N rem A.







