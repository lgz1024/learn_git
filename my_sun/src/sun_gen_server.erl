%% 

%%
-module(sun_gen_server).
-behaviour(gen_server).

-include("common_atom.hrl").
-include_lib("stdlib/include/ms_transform.hrl"). %% 应对etsfun报错

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-define(Client, sun_client).
-define(UpdateTime, 1000). %% 1秒更新行为
-define(ClientEts, ets_sun_gen_server_client).
-define(EqEts, ets_eq).
-define(EqPosEts, ets_eq_pos).
-define(AttributeEts, ets_attribute). %% 包含属性点
-define(MapEts, ets_map).
-define(Undefined, undefined).

-record(sun_client, {name = "",
  is_online = ?True,
  request_time = 0 %% 总共请求生成属性点的次数
}).
-record(eq, {key = {player_name, eq_name}, player_name = "", eq_name = "", attribute = []}).
-record(eq_pos, {key = {player_name, pos}, player_name = "", pos = 0, eq_name = "", attribute = []}).
-record(attribute, {player_name = "", attribute_point = [], attribute_value = []}).
-record(map, {key = {player_name, map_cfg_id}, player_name = "", map_cfg_id = 0, pos_x = 0, pos_y = 0, blood = 0}).

%%%====================================================================
%%% API functions
%%%====================================================================
-export([start_link/0, start/0, send_2_me/1, call_me/1]).
-export([get_all/0, kill/0]).
-export([get_player_attribute/1, get_player_eq_list/1, get_player_eq_pos_list/1]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []). 
  
send_2_me(Msg) ->
  gen_server:cast(?SERVER, Msg).
  
call_me(Msg) ->
  gen_server:call(?SERVER, Msg).
  
%%%====================================================================
%%% Behavioural functions
%%%====================================================================
-record(state, {}).

init(Args) ->
  try
    State = do_init(Args),
	{ok, State}
  catch
    Class:ExceptionReason ->
	  sun_util:log_error(Class, ExceptionReason),
	  {stop, ExceptionReason}
  end.
  
handle_call(Request, From, State) ->
  try
	{Reply, NewState} = do_call(Request, From, State),
    {reply, Reply, NewState}
  catch
	Class:ExceptionReason ->
	  sun_util:log_error(Class, ExceptionReason),
	  {reply, {error, ExceptionReason}, State}
  end.
  
handle_cast(Request, State) ->
  try
	NewState = do_cast(Request, State),
	{noreply, NewState}
  catch
	Class:ExceptionReason ->
	  sun_util:log_error(Class, ExceptionReason),
	  {noreply, State}
  end.

handle_info(Info, State) ->
  try
	NewState = do_cast(Info, State),
	{noreply, NewState}
  catch
	Class:ExceptionReason ->
	  sun_util:log_error(Class, ExceptionReason),
	  {noreply, State}
  end.
  
terminate(Reason, State) ->
  try
	do_terminate(Reason, State)
  catch
	Class:ExceptionReason ->
	  sun_util:log_error(Class, ExceptionReason)
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_init([]) ->
  erlang:send_after(?UpdateTime, self(), on_tick),
  init(),
  #state{}.

do_call({?Client, Name, do_local_save_start}, _CallFrom, State) ->
  do_local_save_start(Name),
  {ok, State};
do_call({?Client, Name, Msg}, _CallFrom, State) ->
  Reply = case ets:member(?ClientEts, Name) of
            true -> select_what_todo({Msg, Name});
			_ -> not_login
          end,
  {Reply, State};
  
do_call(CallRequest, CallFrom, State) ->
  io:format("unknown call: request=~p, from=~p~n", [CallRequest, CallFrom]),
  Reply = ok,
  {Reply, State}.

do_cast(on_tick, State) ->
  erlang:send_after(?UpdateTime, self(), on_tick),
  EtsFun = ets:fun2ms(fun(#sun_client{is_online = ?True}) -> 1 end),
  case ets:select_count(?ClientEts, EtsFun) > 0 of
    ?True ->
      on_tick();
    _ -> skip
  end,
  State;

do_cast({?Client, save, Name}, State) ->
  do_save(Name),
  State;
do_cast({?Client, login, Name}, State) ->
  do_login(Name),
  State;
do_cast({?Client, logout, Name}, State) ->
  case ets:member(?ClientEts, Name) of
    ?True ->
      do_logout(Name),
      ets:update_element(?ClientEts, Name, {#sun_client.is_online, ?False});
    _ -> not_login
  end,
  State;
  
do_cast(CastRequest, State) ->
  io:format("unknown cast: request=~p~n", [CastRequest]),
  State.
  
do_terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% logic
%%%===================================================================

init() ->
  ets:new(?EqEts, [protected, named_table, {keypos, #eq.key}]),
  ets:new(?EqPosEts, [protected, named_table, {keypos, #eq_pos.key}]),
  ets:new(?AttributeEts, [protected, named_table, {keypos, #attribute.player_name}]),
  ets:new(?MapEts, [protected, named_table, {keypos, #map.key}]),
  ets:new(?ClientEts, [protected, named_table, {keypos, #sun_client.name}]).
  
on_tick() ->
  Now = sun_util:now(),
  SeasonChangeTime = 60 * 60, %% 60分钟
  WeatherChangeTime = 10 * 60, %% 10分钟
  if Now rem SeasonChangeTime == 0 ->
      do_season_change();
	Now rem WeatherChangeTime == 0 ->
	  do_weather_change();
	true -> skip
  end.
	
do_season_change() ->
  Str = 
  case get_season() of
    ?Undefined -> 
	  set_season(spring),
	  "Spring Come!";
	spring ->
	  set_season(summer),
	  "Summer Come!";
	summer ->
	  set_season(fall),
	  "Fall Come!";
	fall ->
	  set_season(winter),
	  "Winter Come!";
	_ ->
	  set_season(spring),
	  "Spring Come!"
  end,
  sun_util:io_format_prefix_not_end(Str ++ "~n").
 
do_weather_change() ->
  Str =
  case get_weather() of
    ?Undefined ->
	  set_weather(sunny),
	  "Today is sunny day!";
	sunny ->
	  set_weather(rainy),
	  "Today is rainy day!";
	Weather ->
	  WeatherList = [{1, sunny}, {2, rainy}, {3, windy}, {4, cloudy}],
	  Random = rand:uniform(length(WeatherList)), %% 1-n
	  case lists:keyfind(Random, 1, WeatherList) of
	    {_, Weather} -> "Today is also " ++ atom_to_list(Weather) ++ "day!";
		{_, Other} -> 
		  set_weather(Other),
		  "Oh! Today is " ++ atom_to_list(Other) ++ "day!"
	  end
  end,
  sun_util:io_format_prefix_not_end(Str ++ "~n").
  

%% 选择做什么
select_what_todo({enter_game_use_old, PlayerName}) ->
  case ets:lookup(?AttributeEts, PlayerName) of
    [Attr] ->
	  EqList = ets:match_object(?EqEts, #eq{player_name = PlayerName, _ = '_'}),
	  EqPosList = ets:match_object(?EqPosEts, #eq_pos{player_name = PlayerName, _ = '_'}),
	  {enter_game_use_old_success, Attr, EqList, EqPosList};
	_ -> no_role
  end;
select_what_todo({enter_game_use_new, PlayerName}) ->
  [Client] = ets:lookup(?ClientEts, PlayerName),
  OldTimes = Client#sun_client.request_time,
  case OldTimes < 3 of
    true ->
	  PointList = sun_util:get_random_point_list(),
      ValueList = sun_util:calc_attribute(PointList),
      Attr = #attribute{player_name = PlayerName, attribute_point = PointList, 
      attribute_value = ValueList},
      ets:insert(?AttributeEts, Attr),
      NewClient = Client#sun_client{request_time = Client#sun_client.request_time + 1},
      ets:insert(?ClientEts, NewClient),
      {enter_game_use_new_success, Attr};
	_ -> 
	  case get_player_attribute(PlayerName) of
	    [Attr] -> {enter_game_use_new_max_times, Attr};
		_ -> unknown_bug
	  end
  end;
select_what_todo({Msg, Name}) ->
  player:do_msg(Msg, Name).
  
do_login(Name) ->
  case ets:member(?ClientEts, Name) of
    ?False ->
      NewClient = #sun_client{name = Name},
      ets:insert(?ClientEts, NewClient);
    _ ->
      ets:update_element(?ClientEts, Name, {#sun_client.is_online, ?True})
  end.

%% 本地到server_ets
do_local_save_start(Name) ->
  case ets:member(?ClientEts, Name) of
    ?True -> skip;
    _ ->
      ClientEtsName = sun_client:get_client_ets_name(Name),
      RequestTime = lookup(ClientEtsName, request_time, 0),
      NewClient = #sun_client{name = Name, request_time = RequestTime},
      ets:insert(?ClientEts, NewClient),

      Attribute = lookup(ClientEtsName, attribute, []),
      EqList = lookup(ClientEtsName, eq_list, []),
      EqPosList = lookup(ClientEtsName, eq_pos_list, []),
      ets:insert(?AttributeEts, Attribute),
      ets:insert(?EqEts, EqList),
      ets:insert(?EqPosEts, EqPosList)
  end.

lookup(Table, Key, Default) ->
  case ets:lookup(Table, Key) of
    [{_, V} | _] -> V;
    _ -> Default
  end.

do_save(Name) ->
  case ets:lookup(?ClientEts, Name) of
    [#sun_client{request_time = RequestTime}]->
      Process = 0,
      [Attribute] = ets:lookup(?AttributeEts, Name),
      EqList = get_player_eq_list(Name),
      EqPosList = get_player_eq_pos_list(Name),
      Msg = [{name, Name}, {process, Process}, {attribute, Attribute}, {eq_list, EqList},
        {eq_pos_list, EqPosList}, {request_time, RequestTime}],
      sun_client:save({Name, Msg});
    _ -> bug
  end.

do_logout(Name) ->
  sun_client:log_out(Name),
  player:logout(Name).

get_player_attribute(Name) ->
  ets:lookup(?AttributeEts, Name).
get_player_eq_list(Name) ->
  ets:match_object(?EqEts, #eq{player_name = Name, _ = '_'}).
get_player_eq_pos_list(Name) ->
  ets:match_object(?EqPosEts, #eq_pos{player_name = Name, _ = '_'}).
  
get_all() -> ets:tab2list(?ClientEts).

kill() -> case whereis(?SERVER) of ?Undefined -> ok; Pid -> erlang:exit(Pid, kill) end.

%% 进程字典
%% 季节
get_season() -> get(sun_gen_server_season).
set_season(Season) -> put(sun_gen_server_season, Season).
%% 天气
get_weather() -> get(sun_gen_server_weather).
set_weather(Weather) -> put(sun_gen_server_weather, Weather).



