-module(baby_fsm).

-behaviour(multi_fsm).

%% multi_fsm callbacks
-export([hungry/2, not_hungry/2, thirsty/2, not_thirsty/2,
         state/2, init/1, terminate/2, code_change/3,
         handle_event/2, handle_sync_event/3, handle_info/2]).

%% API
-export([start/1, eat/1, drink/1]).

-record(state, {name,
                hunger_state,
                hunger_timer,
                thirst_state,
                thirst_timer}).
                
-define(eating_timeout,  10000).
-define(drinking_timeout, 5000).


%%% ==================================================================
%%%  API
%%% ==================================================================

start(Name) ->
    multi_fsm:start({local, Name}, ?MODULE, Name, []).

eat(Name) ->
    gen_fsm:send_event(Name, eat).

drink(Name) ->
    gen_fsm:send_event(Name, drink).

%%% ==================================================================
%%%  multi_fsm callbacks
%%% ==================================================================

%% hunger FSM
hungry(eat, State) ->
    io:format("~p is hungry and eating~n", [State#state.name]),
    Timer = gen_fsm:start_timer(?eating_timeout, hunger_timer),
    {next_state, State#state{hunger_state=not_hungry,
                             hunger_timer=Timer}}.
not_hungry(eat, State) ->
    io:format("~p is not hungry but eating~n", [State#state.name]),
    Timer = gen_fsm:start_timer(?eating_timeout, hunger_timer),
    gen_fsm:cancel_timer(State#state.hunger_timer),
    {next_state, State#state{hunger_timer=Timer}};
not_hungry({timeout, _Timer, hunger_timer}, State) ->
    io:format("~p is now hungry~n", [State#state.name]),
    {next_state, State#state{hunger_state=hungry}}.

%% thirst FSM
thirsty(drink, State) ->
    io:format("~p is thirsty and drinking~n", [State#state.name]),
    Timer = gen_fsm:start_timer(?drinking_timeout, thirst_timer),
    {next_state, State#state{thirst_state=not_thirsty,
                             thirst_timer=Timer}}.

not_thirsty(drink, State) ->
    io:format("~p is not thirsty but drinking~n", [State#state.name]),
    Timer = gen_fsm:start_timer(?drinking_timeout, thirst_timer),
    gen_fsm:cancel_timer(State#state.thirst_timer),
    {next_state, State#state{thirst_timer=Timer}};
not_thirsty({timeout, _Timer, thirst_timer}, State) ->
    io:format("~p is now thirsty~n", [State#state.name]),
    {next_state, State#state{thirst_state=thirsty}}.

%% dispatch FSM
state(eat, State) ->
    {ok, State#state.hunger_state};
state({timeout, _Timer, hunger_timer}, State) ->
    {ok, State#state.hunger_state};
state(drink, State) ->
    {ok, State#state.thirst_state};
state({timeout, _Timer, thirst_timer}, State) ->
    {ok, State#state.thirst_state}.
    
init(Name) ->
    {ok, #state{name=Name,
                hunger_state=hungry,
                thirst_state=thirsty}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OVsn, State, _Extra) ->
    {ok, State}.

handle_event(_Event, State) ->
    {next_state, State}.

handle_sync_event(_Event, _From, State) ->
    {next_state, State}.

handle_info(_Info, State) ->
    {next_state, State}.

