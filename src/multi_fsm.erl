%%%%  Copyright (c) 2009 Mark Fine <mark.fine@gmail.com>
%%%%
%%%%  Permission is hereby granted, free of charge, to any person
%%%%  obtaining a copy of this software and associated documentation
%%%%  files (the "Software"), to deal in the Software without
%%%%  restriction, including without limitation the rights to use,
%%%%  copy, modify, merge, publish, distribute, sublicense, and/or sell
%%%%  copies of the Software, and to permit persons to whom the
%%%%  Software is furnished to do so, subject to the following
%%%%  conditions:
%%%%
%%%%  The above copyright notice and this permission notice shall be
%%%%  included in all copies or substantial portions of the Software.
%%%%
%%%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%%%  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%%%  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%%%  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%%%  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%%%  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%%%  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%%%  OTHER DEALINGS IN THE SOFTWARE.

%%%%  $Id: multi_fsm.erl,v 1.1.1.1 2009/03/07 06:01:49 mfine Exp $
%%%%
%%%%  This module is a wrapper around gen_fsm for dispatching on multiple
%%%%  states. Modifies gen_fsm callbacks by removing StateName parameters
%%%%  and return values. Adds callback to idenity which state to dispatch
%%%%  on. gen_fsm APIs not overlaid by this module can be used.

-module(multi_fsm).

-author("Mark Fine <mark.fine@gmail.com").

-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([state/2, state/3, init/1, terminate/3, code_change/4,
         handle_event/3, handle_sync_event/4, handle_info/3]).

%% API
-export([start/3, start/4, start_link/3, start_link/4,
         enter_loop/2, enter_loop/3, enter_loop/4]).

-export([behaviour_info/1]).


%%% =======================================================================
%%%  API
%%% =======================================================================

start(Mod, Args, Options) ->
    gen_fsm:start(?MODULE, {Mod, Args}, Options).
start(Name, Mod, Args, Options) ->
    gen_fsm:start(Name, ?MODULE, {Mod, Args}, Options).

start_link(Mod, Args, Options) ->
    gen_fsm:start_link(?MODULE, {Mod, Args}, Options).
start_link(Name, Mod, Args, Options) ->
    gen_fsm:start_link(Name, ?MODULE, {Mod, Args}, Options).

enter_loop(Options, State) ->
    gen_fsm:enter_loop(?MODULE, Options, state, State).
enter_loop(Options, State, Name) ->
    gen_fsm:enter_loop(?MODULE, Options, state, State, Name).
enter_loop(Options, State, Name, Timeout) ->
    gen_fsm:enter_loop(?MODULE, Options, state, State, Name, Timeout).

behaviour_info(callbacks) ->
    [{state,2},{init,1},{terminate,2},{code_change,3},
     {handle_event,2},{handle_sync_event,3},{handle_info,2}];
behaviour_info(_Other) ->
    undefined.


%%% =======================================================================
%%%  gen_fsm callbacks
%%% =======================================================================

state(Event, {Mod, State}) ->
    case catch Mod:state(Event, State) of
        {ok, Fun} ->
            case Mod:Fun(Event, State) of
                {next_state, NState} ->
                    {next_state, state, {Mod, NState}};
                {next_state, NState, Timeout} ->
                    {next_state, state, {Mod, NState}, Timeout};
                Else ->
                    Else
            end;
        Error ->
            Error
    end.
state(Event, From, {Mod, State}) ->
    case catch Mod:state(Event, State) of
        {ok, Fun} ->
            case Mod:Fun(Event, From, State) of
                {next_state, NState} ->
                    {next_state, state, {Mod, NState}};
                {next_state, NState, Timeout} ->
                    {next_state, state, {Mod, NState}, Timeout};
                {reply, Reply, NState} ->
                    {reply, Reply, state, {Mod, NState}};
                {reply, Reply, NState, Timeout} ->
                    {reply, Reply, state, {Mod, NState}, Timeout};
                Else ->
                    Else
            end;
        Error ->
            Error
    end.

init({Mod, Args}) ->
    case Mod:init(Args) of
        {ok, State} ->
            {ok, state, {Mod, State}};
        {ok, State, Timeout} ->
            {ok, state, {Mod, State}, Timeout};
        Else ->
            Else
    end.

terminate(Reason, state, {Mod, State}) ->
    Mod:terminate(Reason, State).

code_change(OVsn, state, {Mod, State}, Extra) ->
    case Mod:code_change(OVsn, State, Extra) of
        {ok, NState} ->
            {ok, state, {Mod, NState}};
        Else ->
            Else
    end.

handle_event(Event, state, {Mod, State}) ->
    case Mod:handle_event(Event, State) of
        {next_state, NState} ->
            {next_state, state, {Mod, NState}};
        {next_state, NState, Timeout} ->
            {next_state, state, {Mod, NState}, Timeout};
        Else ->
            Else
    end.

handle_sync_event(Event, From, state, {Mod, State}) ->
    case Mod:handle_sync_event(Event, From, State) of
        {next_state, NState} ->
            {next_state, state, {Mod, NState}};
        {next_state, NState, Timeout} ->
            {next_state, state, {Mod, NState}, Timeout};
        {reply, Reply, NState} ->
            {reply, Reply, state, {Mod, NState}};
        {reply, Reply, NState, Timeout} ->
            {reply, Reply, state, {Mod, NState}, Timeout};
        Else ->
            Else
    end.

handle_info(Info, state, {Mod, State}) ->
    case Mod:handle_info(Info, State) of
        {next_state, NState} ->
            {next_state, state, {Mod, NState}};
        {next_state, NState, Timeout} ->
            {next_state, state, {Mod, NState}, Timeout};
        Else ->
            Else
    end.
