%%% @author William Granli 
%%% @doc
%%% Erlang application file
%%% How to run: 
%%% 0. Run cmd
%%% 1. Navigate to the project folder
%%% 2. Run: erl -pa ebin/
%%% 3. Run: applicaton:start(stocksperitas).
%%% You must have Erlang added to enviromental variables before you can run the application.
%%% @end 
%%%-------------------------------------------------------------------
-module(stocksperitas_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
	stock_supervisor:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.