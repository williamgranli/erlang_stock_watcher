
%% @author: Berima Andam Kweku
%% @doc
%% Description: Function saves data to the couch_db database
%% @end
%% Created: 2013-11-15
%% @version 0.2


-module(database_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

-export([init/1, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_call/3]).
-export([start_link/0, save_data/1, delete_tickers/0]).


%% ====================================================================
%% Initialization functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	erlang:display('database_server started'),
  {ok, #state{}}.



%%============================================================================
%% Client Functions
%%============================================================================

%% @doc
%% Saves data to the couch_db database
%% Parameters: A valid JSON string
%% @end
save_data(Data)->
	gen_server:cast(database_server, {save_data, Data}).

%% @doc
%% Deletes all old ticker data saved in the database
%% @end
delete_tickers() ->
	gen_server:cast(database_server, {delete_tickers}).


%% Handles a call to save data
handle_cast({save_data, Data}, State)-> 
	spawn(fun() ->
	couch_connection:store("95.80.9.66",5984, Data)
	end),
	erlang:display('database_server reached'),
	{noreply, State};

 %% Handles delete all tickers call
handle_cast({delete_tickers}, State) ->
	spawn(fun() ->
	couch_connection:delete_tickers("95.80.9.66",5984)
	end),
	erlang:display('database_server reached'),
	{noreply, State};

















%%============================================================================
%% Catch casts that don't match a clause
%%============================================================================
handle_cast(Message, State) ->
    error_logger:info_report(["Error:", {message, Message}]),
    {noreply, State}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}. 

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

