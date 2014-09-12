%% @author William Granli
%% @doc 
%% 	OTP server that manages concurrency and processes
%%	receives messages from scheduler.erl and serves as a 
%%  control unit for the parsers
%% @end

-module(control_unit).
-behaviour(gen_server).

%% API
-export([start_link/0, update_nasdaq/0, update_nyse/0, update_amex/0, guru_get/0, update_tickers/0, get_historical/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).


%%====================================================================
%% Initialization Functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
	erlang:display('control_unit started'),
	{ok, #state{}}.

%%====================================================================
%% API Functions
%%====================================================================
update_tickers() ->
	gen_server:cast(control_unit, update_tickers).

update_nasdaq() ->
	gen_server:cast(control_unit, {update_nasdaq}).

update_nyse() ->
	gen_server:cast(control_unit, {update_nyse}).

update_amex() ->
	gen_server:cast(control_unit, {update_amex}).

guru_get()->
	gen_server:cast(control_unit, guru_get).

get_historical(Market) ->
	gen_server:cast(control_unit, {get_historical, Market}).


%%============================================================================
%%% Deletes and updates the tickers in the database 
%%============================================================================
handle_cast(update_tickers, State) ->

	database_server:delete_tickers(),	%delete current tickers

	case get_tickers:read_file("NASDAQ") of
		error -> error;
		_ -> database_server:save_data(get_tickers:read_file("NASDAQ")) %save nasdaq tickers
	end,

	case get_tickers:read_file("AMEX") of
		error -> error;
		_ -> database_server:save_data(get_tickers:read_file("AMEX")) %save amex tickers
	end,

	case get_tickers:read_file("NYSE") of
		error -> error;
		_ -> database_server:save_data(get_tickers:read_file("NYSE")) %save nyse tickers
	end,
	{noreply, State};


%%====================================================================
%%% Get and save stock data in the database (Author: Andam Berima)
%%====================================================================
handle_cast({update_nasdaq}, State) ->
	spawn(fun() ->
		TickerList = get_tickers:read_file("NASDAQ"), %get tickers from db
		TickerListList  = parser_prep:split_list(TickerList, 160), %prepare list for parser
		parser_prep:save_nasdaq_values(TickerListList) %send list to parser
	end),
	{noreply, State};

handle_cast({update_nyse}, State) ->
	spawn(fun() -> 
		TickerList = get_tickers:read_file("NYSE"), %get tickers from db
		TickerListList = parser_prep:split_list(TickerList, 200), %prepare list for parser
		parser_prep:save_nyse_values(TickerListList) %send list to parser
	end),
	{noreply, State};

handle_cast({update_amex}, State) ->
	spawn(fun() -> 
		TickerList = get_tickers:read_file("AMEX"), %get tickers from db
    	parser_prep:save_amex_values(TickerList, []) %send list to parser
	end),
	{noreply, State};


%%============================================================================
%% Get and save stock investment advice to the database (Author: Andam Berima)
%%============================================================================
handle_cast(guru_get, State) ->
	spawn(fun()->
	parser_prep:guru_get("NASDAQ"),
	parser_prep:guru_get("AMEX"),
	parser_prep:guru_get("NYSE") 
		  end),

	{noreply, State};	

%%============================================================================
%%% Get Historical Data and save it to the database 
%%============================================================================
handle_cast({get_historical, Market}, State) -> 
	spawn(fun()->
		TickerList = get_tickers:read_file(Market), 
		parser_prep:historical(TickerList)
		end),	

	{noreply, State};

%%============================================================================
%% Catch casts that don't match a clause
%%============================================================================
handle_cast(Message, State) ->
    error_logger:info_report(["Error:", {message, Message}]),
    {noreply, State}.

%%-----------Not Used---------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%-------------------Not Used-------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%---------------------------Not Used-----------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%-----------------------------------Not Used---------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}. 