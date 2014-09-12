%% @author William Granli
%% @doc 
%% An event "server" that sends events to control_unit based on timestamps
%% @end

-module(scheduler).
-export([start/0]).

%%Gothenburg time:
%% Markets open 15:30
%% Markets close 22:00 

%% Initiates the counting
start() -> 
	erlang:display('scheduler started'),
	start(erlang:localtime()).

%% Gets the guru investment advice dadvice for each company. Executes right after market close, once a day. 
start({{_, _, _}, {22, 00, 00}})  -> 
	control_unit:guru_get(),
	CurrentTime = erlang:localtime(),
	timer:sleep(1000),
	start(CurrentTime);

%%Does nothing because markets are closed. Acts as a guard for the function below. 
start({{_, _, _}, {Hour, Minute, _}}) when Hour >= 22; Hour < 15; Hour == 15, Minute < 24 -> 
	CurrentTime = erlang:localtime(),
	timer:sleep(1000),
	start(CurrentTime);

start({{_, _, _}, {Hour, Minute, Sec}}) -> 
	
	if
		%% Gets the tickers for all 3 markets. Executes 2 minutes before markets open.
		Hour == 15, Minute == 25, Sec == 00 ->
			control_unit:update_tickers();
		
		%% Gets the stock data for NASDAQ. Updates twice every hour at HH:00:00 and HH:30:00
		((Minute == 00) and (Sec == 00)) or ((Minute == 30) and (Sec == 00)) ->
			control_unit:update_nasdaq();
		
		%% Gets the stock data for NYSE. Updates twice every hour at HH:10:00 and HH:40:00
		((Minute == 10) and (Sec == 00)) or ((Minute == 40) and (Sec == 00)) ->
			control_unit:update_nyse();
		
		%% Gets the stock data for Amex. Updates twice every hour at HH:20:00 and HH:50:00
		((Minute == 20) and (Sec == 00)) or ((Minute == 50) and (Sec == 00))->
			control_unit:update_amex();

		%% Historical Data 
		Hour == 02, Minute == 00, Sec == 00 ->
			control_unit:get_historical("AMEX");
		Hour == 03, Minute == 00, Sec == 00 ->
			control_unit:get_historical("NASDAQ");
		Hour == 04, Minute == 00, Sec == 00 ->
			control_unit:get_historical("NYSE");
		
		1==1 -> ok
	end,
	
	%%Starts the counting again
	CurrentTime = erlang:localtime(),
	timer:sleep(1000),
	start(CurrentTime).