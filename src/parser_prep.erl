
%% @author: Berima Kweku Andam
%% @doc
%% Contains a list of helper functions that are used by the parsers
%% And the control Unit
%% @end
%% Created: 2013-11-06
%% @version 0.2


-module(parser_prep).
-export([split_list/2, get_results/1, save_nasdaq_values/1, 
		save_nyse_values/1, save_amex_values/2, guru_get/1, 
		historical/1]).


%%============================================================================
%%% NASDAQ / Google Parser
%%============================================================================
%% @doc
%% Goes through a list of lists of tickers and for each ticker list
%% reformates the tickers into a string of the formate
%% "NASDAQ:ticker,NASDAQ:ticker"  and then
%% calls the google parser with the string, saves the return data from the
%% parser and passes it on to the database_server for saving
%% @end
	save_nasdaq_values([])-> 
		ok;
	save_nasdaq_values([H | T])->
		write_in_intervals_nasdaq(H),
		save_nasdaq_values(T).

%% @doc
%% Takes a list and passes it to be formated and fed to the google parser
%% Saves the return value and passes it to the database_server to be
%% saved.
%% Arguments: List of Tickers
%% Returns: boolean
%% @end
	write_in_intervals_nasdaq([]) -> 
		ok;
	write_in_intervals_nasdaq(List) ->
		timer:sleep(1000),
		
		NasdaqString = get_nasdaq_args(List),
		
		Data = google_parse:parse(lists:flatten(NasdaqString), []),

		case Data of
			error -> error;
			_ -> database_server:save_data(Data) 
		end.


%% @doc
%% Takes a list of tickers and reformats it into the form
%% "NADSAQ:ticker,NASDAQ:ticker"
%% Argument: List of Strings
%% Return: String
%% @end
get_nasdaq_args([]) -> 
	"";

get_nasdaq_args([H | []])->
	CompanyDetails = mochijson:decode(H),
	Something = get_results(CompanyDetails),
	 case is_atom(Something) of
		 true -> "";
		 false -> 
			 {Symbol, _Name, Market, _Sector, _Industry} = Something,
		
			 Market ++ ":" ++ string:strip(Symbol, both)++ get_nasdaq_args([])
	 end;


get_nasdaq_args([H|T]) ->
	CompanyDetails = mochijson:decode(H),
	Something = get_results(CompanyDetails),
	 case is_atom(Something) of
		 true -> get_nasdaq_args(T);
		 false -> 
			 {Symbol, _Name, Market, _Sector, _Industry} = Something,
			
			 Market ++ ":" ++ string:strip(Symbol, both)++ "," ++ get_nasdaq_args(T)
	 end.






%%============================================================================
%%% NYSE / Yahoo Parser
%%============================================================================
%% @doc
%% Goes through a list of lists of tickers and for each ticker list
%% reformats the tickers  into a list of tuples, calls the google parser with the tuple list,
%% saves the return data from the
%% parser and passes it on to the database_server for saving
%% @end
save_nyse_values([])-> 
	ok;
save_nyse_values([H | T])->
	write_in_intervals_nyse(H),
	save_nyse_values(T).


%% @doc
%% Takes a list and passes it to be formated and fed to the yahoo parser
%% Saves the return value and passes it to the database_server to be
%% saved.
%% Arguments: List of Tickers
%% Returns: boolean
%% @end
write_in_intervals_nyse([])-> ok;
write_in_intervals_nyse(List) ->
	timer:sleep(1000),
	YahooString = get_nyse_args(List),
	
	Data = yahoo_parser:read_source(YahooString),

	case Data of
			error -> error;
			_ -> database_server:save_data(Data) 
	end.


%% @doc
%% Takes a list of tickers and reformats it into
%% a list of 3 tuples of the the form:
%%{Symbol, Name, Market, Sector, Industry}
%% Argument: List of Strings
%% Return: List of tuples
%% @end
get_nyse_args([]) -> 
	"";

get_nyse_args([H | []])->
	CompanyDetails = mochijson:decode(H),
	Something = get_results(CompanyDetails),
	 case is_atom(Something) of
		 true -> [];
		 false -> 
			 {Symbol, _Name, Market, Sector, Industry} = Something,
			 [{string:strip(Symbol, both), Market, Sector, Industry} | get_nyse_args([])]
	 end;


get_nyse_args([H | T]) ->
	CompanyDetails = mochijson:decode(H),
	Something = get_results(CompanyDetails),
	 case is_atom(Something) of
		 true -> get_nyse_args(T);
		 false -> 
			 {Symbol, _Name, Market, Sector, Industry} = Something,

			 [{string:strip(Symbol, both), Market, Sector, Industry} | get_nyse_args(T)]
	 end.






%%============================================================================
%%% AMEX / Markitondemand Parser
%%============================================================================
%% @doc
%% Takes a list of tickers and reformats it into
%% a tuple of the the form:
%%{Symbol, Name, Market, Sector, Industry}
%% passes it to the markitondemand_parser,
%% saves the return json string and passes it on
%% to the database_server
%% Argument: List of Strings
%% Return: List of tuples
%% @end
save_amex_values([], Acc) ->
  database_server:save_data(Acc);

save_amex_values([H|T], Acc) ->
  timer:sleep(200),
  erlang:display(length(T)),
  CompanyDetails = mochijson:decode(H),
  {Symbol, _Name, Market, Sector, Industry} = get_results(CompanyDetails),
  Tuple = {Symbol, Market, Sector, Industry},
  JSON = markitondemand_parser:read_source(Tuple),


  case (is_atom(JSON)) of
    true ->
      timer:sleep(1000),
      save_amex_values(T, Acc);
    false ->
      save_amex_values(T, [JSON | Acc])
  end.





%%==========================================================================================
%% Investment Advice / Guru HTML parser
%%==========================================================================================
%% @doc
%% Takes a list of tickers and passes each to the guru_getter
%% to extract information from the nasdaq website about the
%% stock and passes on the return JSON to the database_server
%% to save
%% Argument: List of tickers
%% Return: boolean
%% @end
guru_get(Market)->
	CompanyDataList = get_tickers:read_file(Market),
	[guru_save(CompanyData)|| CompanyData <- CompanyDataList].


guru_save(X) ->
	CompanyDetails = mochijson:decode(X),
	{Ticker, _, _, _, _} = get_results(CompanyDetails),
	
	Data = guru_getter:read_page(Ticker),

	case Data of
			error -> error;
			_ -> database_server:save_data(Data) 
	end,	
	timer:sleep(400).






%%==========================================================================================
%% Historical Data / Yahoo historical_parser
%%==========================================================================================
%% @doc
%% Takes a list of tickers and passes each to the historical_parser
%% to extract historical stock data from the nasdaq website about the
%% stock and passes on the return JSON to the database_server
%% to save
%% Argument: List of tickers
%% Return: boolean
%% @end
historical([]) -> 
	ok;

historical([H|T]) -> 
	X = mochijson2:decode(H), 
	Tuple = get_results_binary(X),
	Y = historical_parser:read_source(Tuple),

	case Y of
		error -> 
			historical(T);
		_ -> 
			database_server:save_data(Y),
			timer:sleep(500),
			historical(T)
		end.






%%==========================================================================================
%% Helper functions used to prepare the data for the parsers
%%==========================================================================================

%% @doc
%% Takes a mochijason decoded tuple and extracts a tuple of the format
%% {Symbol, Name, Market, Sector, Industry}
%% Argument: tuple
%% Return: tuple
%% @end
get_results({struct, Output}) ->
	get_data(Output).


get_data([{"CompanyData", {struct, [{"Market", Market},{"Name", Name}, {"Symbol", Symbol}, {"Sector", Sector}, {"Industry", Industry}]}}]) ->
	Tuple = {Symbol, Name, Market, Sector, Industry},
	Tuple.

get_results_binary({struct, Output}) ->
	X = proplists:get_value(<<"CompanyData">>, Output, not_found0),
	get_data_binary(X).  

get_data_binary({struct, X}) ->
	Symbol = binary_to_list(proplists:get_value(<<"Symbol">>, X, not_found1)),
	Name = binary_to_list(proplists:get_value(<<"Name">>, X, not_found2)),
	Market = binary_to_list(proplists:get_value(<<"Market">>, X, not_found3)),
	Sector = binary_to_list(proplists:get_value(<<"Sector">>, X, not_found4)),
	Industry = binary_to_list(proplists:get_value(<<"Industry">>, X, not_found5)),

	{Symbol, Name, Market, Sector, Industry}.


%% @doc
%% Takes a list and splits it into a list of smaller lists
%% Size of the smaller list is specified by user
%% Argument: List, Interval
%% Return: listofList
%% @end
split_list(List, Interval) ->
	split_list(List, [], Interval).

split_list(List, Acc, Interval) ->
	A = length(List),
	case A > Interval of
		true ->  B = [lists:sublist(List, Interval) | Acc],
				split_list(lists:sublist(List, Interval + 1, A), B, Interval);
		false ->[List | Acc]
	end.