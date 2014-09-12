%% @author William Granli
%% @doc 
%% E: Reads Yahoo's .csv files of historical data.
%% T: Extracts the data we want and transforms it to JSON
%% L: Returns the JSON string so that it can be sent to the DB module
%% @end

-module(historical_parser).
-export([read_source/1]).

%% Sends a http request to the site, which returns the .csv file
%% The timespan of the data is specified in the URL
read_source({Symbol, Name, Market, Sector, Industry}) ->  
	InfoTuple = {Symbol, Name, Market, Sector, Industry}, %packs the extra values into a tuple
	inets:start(),
	Answer = httpc:request(get, {"http://ichart.yahoo.com/table.csv?s=" ++ Symbol ++ "&a=0&b=1&c=2012&d=11&e=10&f=2013&g=d" ,[]},[],[]),
	inets:stop(),

	case Answer of 
		{ok,{_,_,X}} ->	parse(X, [], [], [], [], [], [], 0, InfoTuple, []);
		_ -> error
	end.

%% Parses each line of the file, through counting commas and linebreaks
%% Returns the list of JSON strings
parse([$\n|[]], _Date, _OpeningPrice, _HighestPrice, _LowestPrice, _CurrentPrice, 
	_Volume, _CommaCounter,{_Symbol, _Name, _Market, _Sector, _Industry}, ListOfJSON) ->
	ListOfJSON;
parse([$\n|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, 
	Volume, _CommaCounter, {Symbol, Name, Market, Sector, Industry}, ListOfJSON) ->

	JSONObject = 
			[${]++[$"]++"StockData"++[$"]++[$:]++
			[${]++
			[$"]++"Market"			++[$"]++[$:]++[$"]++Market			++[$"]++[$,]++
			[$"]++"Symbol"			++[$"]++[$:]++[$"]++Symbol			++[$"]++[$,]++
			[$"]++"Name"			++[$"]++[$:]++[$"]++Name			++[$"]++[$,]++
			[$"]++"Date"			++[$"]++[$:]++[$"]++Date			++[$"]++[$,]++
			[$"]++"Time"			++[$"]++[$:]++[$"]++"10:58:00"		++[$"]++[$,]++
			[$"]++"CurrentPrice"	++[$"]++[$:]++[$"]++CurrentPrice	++[$"]++[$,]++
			[$"]++"OpeningPrice"	++[$"]++[$:]++[$"]++OpeningPrice	++[$"]++[$,]++
			[$"]++"HighestPrice"	++[$"]++[$:]++[$"]++HighestPrice	++[$"]++[$,]++
			[$"]++"LowestPrice"		++[$"]++[$:]++[$"]++LowestPrice		++[$"]++[$,]++
			[$"]++"Volume"			++[$"]++[$:]++[$"]++Volume			++[$"]++[$,]++
			[$"]++"Sector"			++[$"]++[$:]++[$"]++Sector			++[$"]++[$,]++
			[$"]++"Industry"		++[$"]++[$:]++[$"]++Industry		++[$"]++[$,]++
			[$"]++"Historical"		++[$"]++[$:]++[$"]++"true"			++[$"]++	
			[$}]++
			[$}],
	parse(T, [], [], [], [], [], [], 0, {Symbol, Name, Market, Sector, Industry}, [JSONObject|ListOfJSON]);

parse([$,|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, CommaCounter, InfoTuple, ListOfJSON) -> 
	parse(T, Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, CommaCounter+1, InfoTuple, ListOfJSON); 
parse([H|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 0, InfoTuple, ListOfJSON) ->
	parse(T, Date++[H], OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 0, InfoTuple, ListOfJSON); 
parse([H|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 1, InfoTuple, ListOfJSON) -> 
	parse(T, Date, OpeningPrice++[H], HighestPrice, LowestPrice, CurrentPrice, Volume, 1, InfoTuple, ListOfJSON);
parse([H|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 2, InfoTuple, ListOfJSON) ->
	parse(T, Date, OpeningPrice, HighestPrice++[H], LowestPrice, CurrentPrice, Volume, 2, InfoTuple, ListOfJSON); 
parse([H|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 3, InfoTuple, ListOfJSON) ->
	parse(T, Date, OpeningPrice, HighestPrice, LowestPrice++[H], CurrentPrice, Volume, 3, InfoTuple, ListOfJSON); 
parse([H|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 4, InfoTuple, ListOfJSON) ->
	parse(T, Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice++[H], Volume, 4, InfoTuple, ListOfJSON); 
parse([H|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 5, InfoTuple, ListOfJSON) ->
	parse(T, Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume++[H], 5, InfoTuple, ListOfJSON);
parse([_|T], Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 6, InfoTuple, ListOfJSON) ->
	parse(T, Date, OpeningPrice, HighestPrice, LowestPrice, CurrentPrice, Volume, 6, InfoTuple, ListOfJSON).