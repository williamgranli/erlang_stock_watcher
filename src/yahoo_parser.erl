%% @author William Granli
%% @doc 
%% 	E: Extracts data from www.finance.yahoo.com
%%	T: Transforms the data i.e. extracts the information we want
%% 	L: Returns the JSON string so that it can be sent to the DB module
%% @end

-module(yahoo_parser).
-export([read_source/1]).

%% Takes a list of 160 tickers, market, sector and industry
read_source(List) ->  
	seperate_by_plus(List, [], []).

%% Makes the list of tickers into a huge string that is put in the URL when calling the source
%% The market, sector and industry are packed into tuples and stored in a list of those tuples
seperate_by_plus([{Ticker, Market, Sector, Industry}|[]], String, CompanyDataList) -> 
	FinishedString = String++[Ticker],
	FinishedList = CompanyDataList++[{Market, Sector, Industry}],
	make_call(FinishedString, FinishedList);
seperate_by_plus([{Ticker, Market, Sector, Industry}|T], String, CompanyDataList) -> 
	seperate_by_plus(T, String++[Ticker]++[$+], CompanyDataList++[{Market, Sector, Industry}]).

%% Http request with all the tickers seperate by +
%% What information we want to extract is specified in the end of the URL, as below
%% s=Ticker, n=name, b3=bid, o=openPrice, h=dayHigh, g=dayLow, v=volume	
make_call(TickerString, CompanyDataList) ->
	inets:start(),
	A = httpc:request(get, {"http://finance.yahoo.com/d/quotes.csv?s=" ++ TickerString ++ "&f=snb3ohgv" ,[]},[],[]),
	inets:stop(),
	case A of
		{ok,{_,_,X}} -> parse(X, CompanyDataList);
		_ -> error
	end.

%% Removes the two first characters of the answer
parse(X, CompanyDataList) ->
TickerString = lists:reverse(tl(tl(lists:reverse(X)))),
seperate_companies(TickerString, [], [], CompanyDataList).

%% Returns the finished list of JSON strings
seperate_companies([], Company, ListOfJSON, [H|[]]) ->
	FinishedList = [extract_values(Company, 0, 0, [], [],[],[],[],[],[], H)|ListOfJSON],
	FinishedList;
seperate_companies([$\r, $\n | T1], Company, ListOfJSON, [CompanyDataTuple|T2]) ->
	seperate_companies(T1, [], [extract_values(Company, 0, 0, [], [],[],[],[],[],[], CompanyDataTuple)|ListOfJSON], T2);
seperate_companies([H|T], Company, ListOfJSON, CompanyDataList) ->
	seperate_companies(T, Company++[H], ListOfJSON, CompanyDataList).

%% Parses the answer and stores everything in variables.
%% Sends the variables to jsonify
extract_values([], _, _, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) ->
	jsonify(Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple);
extract_values([$,|T], 3, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, 3, CommaCounter-1, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple);
extract_values([$"|T], QuotationCounter, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter+1, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple );
extract_values([$,|T], QuotationCounter, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter, CommaCounter+1, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple);
extract_values([H|T], 1, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, 1, CommaCounter, Ticker++[H], Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple);
extract_values([H|T], 3, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, 3, CommaCounter, Ticker, Name++[H], CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple);
extract_values([H|T], QuotationCounter, 2, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter, 2, Ticker, Name, CurrentPrice++[H], OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple);
extract_values([H|T], QuotationCounter, 3, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter, 3, Ticker, Name, CurrentPrice, OpeningPrice++[H], HighestPrice, LowestPrice, Volume, CompanyDataTuple);
extract_values([H|T], QuotationCounter, 4, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter, 4, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice++[H], LowestPrice, Volume, CompanyDataTuple);
extract_values([H|T], QuotationCounter, 5, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter, 5, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice++[H], Volume, CompanyDataTuple);
extract_values([H|T], QuotationCounter, 6, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter, 6, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume++[H], CompanyDataTuple);
extract_values([_|T], QuotationCounter, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple) -> 
	extract_values(T, QuotationCounter, CommaCounter, Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, CompanyDataTuple).

%% Unpacks the company data tuple and formats everything to JSON
jsonify(Ticker, Name, CurrentPrice, OpeningPrice, HighestPrice, LowestPrice, Volume, {Market, Sector, Industry}) -> 
	Time = erlang:localtime(),
	FormattedDate = get_date(Time),
	FormattedTime = get_time(Time),
	OutputJSONObject = 
			[${]++[$"]++"StockData"++[$"]++[$:]++
			[${]++
			[$"]++"Market"			++[$"]++[$:]++[$"]++Market			++[$"]++[$,]++
			[$"]++"Symbol"			++[$"]++[$:]++[$"]++Ticker			++[$"]++[$,]++
			[$"]++"Name"			++[$"]++[$:]++[$"]++Name			++[$"]++[$,]++
			[$"]++"Date"			++[$"]++[$:]++[$"]++FormattedDate	++[$"]++[$,]++
			[$"]++"Time"			++[$"]++[$:]++[$"]++FormattedTime	++[$"]++[$,]++
			[$"]++"CurrentPrice"	++[$"]++[$:]++[$"]++CurrentPrice	++[$"]++[$,]++
			[$"]++"OpeningPrice"	++[$"]++[$:]++[$"]++OpeningPrice	++[$"]++[$,]++
			[$"]++"HighestPrice"	++[$"]++[$:]++[$"]++HighestPrice	++[$"]++[$,]++
			[$"]++"LowestPrice"		++[$"]++[$:]++[$"]++LowestPrice		++[$"]++[$,]++
			[$"]++"Volume"			++[$"]++[$:]++[$"]++Volume			++[$"]++[$,]++
			[$"]++"Sector"			++[$"]++[$:]++[$"]++Sector			++[$"]++[$,]++
			[$"]++"Industry"		++[$"]++[$:]++[$"]++Industry		++[$"]++	
			[$}]++
			[$}],
		OutputJSONObject.


%% Changes the date format to the DB format we want
get_date({{Year, Month, Day}, {_Hour, _Minute, _Second}}) -> 
	Date = {integer_to_list(Year),integer_to_list(Month),integer_to_list(Day)},
	add_leading_zero_to_date(Date).

add_leading_zero_to_date({Year, [Month|[]], [Day|[]]}) ->
	FormattedDate = Year++[$-]++[$0]++[Month]++[$-]++[$0]++[Day],
	FormattedDate;
add_leading_zero_to_date({Year, [Month|[]], Day}) -> 
	FormattedDate = Year++[$-]++[$0]++[Month]++[$-]++Day,
	FormattedDate;
add_leading_zero_to_date({Year, Month, [Day|[]]}) ->
	FormattedDate = Year++[$-]++Month++[$-]++[$0]++[Day],
	FormattedDate;
add_leading_zero_to_date({Year, Month, Day}) ->
	FormattedDate = Year++[$-]++Month++[$-]++Day,
	FormattedDate.

get_time({{_Year, _Month, _Day}, {Hour, Minute, Second}}) -> 
	ListHour = integer_to_list(Hour),
	ListMin = integer_to_list(Minute),
	ListSec = integer_to_list(Second),

	case string:len(ListHour) of
		1 -> add_leading_zero_to_min({[$0]++ListHour, ListMin, ListSec});
		2 -> add_leading_zero_to_min({ListHour, ListMin, ListSec})
	end.

add_leading_zero_to_min({Hour, Minute, Second}) ->
	case string:len(Minute) of
		1 -> add_leading_zero_to_sec({Hour, [$0]++Minute, Second});
		2 -> add_leading_zero_to_sec({Hour, Minute, Second})
	end.
add_leading_zero_to_sec({Hour, Minute, Second}) -> 
	case string:len(Second) of
		1 -> Hour++[$:]++Minute++[$:]++[$0]++Second;
		2 -> Hour++[$:]++Minute++[$:]++Second
	end.