%% @author William Granli
%% @doc 
%% 	E: Extracts data from www.dev.markitondemand.comm
%%	T: Transforms the data i.e. extracts the information we want
%% 	L: Returns the JSON string so that it can be sent to the DB module
%% @end

-module(markitondemand_parser).
-export([read_source/1]).

%% Sends a http request to the source. The source returns a JSON string inside some html tags.
read_source({Symbol, Market, Sector, Industry}) ->  
	InfoTuple = {Market, Sector, Industry},
	inets:start(),
	A = httpc:request(get, {"http://dev.markitondemand.com/Api/Quote/jsonp?Symbol=" ++ Symbol ++ "&callback=myFunction",[]},[],[]),
	inets:stop(),
	case A of 
		{ok,{_,_,X}} -> removeStart(X, InfoTuple);
		_ -> error %% catches http errors
	end.

%% strips the answer down to the json string only
removeStart([$m,$y,$F,$u,$n,$c,$t,$i,$o,$n, $( | T], InfoTuple) -> 	%% removes myFunction from the beginning
	helper_call(lists:reverse(tl(lists:reverse(T))), InfoTuple). 		%% removes ) from the end

helper_call(T, InfoTuple) -> 
	Output = mochijson2:decode(T),
	get_results(Output, InfoTuple).

% @doc
% gets rid of the outer tuple. (the name of the json object)
get_results({struct, Output}, InfoTuple) ->
	X = proplists:get_value(<<"Data">>, Output, not_found0),
	get_data(X, InfoTuple).

% @doc
% extracts the data we want for the database
get_data(not_found0, _) -> not_found;

get_data({struct, X}, {Market, Sector, Industry}) ->
	Ticker = binary_to_list(proplists:get_value(<<"Symbol">>, X, not_found1)),
	Name = binary_to_list(proplists:get_value(<<"Name">>, X, not_found2)),
	
	TimeStamp = erlang:localtime(),
	Time = get_time(TimeStamp),
	Date = get_date(TimeStamp),

	CurrentPrice = 
	try float_to_list(proplists:get_value(<<"LastPrice">>, X, not_found5)) of
		_ -> float_to_list(proplists:get_value(<<"LastPrice">>, X, not_found5))
	catch
		error:_ -> integer_to_list(proplists:get_value(<<"LastPrice">>, X, not_found5))
	end,

	OpeningPrice = 
	try float_to_list(proplists:get_value(<<"Open">>, X, not_found6)) of
		_ -> float_to_list(proplists:get_value(<<"Open">>, X, not_found6))
	catch
		error:_ -> integer_to_list(proplists:get_value(<<"Open">>, X, not_found6))
	end,
	
	HighestPrice = 
	try float_to_list(proplists:get_value(<<"High">>, X, not_found5)) of
		_ -> float_to_list(proplists:get_value(<<"High">>, X, not_found5))
	catch
		error:_ -> integer_to_list(proplists:get_value(<<"High">>, X, not_found5))
	end,
	
	LowestPrice = 
	try float_to_list(proplists:get_value(<<"Low">>, X, not_found5)) of
		_ -> float_to_list(proplists:get_value(<<"Low">>, X, not_found5))
	catch
		error:_ -> integer_to_list(proplists:get_value(<<"Low">>, X, not_found5))
	end,

	Volume = integer_to_list(proplists:get_value(<<"Volume">>, X, not_found9)),
	
% @doc	
% formats a json object	
	OutputJSONObject = 
		[${]++[$"]++"StockData"++[$"]++[$:]++
			[${]++
			[$"]++"Market"			++[$"]++[$:]++[$"]++Market			++[$"]++[$,]++
			[$"]++"Symbol"			++[$"]++[$:]++[$"]++Ticker			++[$"]++[$,]++
			[$"]++"Name"			++[$"]++[$:]++[$"]++Name			++[$"]++[$,]++
			[$"]++"Date"			++[$"]++[$:]++[$"]++Date			++[$"]++[$,]++
			[$"]++"Time"			++[$"]++[$:]++[$"]++Time			++[$"]++[$,]++
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


%% formats the date and time
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