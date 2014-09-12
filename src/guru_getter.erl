%% @author William Granli
%% @doc 
%% E: Extracts data from http://www.nasdaq.com/symbol/aapl/guru-analysis through HTML parsing (example url)
%% T: Transforms the data i.e. extracts the information we want
%% L: Returns the JSON string so that it can be sent to the DB module
%% @end

-module(guru_getter).
-export([read_page/1]).

%% Sends a http request to the source site. The request returns the html source.
read_page(Symbol) ->  
	inets:start(),
	A = httpc:request(get, {"http://www.nasdaq.com/symbol/" ++ Symbol ++ "/guru-analysis" ,[]},[],[]),
	inets:stop(),

	case A of 
		{ok,{_,_,X}} -> parse_page(X, [], [], 0, Symbol);
		_ -> error %catches any http error
	end. 

%% Finds the correct HTML span
parse_page([], _HTML_span, ListOfSpans, _TagCounter, Symbol) -> 
	finish(ListOfSpans, [], Symbol);
parse_page([$<,$s,$p,$a,$n,32,$i,$d,$=,$",$q,$u,$o,$t,$e,$s,$_,$c,$o,$n,$t,$e,$n,$t,$_,$l,$e,$f,$t,$_,$l |T], 
	HTML_span, ListOfSpans, _TagCounter, Symbol) -> %matches the start of the tag
	parse_page(T, HTML_span, ListOfSpans, 1, Symbol);
parse_page([$D,$e,$t,$a,$i,$l,$e,$d,32,$A,$n,$a,$l,$y,$s,$i,$s|T], HTML_span, ListOfSpans, _TagCounter, Symbol) -> %matches the end of the tag
	parse_page(T, [], [HTML_span|ListOfSpans], 0, Symbol);
parse_page([_|T], HTML_span, ListOfSpans, 0, Symbol) ->
	parse_page(T, HTML_span, ListOfSpans, 0, Symbol);
parse_page([H|T], HTML_span, ListOfSpans, 1, Symbol) -> 
	parse_page(T, HTML_span++[H], ListOfSpans, 1, Symbol).

%% Sends one span at a time to the parse_span function
finish([], ListOfTuples, _Symbol) -> 
	ListOfTuples;
finish([H|T], ListOfTuples, Symbol) ->
	finish(T, [parse_span(H, [], [], 0, 0, Symbol)|ListOfTuples], Symbol).


%% Parses each span of two values; the name of the investor and the percentage of the investment strength
%% The open and close tags of the values are found through pattern matching
%% Each tuple is sent to jsonify before it is returned to the finish function
parse_span([], Name, Percent, _, _, Symbol) ->
	Tuple =	{Name, Percent},	
	jsonify(Tuple, Symbol);
parse_span([$<,$h,$2,$>|T], Name, Percent, _, BoldCounter, Symbol) -> 
	parse_span(T, Name, Percent, 1, BoldCounter, Symbol);
parse_span([$<,$/,$h,$2,$>|T], Name, Percent, _, BoldCounter, Symbol) ->
	parse_span(T, Name, Percent, 0, BoldCounter, Symbol);
parse_span([$<,$b,$>|T], Name, Percent, HeadCounter, _, Symbol) ->
	parse_span(T, Name, Percent, HeadCounter, 1, Symbol);
parse_span([$%,$<,$/,$b,$>|T], Name, Percent, HeadCounter, _, Symbol) ->
	parse_span(T, Name, Percent, HeadCounter, 0, Symbol);
parse_span([H|T], Name, Percent, 1, BoldCounter, Symbol) ->
	parse_span(T, Name++[H], Percent, 1, BoldCounter, Symbol);
parse_span([H|T], Name, Percent, HeadCounter, 1, Symbol) ->
	parse_span(T, Name, Percent++[H], HeadCounter, 1, Symbol);
parse_span([_|T], Name, Percent, HeadCounter, BoldCounter, Symbol) ->
	parse_span(T, Name, Percent, HeadCounter, BoldCounter, Symbol).


%% Transforms the data to JSON format
%% Also calls the date functions so that it is of proper format
jsonify({Name, Percent}, Symbol) -> 
Date = get_date(erlang:localtime()),
JSONObject = 
			[${]++[$"]++"GuruData"++[$"]++[$:]++
			[${]++
			[$"]++"Symbol"		++[$"]++[$:]++[$"]++Symbol		++[$"]++[$,]++	
			[$"]++"Date"		++[$"]++[$:]++[$"]++Date		++[$"]++[$,]++		
			[$"]++"Name"		++[$"]++[$:]++[$"]++Name		++[$"]++[$,]++
			[$"]++"Percent"		++[$"]++[$:]++[$"]++Percent		++[$"]++	
			[$}]++
		[$}],
JSONObject.

%% Transforms the date to the database format
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