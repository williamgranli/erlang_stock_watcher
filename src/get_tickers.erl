%% @author William Granli
%% @doc 
%% 	This module reads the source of a .csv file (http://www.nasdaq.com/symbol/ -> Download Security List)
%% 	The data from the .csv is extracted, line per line and then transformed into JSON format. The JSON string is returned. 
%% @end


-module(get_tickers).
-export([read_file/1]).


% sends a http request to the location of the .csv file at the website
read_file(Market) ->
	inets:start(),
	CSV = httpc:request(get, {"http://www.nasdaq.com/screening/companies-by-name.aspx?exchange=" ++ Market ++ "&render=download",[]},[],[]),
	inets:stop(),
	
	case CSV of
		{ok,{_,_,X}} -> go_through_lines(X, Market);
		_ -> error %catches any http error
	end.

%% @doc
%% goes through each line in the "file" and then calls the formatter functions extract_value and jsonify.  
%% when the line has been formatted to a json object this function puts it in a accumulator list and returns it
go_through_lines(AllLines, Market) -> 
	go_through_lines(AllLines, [], [], Market).
go_through_lines([], _Line, ListOfJSON, _Market) ->
	ListOfJSON;
go_through_lines([$,, $\r, $\n | T ], Line, ListOfJSON, Market) ->
	go_through_lines(T, [], [extract_values(Line, 0, [], [], [],[], Market)|ListOfJSON], Market);
go_through_lines([$\n | T ], Line, ListOfJSON, Market) ->
	go_through_lines(T, [], [extract_values(Line, 0, [], [], [],[], Market)|ListOfJSON], Market);
go_through_lines([H|T], Line, ListOfJSON, Market) ->
	go_through_lines(T, Line++[H], ListOfJSON, Market).

%% @doc
%% extracts the Ticker, Sector and Industry from each line and sends it to jsonify
extract_values([], _CommaCounter, Ticker, Name, Sector, Industry, Market) ->
	jsonify(Market, Ticker, Name, Sector, Industry);
extract_values([$,|T], CommaCounter, Ticker, Name, Sector, Industry, Market) ->
	extract_values(T, CommaCounter+1, Ticker, Name, Sector, Industry, Market);
extract_values([H|T], 0, Ticker, Name, Sector, Industry, Market) ->
	case H of 
		$" -> extract_values(T, 0, Ticker, Name, Sector, Industry, Market);
		_ -> extract_values(T, 0, Ticker++[H], Name, Sector, Industry, Market)
	end;

extract_values([H|T], 1, Ticker, Name, Sector, Industry, Market) ->
	case H of 
		$" -> extract_values(T, 1, Ticker, Name, Sector, Industry, Market);
		_ -> extract_values(T, 1, Ticker, Name ++ [H], Sector, Industry, Market)
	end;

extract_values([H|T], 6, Ticker, Name, Sector, Industry, Market) ->
	case H of 
		$" -> extract_values(T, 6, Ticker, Name, Sector, Industry, Market);
		_ -> extract_values(T, 6, Ticker, Name, Sector++[H], Industry, Market)
	end;
extract_values([H|T], 7, Ticker, Name, Sector, Industry, Market) ->
	case H of 
		$" -> extract_values(T, 7, Ticker, Name, Sector, Industry, Market);
		_ -> extract_values(T, 7, Ticker, Name, Sector, Industry++[H], Market)
	end;
extract_values([_H|T], CommaCounter, Ticker, Name, Sector, Industry, Market) ->
	extract_values(T, CommaCounter, Ticker, Name, Sector, Industry, Market).

%% @doc
%% makes a json object of the 3 values and returns it
jsonify(Market, Ticker, Name, Sector, Industry) -> 
	JSONObject = 
			[${]++[$"]++"CompanyData"++[$"]++[$:]++
			[${]++
			[$"]++"Market"			++[$"]++[$:]++[$"]++string:strip(Market,both)			++[$"]++[$,]++
			[$"]++"Name"			++[$"]++[$:]++[$"]++string:strip(Name,both)				++[$"]++[$,]++
			[$"]++"Symbol"			++[$"]++[$:]++[$"]++string:strip(Ticker,both)			++[$"]++[$,]++
			[$"]++"Sector"			++[$"]++[$:]++[$"]++string:strip(Sector,both)			++[$"]++[$,]++
			[$"]++"Industry"		++[$"]++[$:]++[$"]++string:strip(Industry,both)			++[$"]++
			[$}]++
			[$}],
	JSONObject.