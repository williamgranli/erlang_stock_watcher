%% @author: Einar Sundgren
%% @doc
%% Description: Functions to download and parse information from the
%% Google finance API and output them as json-objects.
%% used in the T3 BI project by group Prosperitas.
%% @end
%% Created: 2013-10-04
%% @version 0.2
%% @end
-module(google_parse).
-compile(export_all).
-export([parse/2,parse_google_date/1]).
-define(IP2DB, "95.80.9.66").
%% @doc
%% Stock: A list containing comma separated marketplace and ticker in the
%% format NASDAQ:GOOG,NASDAQ:YHOO,NASDAQ:AAPL no spaces and no ending commas.
%% Maximum length of that string is not know. The string is used to form a 
%% request to Googles servers and should according to http-standards be able
%% to handle any length string. The reality is likely that we should limit
%% ourselves to strings less than 2 000 characters.
%% RETURN: A list of Json strings ready to save to a couch db. 
%% The time parser changes every time to 24hr format and UTC. It is only
%% adjusted for opening hours of NASDAQ,NYSE,AMEX and OMXNordic.   
%% @end
-spec parse(list(),list())->list(list()).   
parse(Stock,_Ticker_info)->
    URI = "http://finance.google.com/finance/info?client=ig&q=" ++ Stock,
    inets:start(),
    Answer = httpc:request(get, {URI, []}, [], []),
    inets:stop(),
    
    case Answer of
        {ok,{{_,200,_},_,Body}} -> parse2(Body);
        _ -> error
    end.
    
parse2(Body) ->
    JsonText = string:substr(Body,5,string:len(Body)-5),
    {_,List} = mochijson:decode(JsonText),
    Objects = proplists:get_all_values(struct,List),
    make_json_output(Objects,[],couch_connection:list_tickers(?IP2DB,5984)).

%% @doc
%% creates a list of json elements from several provided sources.
%% Parameters:
%% [H|T]: a list of json objects that expects a list of tuples where the first 
%% element was the atom struct as returned by mochiJson parser
%% Acc: An accumulator list of the output strings.
%% TickIndSect: The ticker, industry and sector to be merged into each output object
%% @end
-spec make_json_output(list(),list(),list()) -> list(list()).
make_json_output([H|T],Acc,TickIndSect)->
		     %% Some ugly defensive programming.
	           Object = (catch create_single_object(H,match_ticker(find_value("t",H),TickIndSect))),
		      %% Handle potential errors
              if
                is_list(Object) ->
			         Acc ++ Object ++ make_json_output(T,Acc,TickIndSect);
			     true -> 
                     Acc ++ make_json_output(T,Acc,TickIndSect)
		       end;
make_json_output([],Acc,_) ->
    Acc.


%% @doc
%% InputObject: A single Json-object in the mochijson format.
%% RETURN: A list of jsonobjects in the form that each should be possible to 
%% directly save to prospertas_bi on couch db
%% @end
-spec create_single_object(list(),tuple(list(),list()))->list().
create_single_object(InputObject,{Sector,Industry})->
    Symbol = find_value("t",InputObject), 
    Name = "n/a",
    Date = parse_google_date(find_value("lt",InputObject)),
    Time = parse_google_time(find_value("ltt",InputObject)),
    CurrentPrice = find_value("l_cur",InputObject),
    OpeningPrice = "n/a",
    HighestPrice = "n/a",
    LowestPrice = "n/a",
    Volume = "n/a",	
    NewsString = "n/a",

    ["{\"StockData\":{" ++
      "\"Symbol\":\"" ++ Symbol ++ "\","
     "\"Name\":\"" ++ Name ++ "\","
     "\"Date\":\"" ++ Date ++ "\","
     "\"Time\":\"" ++ Time ++ "\","
     "\"CurrentPrice\":\"" ++ CurrentPrice ++ "\","
     "\"OpeningPrice\":\"" ++ OpeningPrice ++ "\","
     "\"HighestPrice\":\"" ++ HighestPrice ++ "\","
     "\"LowestPrice\":\"" ++ LowestPrice ++ "\","
     "\"Industry\":\"" ++ Industry ++ "\","
     "\"Sector\":\"" ++ Sector ++ "\","
     "\"Volume\":\"" ++ Volume ++ "\","
     "\"NewsString\":\"" ++ NewsString ++ "\""
     ++ "}}"].


%% @doc
%% Find first occasion of a key-value pair in a mochijson structure- 	
%% Initial state. A list of twotuples.
%% Parameters:
%% Key: Obviously the key you are looking for
%% List of tuples with key values pairs
%% RETURN:
%% The value if found. Else the atom fail
%% @end
-spec find_value(any(),list(tuple()))->any().
find_value(Key,[{K,V}|_]) when Key == K ->
    V;
find_value(Key,[{_,_}|T]) ->
    case T of
	[] -> fail;
	_ -> find_value(Key,T)
    end;
find_value(_,[]) ->
    fail.

%% @doc
%% Parses a date in the form Google finance supplies and returns a
%% ISO8601 date without time.
%% Date: The date supplied by google as a string.
%% RETURN: A string in ISO8601 YYYY-MM-DD
%% @end
-spec parse_google_date(list())->list().
parse_google_date(Date)->
    {{Year,_,_},{_,_,_}} = calendar:local_time(),
    Year_string = integer_to_list(Year),
    Day_string = string:strip(string:sub_word(Date,2),both,$,),
    Parsed_day = case string:len(Day_string) of
		     1->"0" ++ Day_string;
		     _-> Day_string
		 end,

    Month_string = case string:sub_word(Date,1) of
		       "Jan"->01;
		       "Feb"->02;
		       "Mar"->03;
		       "Apr"->04;
		       "May"->05;
		       "Jun"->06;
		       "Jul"->07;
		       "Aug"->08;
		       "Sep"->09;
		       "Oct"->10;
		       "Nov"->11;
		       "Dec"->12;
		       _ -> erlang:error(bad_data_error)			    			    
	      	  end,
    Parsed_month = integer_to_list(Month_string),
    Year_string++"-"++Parsed_month++"-"++Parsed_day.

%% doc@
%% Parses the time as supplied by Google finance to a 24hr clock
%% string in UTC with one minute precision.
%% Time: Time as supplied by Google finance.
%% RETURNS: A string in HH:MM, adjusted to UTC and 24 hr clock.
%% @end
-spec parse_google_time(list())->list().
parse_google_time(Time)->
    Hour_string =  string:sub_word(Time,1,$:),
    Minute_string = string:sub_string(string:sub_word(Time,2,$:),1,2),
    AMPM_string = string:sub_string(string:sub_word(Time,2,$:),3,4),
    Tz_string = string:sub_string(string:sub_word(Time,2,$:),6),
    
    Adjusted_hour = time_zone_to_utc(ampm_to_24(list_to_integer(Hour_string),
				AMPM_string),Tz_string),
    integer_to_list(Adjusted_hour)++":"++Minute_string++":00".

-spec time_zone_to_utc(number(),list())->number().
time_zone_to_utc(Hour,Tz) ->
    case Tz of 
	"EDT"->Hour+4;
	"EST"->Hour+5;
	"GMT+2"->Hour-2
    end.

%% @doc
%% Changes 12 hour time to 24 hour. Crashes if hour is larger than 12 or 
%% smaller than 0.
%% Hour: The hour of the day in 12 hr format.
%% AMPM: If its AM or PM
%% RETURNS: The current 24 hr equivalent
%% @end
-spec ampm_to_24(number(),list())->number().
ampm_to_24(Hour,AMPM) when (AMPM == "AM") and (Hour =<12) and (Hour >0) ->
    Hour;
ampm_to_24(Hour,AMPM) when (AMPM == "PM") and (Hour =<12) and (Hour >0 ) ->
    Hour + 12.

%% @doc
%% Searches the downloaded list of tickers for any match of the current
%% Returns the tuple {Sector, Industry} if it finds it.
%% Crashes if it doesn't find anything.
%% @end
-spec match_ticker(list(),list(list()))->list().
match_ticker(Symbol, [[Symbol|SecInd]|_]) ->
    [Sector|[Industry|[]]] = SecInd,
    {Sector,Industry};
match_ticker(Symbol,[_|T])->
    match_ticker(Symbol,T).