%% @author: Einar Sundgren <mail@einarsundgren.se>
%% @copyright 2013 Einar Sundgren
%% @doc "Functions to connect to a specific instance of couch db
%% used in the T3 BI project by group Prosperitas."
%% @end
%% Created: 2013-10-04
%% @version 0.1


-module(couch_connection).
-compile(export_all).
-export([store/4, store/3, get_current_rev/3, 
		list_tickers/2, delete_document/4, delete_old/4]).
-define(APPLICATIONNAME, "EinarLoader").

%% @doc "Stores a Json document to the database.
%% IP: host adress of the server running couch db
%% Port: Portnumber for couch db
%% Id: Couch DB:s mandatory id. Must be unique.
%% Json: The Json document to save.
%% RETURNS: ok for any operation. Will be expanded to give error codes
%% depeding on results in a later version. 
%% @end
-spec store(list(), number(), list(), list()) -> tuple().
store(IP,Port,Id,Json) ->
    inets:start(),
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ 
    	"/prosperitas_bi/" ++ Id,
    Type = "application/json",
    Bodytext = Json,
    UA = ?APPLICATIONNAME,
    R = httpc:request(put, {Uri, [{"User-Agent", UA}], Type, Bodytext}, [], []),
 	case R of
        {ok, {{_, 200, _}, _, _}} -> 
         inets:stop(),
    	 erlang:display('saved successfully'),
    	 R;
        _ -> error
    end.

   
%% @doc
%% Bulk stores a list of Json document to the database.
%% IP: host adress of the server running couch db
%% Port: Portnumber for couch db
%% Json: A list of Json document to save.
%% RETURNS: ok for any operation. Will be expanded to give error codes
%% depeding on results in a later version. 
%% @end
-spec store(list(),number(),list()) -> tuple().
store(IP, Port, Json) when is_list(Json)->
    Objects = "{\"docs\":[" ++ merge_json(Json) ++ "]}",
    inets:start(),
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ 
    	"/prosperitas_bi/_bulk_docs",
    Type = "application/json",
    Bodytext = Objects,
    UA =? APPLICATIONNAME,
    R = httpc:request(post, {Uri, [{"User-Agent", UA}], Type, Bodytext}, [], []),

	case R of
        {ok, {{_, 200, _}, _, _}} -> 
         inets:stop(),
    	 erlang:display('saved successfully'),
    	 R;
        _ -> error
    end.


%% @doc
%% A function that merges a list of json objects into a consistent string.
%% Parameter: A list of single json objects.
%% RETURN: The list of objects merged into one string.
%% @end
-spec merge_json(list(list)) -> list().
merge_json([H|T]) when is_list(H)->
    case T of
	[] ->   H;
	_ ->	H ++ "," ++ merge_json(T)
     end;  

merge_json([]) ->   
    [].

%% @doc
%% Gets the current revision number for a couch db document.
%% IP: Hostname of the couch db server
%% Port: Portnumber of the couch db server
%% Id: Id of the document you want to get revision for.
%% RETURN: A string containing the revision number or an error message
%% Error messages are not handeled in a nice way for now but in later verisons
%% the function will return usefull things.
%% @ end
-spec get_current_rev(list(),number(),list()) -> atom().
get_current_rev(IP, Port, Id)->
    inets:start(),
    Uri = "http://" ++ IP ++ ":" ++ Port ++ "/prosperitas_bi/" ++ Id,
    UA = ?APPLICATIONNAME,
    R =  httpc:request(get, {Uri, [{"User-Agent", UA}]}, [], []),
	case R of
        {ok, {{_, 200, _}, _, _}} -> 
      		inets:stop(),
    		Json = mochijson:decode(R),
    		{_,V} = json_support:get_first_json_element(Json, "_rev"),
    		V;
        _ -> error
    end.






%% @doc
%% Lists all stock market tickers saved on the couch db in documents that 
%% contain the Json object CompanyData.
%% IP: Hostname of the couch db server
%% Port: Portnumber of the couch db server.
%% RETURN: A list of strings containing all the saved tickers.
%% @end
-spec list_tickers(list(),number()) -> list(list()).
list_tickers(IP,Port)->
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ 
    	"/prosperitas_bi/_design/support/_view/tickindsect",
	list_tickers(Uri).


%% @doc
%% Lists stock market tickers saved on the couch db in documents that 
%% contain the Json object CompanyData and where the ticker is equal to Key.
%% IP: Hostname of the couch db server
%% Port: Portnumber of the couch db server.
%% RETURN: A list of strings containing all the saved tickers.
%% @end
-spec list_tickers(list(),number(), list()) -> list().
list_tickers(IP, Port, Key)->
	Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ 
		"/prosperitas_bi/_design/support/_view/tickindsect?key=" ++ [$"] ++ Key ++ [$"],
	list_tickers(Uri).

%% @doc
%% The not exported function that handles the actual calls.
%% end
-spec list_tickers(list()) -> list().
list_tickers(Uri) ->
inets:start(),    
UA=?APPLICATIONNAME,
{ok, {{_, 200, _}, _, Body}} 
	=  httpc:request(get, {Uri, [{"User-Agent", UA}]}, [], []),
inets:stop(),
%% Let mochijson decode the body of the text.
Json = mochijson:decode(Body),
json_support:list_json_elements(Json, "value").


%% @doc
%% This function deletes a specific doc from the prosperitas couch db at 
%% the given IP.
%% Parameters:
%% IP: Ip of server as string 
%% Port: port number as integer
%% Doc_id: The id of the couch document to be deleted.
%% Doc_rev: The currrent rev of the document to be deleted.
%% RETURN: A tuple with the atom deleted and the id of the doc that was deleted.
%% in case of the document not beeing deleted the function crashes.
%% @end 
-spec delete_document(list(),number(),list(),list()) -> tuple(atom(),list()).
delete_document(IP, Port, Doc_id, Doc_rev)->
    inets:start(),
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ "/prosperitas_bi/"
     ++ Doc_id ++ "?rev=" ++ Doc_rev,
    HTTPOptions = [],
    Options = [],
    UA =? APPLICATIONNAME,
    R = httpc:request(delete, {Uri, [{"User-Agent", UA}]}, HTTPOptions, Options),
    
	case R of
        {ok, {{_, 200, _}, _, _}} -> 
    		inets:stop(),
			{deleted, Doc_id};      	
        _ -> error
    end.


%% @doc
%% ****************************************************************************
%% This function is potentially very destructive. It will not ask for 
%% verification of the deletion. The db will be cleaned.
%% ****************************************************************************
%% Deletes all documents on the prosperitas db on the given server. This is done 
%% in an innefiiceint way- 
%% Parameters:
%% IP: The ip number as a string
%% Port: The port as an int
%% RETURN: The result of inets:stop(). Will crash if deletion fails.
%% @end

delete_all_documents(IP, Port)->
        Json = list_all_id_rev(IP, Port),
        delete_all_documents(IP, Port, Json).

delete_all_documents(IP, Port,[])->
    inets:start(),
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ 
    	"/prosperitas_bi/_compact",
    HTTPOptions = [],
    Options = [],
    UA=?APPLICATIONNAME,
    R =  httpc:request(delete, {Uri, [{"User-Agent", UA}]}, HTTPOptions, Options),

	case R of
        {ok, {{_, 200, _}, _, _}} -> 
    		inets:stop();
        _ -> error
    end;


    

delete_all_documents(IP, Port, {struct, [_, _, {_ ,{array, [{struct, Value} | T ]}}]})->
    {Id,Rev} = get_id_rev(Value),
    case is_designdoc(Id) of
	false -> delete_document(IP, Port, Id, Rev),
		 delete_all_documents(IP, Port, T);
	true ->  delete_all_documents(IP, Port, T)
    end;

delete_all_documents(IP, Port, [{ struct, Value} | T])->
    {Id, Rev} = get_id_rev(Value),
    case is_designdoc(Id) of
	false -> delete_document(IP, Port, Id, Rev),
		 delete_all_documents(IP, Port, T);
	true ->  delete_all_documents(IP, Port, T)
    end.	  

%% @doc
%% ****************************************************************************
%% This function is potentially very destructive. It will not ask for 
%% verification of the deletion. The db will be cleaned.
%% ****************************************************************************
%% Deletes all documents on the prosperitas db on the given server. A more 
%% efficient version of delete_all_documents  
%% Parameters:
%% IP: The ip number as a string
%% Port: The port as an int
%% RETURN: The result of inets:stop(). Will crash if deletion fails.
%% @end
-spec bulk_delete(list(),number())-> list().
bulk_delete(IP, Port)->
        Json = list_all_id_rev(IP, Port),
        bulk_delete(IP, Port, Json, "{\"doc\":[").

bulk_delete(IP,Port,Json) ->
  inets:start(),
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ 
    	"/prosperitas_bi/_purge",
    Type = "application/json",
    Bodytext = Json,
    _HTTPOptions = [],
    _Options = [],
    UA=?APPLICATIONNAME,
    R = httpc:request(post, {Uri, [{"User-Agent", UA}], Type, Bodytext} ,[],[]),
	case R of
        {ok, {{_, 200, _}, _, _}} -> 
    		inets:stop(),
			{deleted, ok};      	
        _ -> error
    end.

bulk_delete(IP, Port, [], Acc) ->
    erlang:display("Empty list"),
    To_delete = lists:sublist(Acc, 1, length(Acc) -1 ) ++ "]}",
    bulk_delete(IP,Port,To_delete);

bulk_delete(IP, Port, _, Acc) when length(Acc) >= 2000 ->
    erlang:display("to long list"),
    To_delete = lists:sublist(Acc, 1, length(Acc) - 1 ) ++ "]}",
    bulk_delete(IP, Port, To_delete);

bulk_delete(IP, Port, {struct, [_, _, {_, {array, [{struct, Value} | T ]}}]}, Acc) ->
    {Id,Rev}=get_id_rev(Value),
    case is_designdoc(Id) of
	false -> bulk_delete(IP,Port,T,
	         Acc++"{\"_id\":\""++Id++"\",\"_rev\":\""++Rev++"\"},");
	true  -> bulk_delete(IP,Port,T,Acc)	    
    end;

bulk_delete(IP, Port, [{struct, Value} | T ],Acc) ->
    {Id,Rev}=get_id_rev(Value),
    case is_designdoc(Id) of
	false -> bulk_delete(IP, Port, T,
		 Acc ++ "{\"_id\":\"" ++ Id ++ "\",\"_rev\":\"" ++ Rev ++ "\"},");
         true -> bulk_delete(IP, Port, T, Acc)	    
    end.	  

%% @doc
%% A helper function to get tuples of Id and Rev from the mochijson parsed
%% return from the search for all Id and Rev pairs in the db.
%% Parameters: The return as formated by list_all_id_rev
%% RETURN: The tuple {Id,Rev}. Crashes if not found.
%% @end
-spec get_id_rev(list()) -> tuple().
get_id_rev([{"id", Id} | [{"key", _}, {"value", {struct, [{"rev", Rev} | _ ]}}]]) ->
    {Id, Rev}.

%% @doc
%% A helper function that checks if a document is a designdoc in couch
%% Parameter: Id of the doc as a string
%% RETURN: the atom true or false
%% @end
-spec is_designdoc(list()) -> atom().
is_designdoc(Id)->
    {ok, MP2} = re:compile("_design?", []),
     case re : run(Id, MP2) of
	 {match,_} -> true;
		 _-> false
     end.
%% @doc
%% Lists all Id:s paired with the current rev of the documents at the 
%% prosperitas_bi db at the given Ip/port. 
%% Parameters:
%% IP: Ip number as a string
%% Port: Port number as an int 
%% RETURN: A mochi json decoded tuple of the return.
%% @end
-spec list_all_id_rev(list(),number())->tuple().
list_all_id_rev(IP, Port)->
    inets:start(),
    Uri = "http://"++ IP ++ ":" ++ integer_to_list(Port) ++ "/prosperitas_bi/_all_docs",
    UA =? APPLICATIONNAME,
    R = httpc:request(get, {Uri, [{"User-Agent", UA}]}, [], []),

	case R of
        {ok, {{_, 200, _}, _, _}} -> 
	    	inets:stop(),
    		mochijson:decode(R),
			{deleted, ok};      	
        _ -> error
    end.




%% @doc
%% Deletes all documents with the subelement CompanyData which is used for
%% the tickers. After the function has updated the documents with 
%% a _delete=true it also runs a compatioin session.
%% Parameters:
%% IP: The ip of the server as a string.
%% Port: The portnumber as an int.
%% RETURN:
%% The inets:stop status report. If something goes wrong the function crashes.
%% @end
-spec delete_tickers(list(),number())->tuple().
delete_tickers(IP, Port)->
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port)
            ++"/prosperitas_bi/_design/support/_view/tickers",
    delete_tickers(IP, Port, Uri, re).

delete_tickers(IP, Port, Uri, re) ->
inets:start(),
    UA=?APPLICATIONNAME,
    R = httpc:request(get, {Uri, [{"User-Agent", UA}]}, [], []),
    
    case R of
        {ok, {{_, 200, _}, _, _}} -> 
		 inets:stop(),
    	{struct,[{"total_rows", _}, {"offset", _}, {"rows",
    	{array, Json}}]} = mochijson:decode(R),
    	Objects = proplists:get_all_values(struct, Json),
    	DeleteString = extract_id_rev(Objects),

	    %% Store the documents as deleted. Thats how couch delets things.
    	store(IP, Port, DeleteString),

	    %% Run a compacting session as well to make sure it has throughput.
    	inets:start(),
    	CompactUri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) 
        	    ++ "/prosperitas_bi/_compact",
    	Type = "application/json",
    	Bodytext = "",
    	UA =? APPLICATIONNAME,
    	
    	httpc:request(post, {CompactUri, 
            [{"User-Agent", UA}], Type, Bodytext}, [], []),
    	inets:stop(), 

		{deleted, ok};      	
        _ -> error
    end.


%% @doc
%% Deletes the old stock values. This is targeted to a recurring cleanout of 
%% the db at a given interval. 
%% Parameters:
%% IP: The servers IP as a string
%% Port: The port as an int
%% Startkey: A string in the format "[yyyy-mm-dd]" denoting the date on the 
%% stockdata from when the removal should start
%% Endkey: Astring in the same format as startkey but denoting the last date 
%% to be removed
%% RETURN: The status from inet:stop. Crashes if the removal fails.
%% @end

-spec delete_old(list(),number(),list(),list()) -> atom().
delete_old(IP, Port, Startkey, Endkey)->
    Uri = "http://" ++ IP ++ ":" ++ integer_to_list(Port)
        ++"/prosperitas_bi/_design/support/_view/stockbydate?startkey=\"" ++ 
        	Startkey ++ "\"&endkey=\"" ++ Endkey ++ "\"",
    delete_old(IP, Port, Uri, Startkey, Endkey, re).

delete_old(IP, Port, Uri, _, _, re) ->
inets:start(),
    UA=?APPLICATIONNAME,
    {ok, {{_, 200, _}, _, Body}}
    =  httpc:request(get, {Uri, [{"User-Agent", UA}]}, [], []),
    inets:stop(),
    {struct,[{"total_rows",_},{"offset",_},{"rows",
    {array,Json}}]} 
     = mochijson:decode(Body),
    Objects = proplists:get_all_values(struct,Json),
    DeleteString = extract_id_rev(Objects),
    inets:stop(),

    %% Update all documents with _delete = true. 
    R = store (IP, Port, DeleteString),
    %% Run a compacting session as well to make sure it has throughput.
    inets:start(),
    CompactUri = "http://" ++ IP ++ ":" ++ integer_to_list(Port) 
                ++ "/prosperitas_bi/_compact",
    Type = "application/json",
    Bodytext = "",
    UA = ?APPLICATIONNAME,
    R =  httpc:request(post, {CompactUri, [{"User-Agent", UA}], 
    		Type, Bodytext},[],[]),
    inets:stop().

%% @doc
%% Extracts the id and rev of the documents from a list of json elements in a given format
%% The format is the one returned from the couch db view stockbydate. 
%% Parameters: 
%% Objects: A mochi json decoded tuple form the return of the stockbydate view
%% RETURN: a list of json objects in the form 
%% {"_id":"ID OF DOC","_rev":"REV OF DOC","_deleted": true}. This is ready to 
%% Merge and send to couch db for deletion.
%% @end
-spec extract_id_rev(list()) -> list().
extract_id_rev(Objects)->
    extract_id_rev(Objects,[]).
extract_id_rev ([],Acc)->
    Acc;
extract_id_rev([[{"id", Id}, {"key", _Key}, {"value", Rev}] | T], Acc) ->
     Add = Acc ++ ["{\"_id\":\"" ++ Id ++ "\",\"_rev\":\"" ++ Rev 
                ++ "\",\"_deleted\": true}"],
    extract_id_rev (T, Add).