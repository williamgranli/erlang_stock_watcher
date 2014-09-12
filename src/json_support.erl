%% Author: Einar Sundgren
%% Description: Functions to support the transition from mochjson to 
%% a format that can be save to the db.
%% used in the T3 BI project by group Prosperitas.
%% Created: 2013-10-04
%% Version 0.1

-module(json_support).
-export([get_first_json_element/2,get_nth_json_element/3,list_json_elements/2]).

-define(TEST, {struct,[{"total_rows",2734},{"offset",0},{"rows",{array,[{struct,[{"id","AAIT"},{"key","CompanyData"},{"value",{array,["AAIT","n/a","n/a"]}}]},{struct,[{"id","AAME"},{"key","CompanyData"},{"value",{array,["AAME","Life Insurance","Finance"]}}]},{struct,[{"id","AAOI"},{"key","CompanyData"},{"value",{array,["AAOI","n/a","n/a"]}}]},{struct,[{"id","AAON"},{"key","CompanyData"},{"value",{array,["AAON","CapitalGoods","n/a"]}}]},{struct,[{"id","AAPL"},{"key","CompanyData"},{"value",{array,["AAPL","Computer Manufacturing","Technology"]}}]},{struct,[{"id","AAWW"},{"key","CompanyData"},{"value",{array,["AAWW","Services","Transportation"]}}]},{struct,[{"id","AAXJ"},{"key","CompanyData"},{"value",{array,["AAXJ","n/a","n/a"]}}]}]}}]} ).

%% @doc
%% Gets the first JSON element from an 
%% MochiJson structure of the type supplied by Key from any expresion.
%% @end
-spec get_first_json_element (tuple(atom(), list()), list()) -> list().
get_first_json_element({struct,SubExp},Key)->
    get_first_json_element(SubExp,Key);
get_first_json_element([{K,V}|T],Key) when Key == K->   
    {K,V};
get_first_json_element({K,V},Key) when Key == K->
    {K,V};
get_first_json_element({_,SubExp},Key) ->
    get_first_json_element(SubExp,Key);
%% This one is weird. For some reason it considers a tuple a list in some cases.
get_first_json_element([H|T],Key) when T==[]->
    get_first_json_element(H,Key);
get_first_json_element([_|T],Key)->
    get_first_json_element(T,Key).


%% @doc
%% Gets the first JSON element from an 
%% MochiJson structure of the type supplied by Key from any sub k/v expresion.
%% @end
-spec get_nth_json_element(list(), list(), number()) -> list().
get_nth_json_element(Json,Key,Num)->
    get_nth_json_element(Json,Key,Num,0).
get_nth_json_element({struct,SubExp},Key,Num,Count)->
    get_nth_json_element(SubExp,Key,Num,Count);
get_nth_json_element([{K,V}|T],Key,Num,Count) when Key == K->   
    {K,V};
get_nth_json_element({K,V},Key,Num,Count) when Key == K->
    {K,V};
get_nth_json_element({_,SubExp},Key,Num,Count) ->
    get_nth_json_element(SubExp,Key,Num,Count);
%% This one is weird. For some reason it considers a tuple a list in some cases.
get_nth_json_element([H|T],Key,Num,Count) when T==[]->
    get_nth_json_element(H,Key,Num,Count);
get_nth_json_element([_|T],Key,Num,Count)->
    get_nth_json_element(T,Key,Num,Count).


%% @doc
%% Lists all json elements of type  
%% MochiJson structure of the type supplied by Key from any sub k/v expresion.
%% @end   
list_json_elements()->
    list_json_elements(?TEST,"value").
%list_json_elements({struct,SubExp},Key)->
%   list_json_elements(SubExp,Key,[]);
list_json_elements(SubExp,Key)->
    %erlang:display(Key),
   list_json_elements(SubExp,Key,[]).

list_json_elements({struct,SubExp},Key,Acc) when Key /= struct->
    list_json_elements(SubExp,Key,Acc);

list_json_elements([{K,V}|T],Key,Acc) when Key == K->
%erlang:display("Found key"),
    Retur = case Acc of
	    []->[V];
	    L ->[Acc,V]
	end,
    list_json_elements(V,Key,Retur);
list_json_elements([{K,V}|T],Key,Acc)->
    L = list_json_elements(T,Key,Acc), 
    R = list_json_elements({K,V},Key,Acc),
    Lret= case L of
	       {array,LValue}-> 
		  [LValue];
	      [{array,LValue}]-> 
		  [LValue];
	       _ -> 
		  L  
     end,
    Rret = case R of
	    {array,RValue}-> 
		[RValue];
	    [{array,RValue}]-> 
		[RValue];
	       _ ->  R
     end,
    Lret++Rret;
list_json_elements({K,V},Key,Acc) when Key == K->
erlang:display("Found key"),
    list_json_elements(V,K,Acc);
list_json_elements({K,SubExp},Key,Acc) ->
    list_json_elements(SubExp,Key,Acc);
list_json_elements([H|T],Key,Acc) when T==[]->
    list_json_elements(H,Key,Acc);
%% Grabs emptylist. 
list_json_elements([],Key,Acc)->
    Acc;
%% Grabs it all. The endstation for unknown values
list_json_elements(Value,Key,Acc)->
    Acc.
