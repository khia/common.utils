-module(common.utils.lists).

-import(lists).
-import(proplists).
-export([errors/1, reorder_key/3, keys/2, to_record/3, to_record/4, 
	 search_keys/3]).
 
-type(id() :: atom()).
-type(error() :: {error, term()}).
-spec(errors/1 :: 
      ([{id(), true | false}]) -> [id()];
      ([{id(), error()}]) -> [{id(), error()}]).
errors(Errors) ->
    errors(Errors, []).
errors([], Res) -> Res;
errors([{Id, true} | Rest], Res) -> errors(Rest, Res);
errors([{Id, false} | Rest], Res) -> errors(Rest, [Id] ++ Res);
errors([{Id, {error, _R}} = Err | Rest], Res) -> errors(Rest, [Err] ++ Res);
%% to prevent crash on functions which return errors in unsupported form
errors([{Id, Reason} = Err | Rest], Res) -> 
    errors(Rest, [{Id, {error, Reason}}] ++ Res).

-type(reason() :: term()).

%% FIXME
%% Function crashes if elements are not tuples
-spec(reorder_key/3 :: 
      (Order :: [term()], Idx :: integer(), %% 1..tuple_size(Tuple)
       List :: [Tuple :: tuple()]) -> {ok, Res :: list()} | {error, reason()}).  
reorder_key(Order, Idx, List) ->
    reorder_key(Order, Idx, List, []).
reorder_key(Order, Idx, List, Options) ->
    reorder_key(Order, Idx, List, Options, [], []).
reorder_key([], _Idx, _List, Options, Res, [])->
    Order = proplists:get_value(order, Options, obverse),
    case Order of
	obverse-> {ok, lists:reverse(Res)};
	reverse-> {ok, Res}
    end;
reorder_key([], _Idx, _List, _Options, _Res, Errors) -> {error, Errors};
reorder_key([Current | Rest], Idx, List, Options, Res, Errors)->
    {New_Res, New_Errors} = case search_keys(Current, Idx, List) of
				[]-> {Res, Errors};
				Matched-> {Matched ++ Res, Errors}
			    end,
    reorder_key(Rest, Idx, List, Options, New_Res, New_Errors). 

keys(Idx, List) -> [element(Idx, E) || E <- List].

search_keys(Key, Idx, List) ->
    search_keys(Key, Idx, List, []).
search_keys(Key, Idx, [], Res) -> Res;
search_keys(Key, Idx, [Element | Rest], Res0) ->
    Res1 = case (element(Idx, Element) =:= Key) of
	    true -> [Element] ++ Res0;
	    false -> Res0
	end,    
    search_keys(Key, Idx, Rest, Res1).
    
    

%% bad formatted tuples and atoms will be ignored 
to_record(Name, List, RI) ->
    to_record(Name, List, RI, undefined).

to_record(Name, List, RI, Default) ->
    to_record(Name, List, lists:reverse(RI), Default, []).

to_record(Name, _List, [], Default, Res) -> list_to_tuple([Name] ++ Res);
to_record(Name, List0, [Field | Rest], Default, Res0) ->
    {Res1, List1} = case lists:keytake(Field, 1, List0) of
	    {value, Tuple, TupleList} -> 
			    {[element(2, Tuple)] ++ Res0, TupleList};
	    false -> {[Default] ++ Res0, List0}
	end,
    to_record(Name, List0, Rest, Default, Res1).

    
