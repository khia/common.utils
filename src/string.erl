-module(common.utils.string).

-import(common.utils.ustring).
-import(string).
-import(io_lib).
-import(lists).
-import(erl_scan).
-import(erl_parse).
-import(erl_eval).

-include("ustring.hrl").
-export([tokens/2,
	 left/2,
	 join/2,
	 to_integer/1,
	 to_integer/2,
	 to_list/1,
	 to_list/2,
	 from_list/1,
	 from_list/3,
	 to_ref/1
	]).

tokens(String, Delimiters) when ?is_ustring(String) ->
    ustring:tokens(String, Delimiters);
tokens(String, Delimiters) ->
    string:tokens(String, Delimiters).

left(String, Integer) when ?is_ustring(String) ->
    ustring:left(String, Integer);
left(String, Integer) ->
    string:left(String, Integer).

join([], _Separator) -> [];
join(StringList, Separator) when hd(?is_ustring(StringList)) ->
    ustring:join(StringList, Separator);    
join(StringList, Separator) ->
    string:join(StringList, Separator).

from_list(List_Of_Integers) ->
    from_list(16, 2, List_Of_Integers).

from_list(Radix, Max, List_Of_Integers) ->
    Format = lists:flatten(io_lib:format("~~~p.~p.0B" ,[Max, Radix])),
    from_list(Radix, Format, List_Of_Integers, []).

from_list(_Radix, _Format, [], Res) -> lists:flatten(lists:reverse(Res));
from_list(Radix, Format, [Current | Rest], Res) ->
    from_list(Radix, Format, Rest, [io_lib:format(Format, [Current])] ++ Res).

to_list(String) -> to_list(16, String).

to_list(Radix, String) -> 
    to_list(Radix, String, []).

to_list(_Radix, [], Res) -> lists:reverse(Res);
to_list(Radix, [L, R | Rest], Res) ->
    to_list(Radix, Rest, [to_integer(Radix, [L] ++ [R])] ++ Res).

to_integer(String) -> to_integer(10, String).

%% TODO for ustring
to_integer(Radix, String) when is_list(String) ->
    to_integer(Radix, lists:reverse(String), 1, 0);

%% char
to_integer(Radix, Char) 
  when is_integer(Radix) andalso 2 =< Radix, Radix =< 36 ->
    case Char - $0 of
	Small when 0 =< Small, Radix >= Small -> Small;
	_ ->
	    case Char - $A + 10 of
		Int when Int >= Radix -> {error, {out_of_range, Char}};
		Int -> Int
	    end
    end;   
to_integer(Radix, _String) -> {error, {unsupported_radix, Radix}}.

to_integer(_Radix, [], _Power, Res) -> Res;
to_integer(Radix, [C | R], Power, Acc) -> 
    to_integer(Radix, R, Power * Radix, to_integer(Radix, C) * Power + Acc).


to_ref(String) ->
    case erl_scan:string(String ++ ".") of
	{ok, Tokens, _Line} -> 
	    case erl_parse:parse_exprs(Tokens) of
		{ok, Exprs} -> 
		    case erl_eval:exprs(Exprs, []) of
			{value, Ref, []} when is_function(Ref) -> Ref;
			{value, _Ref, []} -> {error, not_function};
			{value, _Ref, Warnings} -> {error, Warnings};
			Eval_Error -> {error, {eval, Eval_Error}}
		    end;
		Parse_Error -> {error, {parse, Parse_Error}}
	    end;
	Scan_Error -> {error, {scan, Scan_Error}}
    end.
