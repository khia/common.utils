-module(common.utils.ustring).
-include("debug.hrl").
-include("ustring.hrl").
-export([
		 new/1, new/2,
%%		 centre/2, centre/3,
%%		 chars/2, chars/3,
%%		 chr/2,
		 concat/2,
		 copies/2,
%%		 cspan/2,
		 equal/2,
		 join/2,
%%		 left/2, left/3,
		 len/1,
%%		 rchr/2,
%%		 right/2, right/3,
%%		 rstr/2,
%%		 span/2,
%%		 str/2,
%%		 strip/1, strip/2, strip/3,
%%		 sub_string/2, sub_string/3,
%%		 sub_word/2, sub_word/3,
%%		 substr/2, substr/3,       
%%		 to_float/1,
%%		 to_integer/1,
%%		 to_lower/1,
%%		 to_upper/1,
%%		 tokens/2,       
%%		 words/1, words/2,
		 test/0, test/1, test/2, test/3
]).

%%-define(is_upper(C), C >= $A, C =< $Z).

new(String) when is_list(String) ->
    new(String, 'ISO-8859-1').

new(String, 'UTF-8') -> 
    %% FIXME
    #ustring{len = len(String), text = list_to_binary(String)};
new(String, Codepage) -> %%when is_list(String) ->
    UTF8 = convert(String, Codepage, 'UTF-8'),
    new(UTF8, 'UTF-8').

format(Format, Args) -> ok.

%% TODO
len(String) when is_list(String) -> length(String);
len(UString) when ?is_ustring(UString) -> UString#ustring.len.	

-define(len(UString), UString#ustring.len).
-define(text(UString), UString#ustring.text).

concat(UStr1, UStr2) when ?is_ustring(UStr1) andalso ?is_ustring(UStr2) ->
    Len = ?len(UStr1) +?len(UStr2),
    Bin1 = ?text(UStr1),
    Bin2 = ?text(UStr2),
    Text = <<Bin1/binary, Bin2/binary>>,
    UStr1#ustring{len = Len, text = Text}. 

equal(UStr1, UStr2) when ?is_ustring(UStr1) andalso ?is_ustring(UStr2) 
    andalso ?len(UStr1) =/= ?len(UStr2) -> false;
equal(UStr1, UStr2) when ?is_ustring(UStr1) andalso ?is_ustring(UStr2) 
    andalso ?text(UStr1) =:= ?text(UStr2) -> true;
equal(_UStr1, _UStr2) -> false.

join(StringList, Separator) when is_list(StringList) ->
    join(StringList, Separator, new("")).

join([], _Separator, Res) -> Res;
join([Current | []], Separator, Res0) ->    
    Res1 = concat(Res0, Current),
    join([], Separator, Res1);
join([Current | Rest], Separator, Res0) ->    
    CS = concat(Current, Separator),
    Res1 = concat(Res0, CS),
    join(Rest, Separator, Res1).

copies(String, Number) -> 
    copies(String, Number, new("")).

copies(_String, 0, Res) -> Res;
copies(String, Number, Res) ->
    copies(String, Number - 1, concat(Res, String)).
    

%% TODO
convert(String, From, To) ->
	String.

%% To get help try 
%% 1> test(usage).
test() ->
    test(run).
test(Action) ->
    test(Action, all).
test(Action, Test_Id) ->
    utes:test(Action, ?MODULE, Test_Id).

test(all, suite, _Config) ->
    [len, concat, equal, join, copies];
test(all, init, Config) ->
    NewConfig=[
	       %% ========== place own parameters in this list =========
	       %% Example: {param, Param}
	       %% ======================================================
	      ],
    lists:append(NewConfig, Config);
test(all, free, _Config) ->
    ok;

test(len = Fun, doc, _Config) -> 
    lists:flatten(io_lib:format("Testing ~p()",[Fun]));
%%test(match_syntax = _Fun, suite, _Config) ->
%%    [match_syntax_simple_if];
test(len = _Fun, run, Config) ->
    ?assert(5 =:= len(new("12345"))),
    ?assert(7 =:= len(new("1234567"))),
    ?assert(0 =:= len(new(""))),
    ok;
test(concat = _Fun, run, Config) ->
    ?assert(new("12345") =:= concat(new("123"), new("45"))),
    ok;
test(equal = _Fun, run, Config) ->
    ?assert(true =:= equal(new("123"), new("123"))),
    ?assert(true =:= equal(new(""), new(""))),
    ?assert(false =:= equal(new("123"), new("321"))),
    ?assert(false =:= equal(new("123"), new("1234"))),
    ok;
test(join = _Fun, run, Config) ->
    ?assert(new("123, 45") =:= join([new("123"), new("45")], new(", "))),
    ?assert(new("123") =:= join([new("123")], new(", "))),
    ok;
test(copies = _Fun, run, Config) ->
    ?assert(new("123123123") =:= copies(new("123"), 3)),
    ?assert(new("123") =:= copies(new("123"), 1)),
    ?assert(new("") =:= copies(new("123"), 0)),
    ok;


test(_, _, _) -> unknown.

	
 
%% utf8points([], L) -> lists:reverse(L);
%% utf8points([H | T], L) ->
%% 	case <<H>> of
%% 	     <<2#110:3, _:5>> -> decode2([H | T], L);	     
%% 	     <<2#1100:4, _:4>> -> decode3([H | T], L);
%% 	     <<2#11000:5, _:3>> -> decode4([H | T], L);
%% 	     <<0:1, _:7>> utf8points(T, [H | L]);
%% 	     _ -> exit({decode_error, {bad_bytes, [H]}})
%% 	end.
    
