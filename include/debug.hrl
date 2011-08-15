

%% The assert macro is written the way it is so as not to cause warnings
%% for clauses that cannot match, even if the expression is a constant.
%% macros was taken from eunit
%% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson
-undef(assert).
-define(assert(BoolExpr),
 	((fun () ->
 	    case (BoolExpr) of
 		true -> ok;
 		__V -> .erlang:error({assertion_failed,
 				      [{module, ?MODULE},
 				       {line, ?LINE},
 				       {expression, (??BoolExpr)},
 				       {expected, true},
 				       {value, case __V of false -> __V;
 						   _ -> {not_a_boolean,__V}
 					       end}]})
 	    end
 	  end)())).
%% -define(assert(BoolExpr),
%% 	(case (BoolExpr) of
%% 		true -> ok;
%% 		__V -> .erlang:error({assertion_failed,
%% 				      [{module, ?MODULE},
%% 				       {line, ?LINE},
%% 				       {expression, (??BoolExpr)},
%% 				       {expected, true},
%% 				       {value, case __V of false -> __V;
%% 						   _ -> {not_a_boolean,__V}
%% 					       end}]})
%% 	    end
%% 	 )).
%%-endif.
-define(_test(Expr), {?LINE, fun () -> (Expr) end}).
-define(_assert(BoolExpr), ?_test(?assert(BoolExpr))).

%% TODO Find more clever way
%% ?debug("About to invoke ~p:private(~p, ~p)", [?M, Fun, Args], invoke),
-ifdef(DEBUG).
-define(debug(Format, Args, Fun), 
	.io:format("~p::~p::~p:~p:" ++ Format ++ "~n", 
		   [?MODULE, Fun, ?LINE, self()] ++ Args)).
-else.
-define(debug(_Format, Args, _Fun), Args).
-endif.

-ifdef(DEBUG).
-define(warning(Format, Args, Fun), 
	.io:format("~p::~p::~p:~p:WARNING:" ++ Format ++ "~n", 
		   [?MODULE, Fun, ?LINE, self()] ++ Args)).
-else.
-define(warning(_Format, Args, _Fun), Args).
-endif.

-ifdef(DEBUG).
-define(error(Format, Args, Fun), 
	.io:format("~p::~p::~p:~p:ERROR:" ++ Format ++ "~n", [?MODULE, Fun, ?LINE, self()] ++ Args)).
-else.
-define(error(_Format, Args, _Fun), Args).
-endif.

