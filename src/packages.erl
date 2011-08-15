-module(common.utils.packages).

-import(filename).
-import(lists).
-import(code).
-import(filename).

-export([get_module_name/1, get_module_name/2, 
	 priv_dir/1, app_dir/1, ebin_dir/1, package/1]).

-spec(get_module_name/1 :: (string()) -> {ok, atom()} | {error, any()}).
get_module_name(FileName) ->
    Ext = .filename:extension(FileName),
    get_module_name(FileName, Ext).

-type(extension() :: string()).
-spec(get_module_name/2 :: (string(), extension()) -> 
	     {ok, atom()} | {error, any()}).
get_module_name(FileName, ".beam") ->
    Info = beam_lib:info(FileName),
    proplists:get_value(module, Info);
get_module_name(FileName, ".erl") ->
    case .file:open(FileName, [read]) of 
	{ok, IO} ->
	    Result = search_module(IO, []),
	    .file:close(IO),
	    Result;
	Error -> Error
    end.

-type(io_device() :: pid()).
-spec(search_module/2 :: (IO::io_device(), []) -> 
	     {ok, atom()} | {error, any()}).    
search_module(IO, []) ->
    Res = case .io:get_line(IO, "") of
	      %% -module must be first attribute
	      "-" ++ Attribute -> 
		  case  .string:tokens(Attribute, "()") of
		      ["module", String | _Rest] -> {ok, list_to_atom(String)};
		      _Else -> []
		  end;
	      eof -> {error, not_found};   
	      {error, _Reason} = Error -> Error;
	      _Any -> []
	  end,
    search_module(IO, Res);
search_module(_IO, Result) -> Result.

priv_dir(Module) ->
    filename:join([app_dir(Module), "priv"]).
ebin_dir(Module) ->
    filename:join([app_dir(Module), "ebin"]).
app_dir(Module) ->
    case code:which(Module) of
	non_existing ->
	    {error, {cannot_find_module, Module}};
	Module_Path ->
	    %% packet shouldn't have ebin component in their name
	    find_upper(Module_Path, "ebin")
    end.

find_upper(Where, What) ->
    List = filename:split(Where),
    case find_upper_(lists:reverse(List), What) of
	{ok, Res} -> filename:join(Res);
	Error -> Error
    end.
    
find_upper_([C | Rest], C) -> {ok, lists:reverse(Rest)};
find_upper_([_C | Rest], What) -> find_upper_(Rest, What);
find_upper_([], C) -> {error, {no_found, C}}.
    

package(Module_String) -> filename:rootname(Module_String).

    
    
    
