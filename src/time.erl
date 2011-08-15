-module(common.utils.time).

-import(lists).
-import(calendar).
-import(httpd_util).
-import(io_lib).

-export([ts/1, ts_format/2, ts_to_sec/1, ts_type/1]).

-type(year()     :: integer()).
-type(month()    :: 1..12 | integer()).
-type(day()      :: 1..31 | integer()).
-type(hour()     :: 0..23 | integer()).
-type(minute()   :: 0..59 | integer()).
-type(second()   :: 0..59 | integer()).
-type(micro_sec() :: 0..999 | integer()).
-type(mega_sec() :: integer()). %% 1 MegaSec = 1000000 Sec

-type(ts() :: {ts, Mega :: mega_sec(), Sec :: second(), Micro :: micro_sec()}).
-type(now() :: {Mega :: mega_sec(), Sec :: second(), Micro :: micro_sec()}).
-type(universal() :: {
		 {Year :: year(), Month :: month(), Day :: day()}, 
		 {Hour :: hour(), Min :: minute(), Sec :: second()}}).
-type(time() :: ts() | now() | universal() | second()).

%% @doc 
%%    Returns formatted string in one of supported formats 
%% for the current time.
%% @end
-spec(ts/1 :: (rfc1123 | iso8601) -> string()).
ts(rfc1123) -> ts_rfc1123();
ts(iso8601) -> ts_iso8601().

%% @doc 
%%    Returns formatted string in one of supported formats 
%% for the specified time. In case of error function returns "".
%% @end
-spec(ts_format/2 :: (rfc1123 | iso8601, TS :: time()) -> string()).
ts_format(rfc1123, TS) -> ts_to_rfc1123(TS);
ts_format(iso8601, TS) -> ts_to_iso8601(TS).
    
ts_rfc1123() -> 
    ts_to_rfc1123(calendar:local_time()).
    
ts_to_rfc1123(TS) -> 
    ts_to_rfc1123(ts_type(TS), TS).

ts_to_rfc1123(ts, TS) ->
    {ts, Mega, Sec, Milly} = TS,
    Universal = calendar:now_to_universal_time({Mega, Sec, Milly}),
    ts_to_rfc1123(universal, Universal);
ts_to_rfc1123(now, TS) ->
    Universal = calendar:now_to_universal_time(TS),
    ts_to_rfc1123(universal, Universal);
ts_to_rfc1123(seconds, TS) ->    
    Universal = calendar:gregorian_to_datetime(TS),
    ts_to_rfc1123(universal, Universal);
ts_to_rfc1123(universal, TS) ->
    httpd_util:rfc1123_date(TS);
ts_to_rfc1123(undefined, _TS) -> "".

ts_iso8601() ->
    ts_to_iso8601(calendar:local_time()).

ts_to_iso8601(TS) ->
    ts_to_iso8601(ts_type(TS), TS).

ts_to_iso8601(ts, TS) ->
    {ts, Mega, Sec, Milly} = TS,
    Universal = calendar:now_to_universal_time({Mega, Sec, Milly}),
    ts_to_iso8601(universal, Universal);
ts_to_iso8601(now, TS) ->
    Universal = calendar:now_to_universal_time(TS),
    ts_to_iso8601(universal, Universal);
ts_to_iso8601(seconds, TS) ->    
    Universal = calendar:gregorian_to_datetime(TS),
    ts_to_iso8601(universal, Universal);
ts_to_iso8601(universal, TS) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = TS,
    L = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		      [Year, Month, Day, Hour, Min, Sec]),
    lists:flatten(L);
ts_to_iso8601(undefined, _TS) -> "".
    
%% @doc 
%%    Convert specified time stamp TS into number of seconds 
%% 00:00:00 of 1 January of 0 year to time specified in TS. 
%% The function returns 0 if cannot parse date format.
%% @end
-spec(ts_to_sec/1 :: (TS :: time()) -> integer()).
ts_to_sec(TS) -> 
    ts_to_sec(ts_type(TS), TS).

ts_to_sec(ts, TS) ->
    {ts, Mega, Sec, Milly} = TS,
    Universal = calendar:now_to_universal_time({Mega, Sec, Milly}),
    ts_to_sec(universal, Universal);
ts_to_sec(now, TS) ->
    Universal = calendar:now_to_universal_time(TS),
    ts_to_sec(universal, Universal);
ts_to_sec(universal, TS) ->
    calendar:datetime_to_gregorian_seconds(TS);
ts_to_sec(seconds, TS) ->
    TS;
ts_to_sec(undefined, _TS) -> 0.

%% @doc
%%    Function determines the current format of time stamp TS.
%% For unknown format it returns undefined.
%% @end
-spec(ts_type/1 :: (TS :: time()) -> 
	     ts | now | universal | seconds | undefined).
ts_type({ts, _Mega, _Sec, _Milly})-> ts;
ts_type({_Mega, _Sec, _Milly}) -> now;
ts_type({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}}) -> universal;
ts_type(Integer) when is_integer(Integer) -> seconds;
ts_type(_Any) -> undefined.
