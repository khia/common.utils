-record(caller_process, {module :: atom(), line :: integer(), pid :: pid()}).
-type(caller_process() :: #caller_process{}).
-define(caller_process(M, P), #caller_process{module = M, line = ?LINE, pid = P}).
-define(caller_signature, ?caller_process(?MODULE, self())).

-record(mfa, {m :: atom(), f :: atom(), a :: [term()]}).
%%-type(mfa() :: #mfa{}).
-define(mfa(M, F, A), #mfa{m = M, f = F, a = A}).

-record(process_callback, {pid, line, module, func, args}).
-define(process_callback(F, A), #process_callback{pid = self(), 
						  line = ?LINE, 
						  module = ?MODULE, 
						  func = F, 
						  args = A}).

-define(ri(Record_Name), record_info(fields, Record_Name)).
-define(record_to_keyval(Name, Record), 
	.lists:zip(record_info(fields, Name), tl(tuple_to_list(Record)))).

-define(keyval_to_record(Name, KeyVal), 
	.common.utils.lists:to_record(Name,KeyVal,record_info(fields,Name)).


%% 	[{F, V} || V <- tl(tuple_to_list(Record)), 
%% 		   F <- record_info(fields, Name)]).

%% -define(get_field_idx(Record_Name, Field_Name), 
%%         [case Field_Name =:= F of
%% 	     true -> I
%% 	    ])

-define(PACKAGE_STRING, .filename:rootname(?MODULE_STRING)).
-define(PACKAGE, list_to_atom(?PACKAGE_STRING)). %% NOT VERY GOOD

-define(APP_STRING, .filename:rootname(?PACKAGE_STRING)).
-define(APP, list_to_atom(?APP_STRING)). %% NOT VERY GOOD
