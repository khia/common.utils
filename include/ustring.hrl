-define(is_ustring(UString), is_record(UString, ustring)).
-record(ustring, {len = 0 :: integer(),
		  codepage = 'UTF-8' :: atom(), %% for future usage
		  text = <<>> :: binary()}).
