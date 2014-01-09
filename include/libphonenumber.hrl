%% @headerfile

-record(phonenumber,
{
	national_number :: non_neg_integer(),
	country_code :: non_neg_integer(),
	italian_leading_zero :: boolean(),
	extension :: binary(),
	raw_input :: binary(),
	preferred_domestic_carrier_code :: binary(),
	country_code_source :: non_neg_integer()
}).
-type phonenumber() :: #phonenumber{}.


-type phonenumber_type() :: 
	fixed_line | 
	mobile | 
	fixed_line_or_mobile | 
	toll_free |
	premium_rate |
	shared_cost |
	voip |
	personal_number |
	pager |
	uan |
	voicemail |
	unknown.

-type phonenumber_format() :: 
	e164 |
	international |
	national |
	rfc3966.


-type validation_result() :: 
	is_possible |
	invalid_country_code |
	too_short |
	too_long.

-type match_type() :: 
	invalid_number | 
	no_match | 
	short_nsn_match | 
	nsn_match |
	exact_match.