%% @headerfile

-record(phonenumber,
{
    has_country_code :: boolean(),
    country_code :: non_neg_integer(),
    has_national_number :: boolean(),
    national_number :: non_neg_integer(),
    has_extension :: boolean(),
    extension :: binary(),
    has_number_of_leading_zeros :: boolean(),
    number_of_leading_zeros :: non_neg_integer(),
    has_italian_leading_zero :: boolean(),
    italian_leading_zero :: boolean(),
    has_raw_input :: boolean(),
    raw_input :: binary(),
    has_country_code_source :: boolean(),
    country_code_source :: non_neg_integer(),
    has_preferred_domestic_carrier_code :: boolean(),
    preferred_domestic_carrier_code :: binary()
}). %% tuple 16 + 1 
-type phonenumber() :: #phonenumber{}.

-type phonenumber_country_code_source() ::
    from_number_with_plus_sign | 
    from_number_with_idd |
    from_number_without_plus_sign |
    from_default_country.

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