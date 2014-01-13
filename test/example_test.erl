-module(example_test).

-include_lib("eunit/include/eunit.hrl").

get_country_region_from_number_test() ->
    PhoneNumber = phonenumber_util:parse(<<"+34644743083">>, <<"">>), 
    <<"ES">> = phonenumber_util:get_region_code_for_number(PhoneNumber). 
    