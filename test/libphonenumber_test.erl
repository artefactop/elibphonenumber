-module(libphonenumber_test).

-include_lib("eunit/include/eunit.hrl").

get_supported_regions_test() ->
    0 < libphonenumber_util:get_supported_regions().

get_region_codes_for_country_calling_code_test() ->
    Regions1 = libphonenumber_util:get_region_codes_for_country_calling_code(1),
    true = lists:any(fun(X) -> X =:= <<"US">> end, Regions1),
    true = lists:any(fun(X) -> X =:= <<"BS">> end, Regions1),

    Regions44 = libphonenumber_util:get_region_codes_for_country_calling_code(44),
    true = lists:any(fun(X) -> X =:= <<"GB">> end, Regions44),

    Regions49 = libphonenumber_util:get_region_codes_for_country_calling_code(49),
    true = lists:any(fun(X) -> X =:= <<"DE">> end, Regions49),

    RegionsUN001 = libphonenumber_util:get_region_codes_for_country_calling_code(800),
    true = lists:any(fun(X) -> X =:= <<"001">> end, RegionsUN001),

    RegionsInvalid = libphonenumber_util:get_region_codes_for_country_calling_code(9),
    0 = erlang:length(RegionsInvalid).

is_alpha_number_test() ->
    true = libphonenumber_util:is_alpha_number(<<"1800 six-flags">>), 
    true = libphonenumber_util:is_alpha_number(<<"1800 six-flags ext. 1234">>), 
    true = libphonenumber_util:is_alpha_number(<<"+800 six-flags">>), 
    true = libphonenumber_util:is_alpha_number(<<"180 six-flags">>), 
    false = libphonenumber_util:is_alpha_number(<<"1800 123-1234">>), 
    false = libphonenumber_util:is_alpha_number(<<"1 six-flags">>), 
    false = libphonenumber_util:is_alpha_number(<<"18 six-flags">>), 
    false = libphonenumber_util:is_alpha_number(<<"1800 123-1234 extension: 1234">>), 
    false = libphonenumber_util:is_alpha_number(<<"+800 1234-1234">>).

convert_alpha_characters_in_number_test() ->
    <<"1800-222-333">> = libphonenumber_util:convert_alpha_characters_in_number(<<"1800-ABC-DEF">>),
    <<"1", 16#E3,16#80,16#80,16#EF,16#BC,16#88," 800) 222-333">> = libphonenumber_util:convert_alpha_characters_in_number(<<"1", 16#E3,16#80,16#80,16#EF,16#BC,16#88," 800) 222-333">>). 

normalize_digits_only_test() ->
    <<"03456234">> = libphonenumber_util:normalize_digits_only(<<"034-56&+a#234">>).

get_national_significant_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    <<"6502530000">> = libphonenumber_util:get_national_significant_number(P12),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(312345678, P21),
    <<"312345678">> = libphonenumber_util:get_national_significant_number(P22), 
    
    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(39, P3),
    P32 = phonenumber:set_national_number(236618300, P31),
    P33 = phonenumber:set_italian_leading_zero(true, P32),
    <<"0236618300">> = libphonenumber_util:get_national_significant_number(P33), 
    
    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(39, P4),
    P42 = phonenumber:set_national_number(12345678, P41),
    <<"12345678">> = libphonenumber_util:get_national_significant_number(P42).

get_length_of_geograpical_area_code_test() ->
    %% Google MTV, which has area code "650".
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    3 = libphonenumber_util:get_length_of_geograpical_area_code(P12),

    %% A North America toll-free number, which has no area code.
    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(8002530000, P21),
    0 = libphonenumber_util:get_length_of_geograpical_area_code(P22), 

    %% An invalid US number (1 digit shorter), which has no area code.
    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(1, P3),
    P32 = phonenumber:set_national_number(65025300001, P31),
    0 = libphonenumber_util:get_length_of_geograpical_area_code(P32), 

    %% Google London, which has area code "20".
    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(44, P4),
    P42 = phonenumber:set_national_number(2070313000, P41),
    2 = libphonenumber_util:get_length_of_geograpical_area_code(P42),

    %% A UK mobile phone, which has no area code.
    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(44, P5),
    P52 = phonenumber:set_national_number(7123456789, P51),
    0 = libphonenumber_util:get_length_of_geograpical_area_code(P52),

    %% Google Buenos Aires, which has area code "11".
    P6 = phonenumber:new(),
    P61 = phonenumber:set_country_code(54, P6),
    P62 = phonenumber:set_national_number(1155303000, P61),
    2 = libphonenumber_util:get_length_of_geograpical_area_code(P62),

    %% Google Sydney, which has area code "2".
    P7 = phonenumber:new(),
    P71 = phonenumber:set_country_code(61, P7),
    P72 = phonenumber:set_national_number(293744000, P71),
    1 = libphonenumber_util:get_length_of_geograpical_area_code(P72),

    %% Italian numbers - there is no national prefix, but it still has an area code.
    P8 = phonenumber:new(),
    P81 = phonenumber:set_country_code(39, P8),
    P82 = phonenumber:set_national_number(236618300, P81),
    P83 = phonenumber:set_italian_leading_zero(true, P82), 
    2 = libphonenumber_util:get_length_of_geograpical_area_code(P83),

    %% Google Singapore. Singapore has no area code and no national prefix.
    P9 = phonenumber:new(),
    P91 = phonenumber:set_country_code(65, P9),
    P92 = phonenumber:set_national_number(65218000, P91),
    P93 = phonenumber:set_italian_leading_zero(false, P92), 
    0 = libphonenumber_util:get_length_of_geograpical_area_code(P93),

    %% An international toll free number, which has no area code.
    PA = phonenumber:new(),
    PA1 = phonenumber:set_country_code(800, PA),
    PA2 = phonenumber:set_national_number(12345678, PA1),
    0 = libphonenumber_util:get_length_of_geograpical_area_code(PA2).

format_us_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    <<"650 253 0000">> = libphonenumber_util:format(P12, national), 
    <<"+1 650 253 0000">> = libphonenumber_util:format(P12, international),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(8002530000, P21),
    <<"800 253 0000">> = libphonenumber_util:format(P22, national), 
    <<"+1 800 253 0000">> = libphonenumber_util:format(P22, international),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(1, P3),
    P32 = phonenumber:set_national_number(9002530000, P31),
    <<"900 253 0000">> = libphonenumber_util:format(P32, national), 
    <<"+1 900 253 0000">> = libphonenumber_util:format(P32, international),
    <<"tel:+1-900-253-0000">> = libphonenumber_util:format(P32, rfc3966),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(1, P4),
    P42 = phonenumber:set_national_number(0, P41),
    <<"0">> = libphonenumber_util:format(P42, national), 

    %% Numbers with all zeros in the national number part will be formatted by
    %% using the raw_input if that is available no matter which format is
    %% specified.
    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(1, P5),
    P52 = phonenumber:set_national_number(0, P51),
    P53 = phonenumber:set_raw_input(<<"000-000-0000">>, P52),
    <<"000-000-0000">> = libphonenumber_util:format(P53, national).