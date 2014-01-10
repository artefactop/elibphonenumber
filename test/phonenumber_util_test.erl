-module(phonenumber_util_test).

-include("../include/libphonenumber.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(INVALID_COUNTRY_CODE, 2).

get_supported_regions_test() ->
    0 < phonenumber_util:get_supported_regions().

get_region_codes_for_country_calling_code_test() ->
    Regions1 = phonenumber_util:get_region_codes_for_country_calling_code(1),
    true = lists:any(fun(X) -> X =:= <<"US">> end, Regions1),
    true = lists:any(fun(X) -> X =:= <<"BS">> end, Regions1),

    Regions44 = phonenumber_util:get_region_codes_for_country_calling_code(44),
    true = lists:any(fun(X) -> X =:= <<"GB">> end, Regions44),

    Regions49 = phonenumber_util:get_region_codes_for_country_calling_code(49),
    true = lists:any(fun(X) -> X =:= <<"DE">> end, Regions49),

    Regions001 = phonenumber_util:get_region_codes_for_country_calling_code(800),
    true = lists:any(fun(X) -> X =:= <<"001">> end, Regions001),

    RegionsInvalid = phonenumber_util:get_region_codes_for_country_calling_code(?INVALID_COUNTRY_CODE),
    0 = erlang:length(RegionsInvalid).

is_alpha_number_test() ->
    true = phonenumber_util:is_alpha_number(<<"1800 six-flags">>), 
    true = phonenumber_util:is_alpha_number(<<"1800 six-flags ext. 1234">>), 
    true = phonenumber_util:is_alpha_number(<<"+800 six-flags">>), 
    true = phonenumber_util:is_alpha_number(<<"180 six-flags">>), 
    false = phonenumber_util:is_alpha_number(<<"1800 123-1234">>), 
    false = phonenumber_util:is_alpha_number(<<"1 six-flags">>), 
    false = phonenumber_util:is_alpha_number(<<"18 six-flags">>), 
    false = phonenumber_util:is_alpha_number(<<"1800 123-1234 extension: 1234">>), 
    false = phonenumber_util:is_alpha_number(<<"+800 1234-1234">>).

convert_alpha_characters_in_number_test() ->
    <<"1800-222-333">> = phonenumber_util:convert_alpha_characters_in_number(<<"1800-ABC-DEF">>),
    <<"1", 16#E3,16#80,16#80,16#EF,16#BC,16#88," 800) 222-333">> = phonenumber_util:convert_alpha_characters_in_number(<<"1", 16#E3,16#80,16#80,16#EF,16#BC,16#88," 800) 222-333">>). 

normalize_digits_only_test() ->
    <<"03456234">> = phonenumber_util:normalize_digits_only(<<"034-56&+a#234">>).

normalize_diallable_chars_only_test() ->
    <<"03*456+234">> = phonenumber_util:normalize_diallable_chars_only(<<"03*4-56&+a#234">>).

get_national_significant_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    <<"6502530000">> = phonenumber_util:get_national_significant_number(P12),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(312345678, P21),
    <<"312345678">> = phonenumber_util:get_national_significant_number(P22), 
    
    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(39, P3),
    P32 = phonenumber:set_national_number(236618300, P31),
    P33 = phonenumber:set_italian_leading_zero(true, P32),
    <<"0236618300">> = phonenumber_util:get_national_significant_number(P33), 
    
    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(39, P4),
    P42 = phonenumber:set_national_number(12345678, P41),
    <<"12345678">> = phonenumber_util:get_national_significant_number(P42).

get_length_of_geograpical_area_code_test() ->
    %% Google MTV, which has area code "650".
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    3 = phonenumber_util:get_length_of_geograpical_area_code(P12),

    %% A North America toll-free number, which has no area code.
    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(8002530000, P21),
    0 = phonenumber_util:get_length_of_geograpical_area_code(P22), 

    %% An invalid US number (1 digit shorter), which has no area code.
    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(1, P3),
    P32 = phonenumber:set_national_number(65025300001, P31),
    0 = phonenumber_util:get_length_of_geograpical_area_code(P32), 

    %% Google London, which has area code "20".
    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(44, P4),
    P42 = phonenumber:set_national_number(2070313000, P41),
    2 = phonenumber_util:get_length_of_geograpical_area_code(P42),

    %% A UK mobile phone, which has no area code.
    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(44, P5),
    P52 = phonenumber:set_national_number(7123456789, P51),
    0 = phonenumber_util:get_length_of_geograpical_area_code(P52),

    %% Google Buenos Aires, which has area code "11".
    P6 = phonenumber:new(),
    P61 = phonenumber:set_country_code(54, P6),
    P62 = phonenumber:set_national_number(1155303000, P61),
    2 = phonenumber_util:get_length_of_geograpical_area_code(P62),

    %% Google Sydney, which has area code "2".
    P7 = phonenumber:new(),
    P71 = phonenumber:set_country_code(61, P7),
    P72 = phonenumber:set_national_number(293744000, P71),
    1 = phonenumber_util:get_length_of_geograpical_area_code(P72),

    %% Italian numbers - there is no national prefix, but it still has an area code.
    P8 = phonenumber:new(),
    P81 = phonenumber:set_country_code(39, P8),
    P82 = phonenumber:set_national_number(236618300, P81),
    P83 = phonenumber:set_italian_leading_zero(true, P82), 
    2 = phonenumber_util:get_length_of_geograpical_area_code(P83),

    %% Google Singapore. Singapore has no area code and no national prefix.
    P9 = phonenumber:new(),
    P91 = phonenumber:set_country_code(65, P9),
    P92 = phonenumber:set_national_number(65218000, P91),
    P93 = phonenumber:set_italian_leading_zero(false, P92), 
    0 = phonenumber_util:get_length_of_geograpical_area_code(P93),

    %% An international toll free number, which has no area code.
    PA = phonenumber:new(),
    PA1 = phonenumber:set_country_code(800, PA),
    PA2 = phonenumber:set_national_number(12345678, PA1),
    0 = phonenumber_util:get_length_of_geograpical_area_code(PA2).

get_length_of_national_destination_code_test() ->
    %% Google MTV, which has national destination code (NDC) "650".
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    3 = phonenumber_util:get_length_of_national_destination_code(P12),

    %% A North America toll-free number, which has NDC "800".
    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(8002530000, P21),
    3 = phonenumber_util:get_length_of_national_destination_code(P22), 

    %% Google London, which has NDC "20".
    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(65025300001, P31),
    2 = phonenumber_util:get_length_of_national_destination_code(P32), 

    %% Google London, which has area code "20".
    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(44, P4),
    P42 = phonenumber:set_national_number(2070313000, P41),
    2 = phonenumber_util:get_length_of_national_destination_code(P42),

    %% A UK mobile phone, which has NDC "7123"
    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(44, P5),
    P52 = phonenumber:set_national_number(7123456789, P51),
    4 = phonenumber_util:get_length_of_national_destination_code(P52),

    %% Google Buenos Aires, which has NDC "11".
    P6 = phonenumber:new(),
    P61 = phonenumber:set_country_code(54, P6),
    P62 = phonenumber:set_national_number(1155303000, P61),
    2 = phonenumber_util:get_length_of_national_destination_code(P62),

    %% An Argentinian mobile which has NDC "911".
    P7 = phonenumber:new(),
    P71 = phonenumber:set_country_code(54, P7),
    P72 = phonenumber:set_national_number(91187654321, P71),
    3 = phonenumber_util:get_length_of_national_destination_code(P72),

    %% Google Singapore. Singapore has NDC "6521".
    P8 = phonenumber:new(),
    P81 = phonenumber:set_country_code(65, P8),
    P82 = phonenumber:set_national_number(65218000, P81),
    4 = phonenumber_util:get_length_of_national_destination_code(P82),

    %% An invalid US number (1 digit shorter), which has no NDC.
    P9 = phonenumber:new(),
    P91 = phonenumber:set_country_code(1, P9),
    P92 = phonenumber:set_national_number(650253000, P91), 
    0 = phonenumber_util:get_length_of_national_destination_code(P92),

    %% A number containing an invalid country code, which shouldn't have any NDC.
    PA = phonenumber:new(),
    PA1 = phonenumber:set_country_code(123, PA),
    PA2 = phonenumber:set_national_number(650253000, PA1),
    0 = phonenumber_util:get_length_of_national_destination_code(PA2),

    %% A number that has only one group of digits after country code when
    %% formatted in the international format.
    PA = phonenumber:new(),
    PA1 = phonenumber:set_country_code(367, PA),
    PA2 = phonenumber:set_national_number(12345, PA1),
    0 = phonenumber_util:get_length_of_national_destination_code(PA2),

    %% The same number above, but with an extension.
    PB = phonenumber:new(),
    PB1 = phonenumber:set_country_code(367, PB),    
    PB2 = phonenumber:set_national_number(12345, PB1),
    PB3 = phonenumber:set_extension(<<"321">>, PB2), 
    0 = phonenumber_util:get_length_of_national_destination_code(PB3),

    %% An international toll free number, which has NDC "1234".
    PC = phonenumber:new(),
    PC1 = phonenumber:set_country_code(800, PC),    
    PC2 = phonenumber:set_national_number(12345678, PC1),
    4 = phonenumber_util:get_length_of_national_destination_code(PC2).

get_country_mobile_token_test() ->
    CountryCallingCodeMX = phonenumber_util:get_country_code_for_region(<<"MX">>),
    <<"1">> = phonenumber_util:get_country_mobile_token(CountryCallingCodeMX),

    %% Country calling code for Sweden, which has no mobile token.
    CountryCallingCodeSE = phonenumber_util:get_country_code_for_region(<<"SE">>),
    <<"">> = phonenumber_util:get_country_mobile_token(CountryCallingCodeSE). 

is_nanpa_country_test() ->
    true = phonenumber_util:is_nanpa_country(<<"US">>), 
    true = phonenumber_util:is_nanpa_country(<<"BS">>),
    false = phonenumber_util:is_nanpa_country(<<"DE">>),
    false = phonenumber_util:is_nanpa_country(<<"ZZ">>),
    false = phonenumber_util:is_nanpa_country(<<"001">>). 

is_valid_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    true = phonenumber_util:is_valid_number(P12), 

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(236618300, P21),
    P23 = phonenumber:set_italian_leading_zero(true, P22),
    true = phonenumber_util:is_valid_number(P23),  

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(7912345678, P31),
    true = phonenumber_util:is_valid_number(P32), 

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(64, P4),
    P42 = phonenumber:set_national_number(21387835, P41),
    true = phonenumber_util:is_valid_number(P42), 

    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(800, P5),
    P52 = phonenumber:set_national_number(12345678, P51),
    true = phonenumber_util:is_valid_number(P52), 

    P6 = phonenumber:new(),
    P61 = phonenumber:set_country_code(979, P6),
    P62 = phonenumber:set_national_number(123456789, P61),
    true = phonenumber_util:is_valid_number(P62). 

is_valid_number_for_region_test() ->
    %% This number is valid for the Bahamas, but is not a valid US number.
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(2423232345, P11),
    true = phonenumber_util:is_valid_number(P12), 
    true = phonenumber_util:is_valid_number_for_region(P12, <<"BS">>), 
    false = phonenumber_util:is_valid_number_for_region(P12, <<"US">>), 
    P13 = phonenumber:set_national_number(2421232345, P12),
    false = phonenumber_util:is_valid_number(P13), 

    %% La Mayotte and Réunion use 'leadingDigits' to differentiate them.
    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(262, P2),
    P22 = phonenumber:set_national_number(262123456, P21),
    true = phonenumber_util:is_valid_number(P22), 
    true = phonenumber_util:is_valid_number_for_region(P22, <<"RE">>), 
    false = phonenumber_util:is_valid_number_for_region(P22, <<"YT">>), 
    %% Now change the number to be a number for La Mayotte.
    P23 = phonenumber:set_national_number(269601234, P22),
    true = phonenumber_util:is_valid_number_for_region(P23, <<"YT">>),
    false = phonenumber_util:is_valid_number_for_region(P23, <<"RE">>), 
    %% This number is no longer valid.
    P24 = phonenumber:set_national_number(269123456, P23),
    false = phonenumber_util:is_valid_number_for_region(P24, <<"YT">>),
    false = phonenumber_util:is_valid_number_for_region(P24, <<"RE">>),
    false = phonenumber_util:is_valid_number(P24), 
    %% However, it should be recognised as from La Mayotte.
    <<"YT">> = phonenumber_util:get_region_code_for_number(P24),
    %% This number is valid in both places.
    P25 = phonenumber:set_national_number(800123456, P24),
    true = phonenumber_util:is_valid_number_for_region(P25, <<"YT">>),
    true = phonenumber_util:is_valid_number_for_region(P25, <<"RE">>),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(800, P3),
    P32 = phonenumber:set_national_number(12345678, P31),
    true = phonenumber_util:is_valid_number_for_region(P32, <<"001">>), 
    false = phonenumber_util:is_valid_number_for_region(P32, <<"US">>), 
    false = phonenumber_util:is_valid_number_for_region(P32, <<"ZZ">>),

    P4 = phonenumber:new(),
    %% Invalid country calling codes.
    P41 = phonenumber:set_country_code(3923, P4),
    P42 = phonenumber:set_national_number(2366, P41),
    false = phonenumber_util:is_valid_number_for_region(P42, <<"ZZ">>), 
    false = phonenumber_util:is_valid_number_for_region(P42, <<"001">>).

get_region_code_for_country_code_test() ->
    <<"US">> = phonenumber_util:get_region_code_for_country_code(1), 
    <<"GB">> = phonenumber_util:get_region_code_for_country_code(44), 
    <<"DE">> = phonenumber_util:get_region_code_for_country_code(49),
    <<"001">> = phonenumber_util:get_region_code_for_country_code(800),
    <<"001">> = phonenumber_util:get_region_code_for_country_code(979).  

get_region_code_for_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(2423232345, P11),
    <<"BS">> = phonenumber_util:get_region_code_for_number(P12), 

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(4241231234, P21),
    <<"ZZ">> = phonenumber_util:get_region_code_for_number(P22),  

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(7912345678, P31),
    <<"GB">> = phonenumber_util:get_region_code_for_number(P32), 

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(800, P4),
    P42 = phonenumber:set_national_number(12345678, P41),
    <<"001">> = phonenumber_util:get_region_code_for_number(P42), 

    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(979, P5),
    P52 = phonenumber:set_national_number(123456789, P51),
    <<"001">> = phonenumber_util:get_region_code_for_number(P52).

is_possible_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    true = phonenumber_util:is_possible_number(P12), 

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(2530000, P21),
    true = phonenumber_util:is_possible_number(P22),  

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(2070313000, P31),
    true = phonenumber_util:is_possible_number(P32), 

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(800, P4),
    P42 = phonenumber:set_national_number(12345678, P41),
    true = phonenumber_util:is_possible_number(P42).

is_possible_number_for_string_test() ->
    true = phonenumber_util:is_possible_number_for_string(<<"+1 650 253 0000">>, <<"US">>),
    true = phonenumber_util:is_possible_number_for_string(<<"+1 650 GOO OGLE">>, <<"US">>),
    true = phonenumber_util:is_possible_number_for_string(<<"(650) 253-0000">>, <<"US">>),
    true = phonenumber_util:is_possible_number_for_string(<<"253-0000">>, <<"US">>),
    true = phonenumber_util:is_possible_number_for_string(<<"+1 650 253 0000">>, <<"GB">>),
    true = phonenumber_util:is_possible_number_for_string(<<"+44 20 7031 3000">>, <<"GB">>),
    true = phonenumber_util:is_possible_number_for_string(<<"(020) 7031 3000">>, <<"GB">>),
    true = phonenumber_util:is_possible_number_for_string(<<"7031 3000">>, <<"GB">>),
    true = phonenumber_util:is_possible_number_for_string(<<"3331 6005">>, <<"NZ">>),
    true = phonenumber_util:is_possible_number_for_string(<<"+800 1234 5678">>, <<"001">>).

is_possible_number_with_reason_test() ->
    %% FYI, national numbers for country code +1 that are within 7 to 10 digits
    %% are possible.
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    is_possible = phonenumber_util:is_possible_number_with_reason(P12), 

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(2530000, P21),
    is_possible = phonenumber_util:is_possible_number_with_reason(P22),  

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(0, P3),
    P32 = phonenumber:set_national_number(2530000, P31),
    invalid_country_code = phonenumber_util:is_possible_number_with_reason(P32), 

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(1, P4),
    P42 = phonenumber:set_national_number(253000, P41),
    too_short = phonenumber_util:is_possible_number_with_reason(P42), 

    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(1, P5),
    P52 = phonenumber:set_national_number(65025300000, P51),
    too_long = phonenumber_util:is_possible_number_with_reason(P52).

truncate_too_long_number_test() ->
    %% US number 650-253-0000, but entered with one additional digit at the end.
    TLN1 = phonenumber:new(),
    TLN11 = phonenumber:set_country_code(1, TLN1),
    TLN12 = phonenumber:set_national_number(65025300001, TLN11),
    VN1 = phonenumber:new(),
    VN11 = phonenumber:set_country_code(1, VN1),
    VN12 = phonenumber:set_national_number(6502530000, VN11),

    Truncate = phonenumber_util:truncate_too_long_number(TLN12),
    VN12 = Truncate.

format_us_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    <<"(650) 253-0000">> = phonenumber_util:format(P12, national), 
    <<"+1 650-253-0000">> = phonenumber_util:format(P12, international),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(8002530000, P21),
    <<"(800) 253-0000">> = phonenumber_util:format(P22, national), 
    <<"+1 800-253-0000">> = phonenumber_util:format(P22, international),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(1, P3),
    P32 = phonenumber:set_national_number(9002530000, P31),
    <<"(900) 253-0000">> = phonenumber_util:format(P32, national), 
    <<"+1 900-253-0000">> = phonenumber_util:format(P32, international),
    <<"tel:+1-900-253-0000">> = phonenumber_util:format(P32, rfc3966),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(1, P4),
    P42 = phonenumber:set_national_number(0, P41),
    <<"0">> = phonenumber_util:format(P42, national), 

    %% Numbers with all zeros in the national number part will be formatted by
    %% using the raw_input if that is available no matter which format is
    %% specified.
    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(1, P5),
    P52 = phonenumber:set_national_number(0, P51),
    P53 = phonenumber:set_raw_input(<<"000-000-0000">>, P52),
    <<"000-000-0000">> = phonenumber_util:format(P53, national).

format_bs_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(2421234567, P11),
    <<"(242) 123-4567">> = phonenumber_util:format(P12, national), 
    <<"+1 242-123-4567">> = phonenumber_util:format(P12, international),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(8002530000, P21),
    <<"(800) 253-0000">> = phonenumber_util:format(P22, national), 
    <<"+1 800-253-0000">> = phonenumber_util:format(P22, international),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(1, P3),
    P32 = phonenumber:set_national_number(9002530000, P31),
    <<"(900) 253-0000">> = phonenumber_util:format(P32, national), 
    <<"+1 900-253-0000">> = phonenumber_util:format(P32, international).

format_gb_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(44, P1),
    P12 = phonenumber:set_national_number(2087389353, P11),
    <<"020 8738 9353">> = phonenumber_util:format(P12, national), 
    <<"+44 20 8738 9353">> = phonenumber_util:format(P12, international),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(44, P2),
    P22 = phonenumber:set_national_number(7912345678, P21),
    <<"07912 345678">> = phonenumber_util:format(P22, national), 
    <<"+44 7912 345678">> = phonenumber_util:format(P22, international).

format_de_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(49, P1),
    P12 = phonenumber:set_national_number(301234, P11),
    <<"030 1234">> = phonenumber_util:format(P12, national), 
    <<"+49 30 1234">> = phonenumber_util:format(P12, international),
    <<"tel:+49-30-1234">> = phonenumber_util:format(P12, rfc3966),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(49, P2),
    P22 = phonenumber:set_national_number(291123, P21),
    <<"0291 123">> = phonenumber_util:format(P22, national), 
    <<"+49 291 123">> = phonenumber_util:format(P22, international),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(49, P3),
    P32 = phonenumber:set_national_number(29112345678, P31),
    <<"0291 12345678">> = phonenumber_util:format(P32, national), 
    <<"+49 291 12345678">> = phonenumber_util:format(P32, international),
    <<"+4929112345678">> = phonenumber_util:format(P32, e164),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(49, P4),
    P42 = phonenumber:set_national_number(9123123, P41),
    <<"09123 123">> = phonenumber_util:format(P42, national), 
    <<"+49 9123 123">> = phonenumber_util:format(P42, international), 

    %% Note this number is correctly formatted without national prefix. Most of
    %% the numbers that are treated as invalid numbers by the library are short
    %% numbers, and they are usually not dialed with national prefix.
    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(49, P5),
    P52 = phonenumber:set_national_number(1234, P51),
    <<"1234">> = phonenumber_util:format(P52, national),
    <<"+49 1234">> = phonenumber_util:format(P52, international).

format_it_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(39, P1),
    P12 = phonenumber:set_national_number(236618300, P11),
    P13 = phonenumber:set_italian_leading_zero(true, P12),
    <<"02 3661 8300">> = phonenumber_util:format(P13, national), 
    <<"+39 02 3661 8300">> = phonenumber_util:format(P13, international),
    <<"+390236618300">> = phonenumber_util:format(P13, e164),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(345678901, P21),
    P23 = phonenumber:set_italian_leading_zero(false, P22),
    <<"345 678 901">> = phonenumber_util:format(P23, national), 
    <<"+39 345 678 901">> = phonenumber_util:format(P23, international),
    <<"+39345678901">> = phonenumber_util:format(P23, e164).

format_au_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(61, P1),
    P12 = phonenumber:set_national_number(236618300, P11),
    <<"(02) 3661 8300">> = phonenumber_util:format(P12, national), 
    <<"+61 2 3661 8300">> = phonenumber_util:format(P12, international),
    <<"+61236618300">> = phonenumber_util:format(P12, e164),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(61, P2),
    P22 = phonenumber:set_national_number(1800123456, P21),
    <<"1800 123 456">> = phonenumber_util:format(P22, national), 
    <<"+61 1800 123 456">> = phonenumber_util:format(P22, international),
    <<"+611800123456">> = phonenumber_util:format(P22, e164).

format_ar_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(54, P1),
    P12 = phonenumber:set_national_number(1187654321, P11),
    <<"011 8765-4321">> = phonenumber_util:format(P12, national), 
    <<"+54 11 8765-4321">> = phonenumber_util:format(P12, international),
    <<"+541187654321">> = phonenumber_util:format(P12, e164),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(54, P2),
    P22 = phonenumber:set_national_number(91187654321, P21),
    <<"011 15-8765-4321">> = phonenumber_util:format(P22, national), 
    <<"+54 9 11 8765-4321">> = phonenumber_util:format(P22, international),
    <<"+5491187654321">> = phonenumber_util:format(P22, e164).

format_mx_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(52, P1),
    P12 = phonenumber:set_national_number(12345678900, P11),
    <<"044 234 567 8900">> = phonenumber_util:format(P12, national), 
    <<"+52 1 234 567 8900">> = phonenumber_util:format(P12, international),
    <<"+5212345678900">> = phonenumber_util:format(P12, e164),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(52, P2),
    P22 = phonenumber:set_national_number(15512345678, P21),
    <<"044 55 1234 5678">> = phonenumber_util:format(P22, national), 
    <<"+52 1 55 1234 5678">> = phonenumber_util:format(P22, international),
    <<"+5215512345678">> = phonenumber_util:format(P22, e164),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(52, P3),
    P32 = phonenumber:set_national_number(3312345678, P31),
    <<"01 33 1234 5678">> = phonenumber_util:format(P32, national), 
    <<"+52 33 1234 5678">> = phonenumber_util:format(P32, international),
    <<"+523312345678">> = phonenumber_util:format(P32, e164),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(52, P4),
    P42 = phonenumber:set_national_number(8211234567, P41),
    <<"01 821 123 4567">> = phonenumber_util:format(P42, national), 
    <<"+52 821 123 4567">> = phonenumber_util:format(P42, international),
    <<"+528211234567">> = phonenumber_util:format(P42, e164).

%% format_by_pattern_test()-> false=true.

format_out_of_country_keeping_alpha_chars_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(8007493524, P11),
    P13 = phonenumber:set_raw_input(<<"1800 six-flag">>, P12),
    <<"0011 1 800 SIX-FLAG">> = phonenumber_util:format_out_of_country_keeping_alpha_chars(P13, <<"AU">>), 

    P14 = phonenumber:clear(P13),
    P15 = phonenumber:set_raw_input(<<"1-800-SIX-flag">>, P14), 
    <<"1-800-SIX-flag">> = phonenumber_util:format_out_of_country_keeping_alpha_chars(P15, <<"AU">>).  

format_national_number_with_carrier_code_test() ->
    %% We only support this for AR in our test metadata.
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(54, P1),
    P12 = phonenumber:set_national_number(91234125678, P11),
    <<"91234125678">> = phonenumber_util:format(P12, national),
    %% Test formatting with a carrier code.
    <<"91234125678">> = phonenumber_util:format_national_number_with_carrier_code(P12, <<"15">>),
    <<"91234125678">> = phonenumber_util:format_national_number_with_carrier_code(P12, <<"">>),
    %% Here the international rule is used, so no carrier code should be present.
    <<"+54 91234125678">> = phonenumber_util:format(P12, international),

    %% We don't support this for the US so there should be no change.
    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(4241231234, P21),
    <<"(424) 123-1234">> = phonenumber_util:format(P22, national),
    <<"(424) 123-1234">> = phonenumber_util:format_national_number_with_carrier_code(P22, <<"15">>),

    %% Invalid country code should just get the NSN.
    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(?INVALID_COUNTRY_CODE, P3),
    P32 = phonenumber:set_national_number(12345, P31),
    <<"12345">> = phonenumber_util:format_out_of_country_calling_number(P32, <<"89">>).

format_number_for_mobile_dialing_test() ->
    %% Numbers are normally dialed in national format in-country, and
    %% international format from outside the country.
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(49, P1),
    P12 = phonenumber:set_national_number(30123456, P11),
    <<"030123456">> = phonenumber_util:format_number_for_mobile_dialing(P12, <<"DE">>, false),
    <<"+4930123456">> = phonenumber_util:format_number_for_mobile_dialing(P12, <<"CH">>, false),

    P13 = phonenumber:set_extension(<<"1234">>, P12),
    <<"030123456">> = phonenumber_util:format_number_for_mobile_dialing(P13, <<"DE">>, false),
    <<"+4930123456">> = phonenumber_util:format_number_for_mobile_dialing(P13, <<"CH">>, false).

format_national_number_with_preferred_carrier_code_test() ->
    %% We only support this for AR in our test metadata.
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(54, P1),
    P12 = phonenumber:set_national_number(91234125678, P11),
    %% Test formatting with no preferred carrier code stored in the number itself.
    <<"91234125678">> = phonenumber_util:format_national_number_with_preferred_carrier_code(P12, <<"15">>),
    <<"91234125678">> = phonenumber_util:format_national_number_with_preferred_carrier_code(P12, <<"">>),

    %% Test formatting with preferred carrier code present.
    P13 = phonenumber:set_preferred_domestic_carrier_code(<<"19">>, P12),
    <<"91234125678">> = phonenumber_util:format(P13, national),
    <<"91234125678">> = phonenumber_util:format_national_number_with_preferred_carrier_code(P13, <<"15">>),
    <<"91234125678">> = phonenumber_util:format_national_number_with_preferred_carrier_code(P13, <<"">>),
  
    %% When the preferred_domestic_carrier_code is present (even when it contains
    %% an empty string), use it instead of the default carrier code passed in.

    P14 = phonenumber:set_preferred_domestic_carrier_code(<<"">>, P13),
    <<"91234125678">> = phonenumber_util:format_national_number_with_preferred_carrier_code(P14, <<"15">>),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(4241231234, P21),
    P23 = phonenumber:set_preferred_domestic_carrier_code(<<"99">>, P22),
    <<"(424) 123-1234">> = phonenumber_util:format(P23, national),
    <<"(424) 123-1234">> = phonenumber_util:format_national_number_with_preferred_carrier_code(P23, <<"15">>).

format_out_of_country_calling_number_test() -> 
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(9002530000, P11),
    <<"00 1 900-253-0000">> = phonenumber_util:format_out_of_country_calling_number(P12, <<"DE">>),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(1, P2),
    P22 = phonenumber:set_national_number(6502530000, P21),
    <<"1 (650) 253-0000">> = phonenumber_util:format_out_of_country_calling_number(P22, <<"BS">>),
    <<"00 1 650-253-0000">> = phonenumber_util:format_out_of_country_calling_number(P22, <<"PL">>),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(7912345678, P31),
    <<"011 44 7912 345678">> = phonenumber_util:format_out_of_country_calling_number(P32, <<"US">>),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(49, P4),
    P42 = phonenumber:set_national_number(1234, P41),
    <<"00 49 1234">> = phonenumber_util:format_out_of_country_calling_number(P42, <<"GB">>),
    %%  Note this number is correctly formatted without national prefix. Most of
    %% the numbers that are treated as invalid numbers by the library are short
    %% numbers, and they are usually not dialed with national prefix.
    <<"1234">> = phonenumber_util:format_out_of_country_calling_number(P42, <<"DE">>),

    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(39, P5),
    P52 = phonenumber:set_national_number(236618300, P51),
    P53 = phonenumber:set_italian_leading_zero(true, P52),
    <<"011 39 02 3661 8300">> = phonenumber_util:format_out_of_country_calling_number(P53, <<"US">>),
    <<"02 3661 8300">> = phonenumber_util:format_out_of_country_calling_number(P53, <<"IT">>),
    <<"+39 02 3661 8300">> = phonenumber_util:format_out_of_country_calling_number(P53, <<"SG">>),

    P6 = phonenumber:new(),
    P61 = phonenumber:set_country_code(65, P6),
    P62 = phonenumber:set_national_number(94777892, P61),
    <<"9477 7892">> = phonenumber_util:format_out_of_country_calling_number(P62, <<"SG">>),

    P7 = phonenumber:new(),
    P71 = phonenumber:set_country_code(54, P7),
    P72 = phonenumber:set_national_number(91187654321, P71),
    <<"011 54 9 11 8765-4321">> = phonenumber_util:format_out_of_country_calling_number(P72, <<"US">>),

    P8 = phonenumber:new(),
    P81 = phonenumber:set_country_code(54, P8),
    P82 = phonenumber:set_national_number(91187654321, P81),
    P83 = phonenumber:set_extension(<<"1234">>, P82),
    <<"011 54 9 11 8765-4321 ext. 1234">> = phonenumber_util:format_out_of_country_calling_number(P83, <<"US">>).

format_out_of_country_calling_number_with_invalid_region_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502530000, P11),
    %% AQ/Antarctica isn't a valid region code for phone number formatting,
    %% so this falls back to intl formatting.
    <<"+1 650-253-0000">> = phonenumber_util:format_out_of_country_calling_number(P12, <<"AQ">>),
    %% For region code 001, the out-of-country format always turns into the
    %% international format.
    <<"+1 650-253-0000">> = phonenumber_util:format_out_of_country_calling_number(P12, <<"001">>).

format_out_of_country_calling_number_with_preferred_intl_prefix_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(39, P1),
    P12 = phonenumber:set_national_number(236618300, P11),
    P22 = phonenumber:set_italian_leading_zero(true, P12),
    %% This should use 0011, since that is the preferred international prefix
    %% (both 0011 and 0012 are accepted as possible international prefixes in our
    %% test metadta.)
    <<"0011 39 02 3661 8300">> = phonenumber_util:format_out_of_country_calling_number(P22, <<"AU">>).

format_in_original_format_test() ->
    case phonenumber_util:parse_and_keep_raw_input(<<"+442087654321">>, <<"GB">>) of 
        {error, Condition1} -> throw(Condition1); 
        P1 ->
            <<"+44 20 8765 4321">> = phonenumber_util:format_in_original_format(P1, <<"GB">>)
    end,

    case phonenumber_util:parse_and_keep_raw_input(<<"02087654321">>, <<"GB">>) of 
        {error, Condition2} -> throw(Condition2); 
        P2 ->
            <<"020 8765 4321">> = phonenumber_util:format_in_original_format(P2, <<"GB">>)
    end,

    case phonenumber_util:parse_and_keep_raw_input(<<"011442087654321">>, <<"US">>) of 
        {error, Condition3} -> throw(Condition3); 
        P3 ->
            <<"011 44 20 8765 4321">> = phonenumber_util:format_in_original_format(P3, <<"US">>)
    end,

    case phonenumber_util:parse_and_keep_raw_input(<<"442087654321">>, <<"GB">>) of 
        {error, Condition4} -> throw(Condition4); 
        P4 ->
            <<"44 20 8765 4321">> = phonenumber_util:format_in_original_format(P4, <<"GB">>)
    end,

    case phonenumber_util:parse_and_keep_raw_input(<<"+42087654321">>, <<"GB">>) of 
        {error, Condition5} -> throw(Condition5); 
        P5 ->
            <<"+42087654321">> = phonenumber_util:format_in_original_format(P5, <<"GB">>)
    end,
    %% Invalid numbers that we have a formatting pattern for should be formatted
    %% properly.  Note area codes starting with 7 are intentionally excluded in
    %% the test metadata for testing purposes.
    case phonenumber_util:parse_and_keep_raw_input(<<"7345678901">>, <<"US">>) of 
        {error, Condition6} -> throw(Condition6); 
        P6 ->
            <<"(734) 567-8901">> = phonenumber_util:format_in_original_format(P6, <<"US">>)
    end,
    %% US is not a leading zero country, and the presence of the leading zero
    %% leads us to format the number using raw_input.
    case phonenumber_util:parse_and_keep_raw_input(<<"0734567 8901">>, <<"US">>) of 
        {error, Condition7} -> throw(Condition7); 
        P7 ->
            <<"0734567 8901">> = phonenumber_util:format_in_original_format(P7, <<"US">>)
    end,
    %% This number is valid, but we don't have a formatting pattern for it. Fall
    %% back to the raw input.
    case phonenumber_util:parse_and_keep_raw_input(<<"02-4567-8900">>, <<"KR">>) of 
        {error, Condition8} -> throw(Condition8); 
        P8 ->
            <<"02-4567-8900">> = phonenumber_util:format_in_original_format(P8, <<"KR">>)
    end,

    case phonenumber_util:parse_and_keep_raw_input(<<"01180012345678">>, <<"US">>) of 
        {error, Condition9} -> throw(Condition9); 
        P9 ->
            <<"011 800 1234 5678">> = phonenumber_util:format_in_original_format(P9, <<"US">>)
    end,

    case phonenumber_util:parse_and_keep_raw_input(<<"+80012345678">>, <<"KR">>) of 
        {error, Condition10} -> throw(Condition10); 
        P10 ->
            <<"+800 1234 5678">> = phonenumber_util:format_in_original_format(P10, <<"KR">>)
    end,
    %% US local numbers are formatted correctly, as we have formatting patterns
    %% for them.
    case phonenumber_util:parse_and_keep_raw_input(<<"2530000">>, <<"US">>) of 
        {error, Condition11} -> throw(Condition11); 
        P11 ->
            <<"253-0000">> = phonenumber_util:format_in_original_format(P11, <<"US">>)
    end,
    %% Number with national prefix in the US.
    case phonenumber_util:parse_and_keep_raw_input(<<"18003456789">>, <<"US">>) of 
        {error, Condition12} -> throw(Condition12); 
        P12 ->
            <<"1 800-345-6789">> = phonenumber_util:format_in_original_format(P12, <<"US">>)
    end,
    %% Number without national prefix in the UK.
    case phonenumber_util:parse_and_keep_raw_input(<<"2087654321">>, <<"GB">>) of 
        {error, Condition13} -> throw(Condition13); 
        P13 ->
            <<"20 8765 4321">> = phonenumber_util:format_in_original_format(P13, <<"GB">>)
    end,
    %% Make sure no metadata is modified as a result of the previous function
    %% call.
    case phonenumber_util:parse_and_keep_raw_input(<<"+442087654321">>, <<"GB">>) of 
        {error, Condition14} -> throw(Condition14); 
        P14 ->
            <<"+44 20 8765 4321">> = phonenumber_util:format_in_original_format(P14, <<"GB">>)
    end.

is_premium_rate_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(9004433030, P11),
    premium_rate = phonenumber_util:get_number_type(P12),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(892123, P21),
    premium_rate = phonenumber_util:get_number_type(P22),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(9187654321, P31),
    premium_rate = phonenumber_util:get_number_type(P32),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(49, P4),
    P42 = phonenumber:set_national_number(9001654321, P41),
    premium_rate = phonenumber_util:get_number_type(P42),

    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(49, P5),
    P52 = phonenumber:set_national_number(90091234567, P51),
    premium_rate = phonenumber_util:get_number_type(P52),

    P6 = phonenumber:new(),
    P61 = phonenumber:set_country_code(979, P6),
    P62 = phonenumber:set_national_number(123456789, P61),
    premium_rate = phonenumber_util:get_number_type(P62).

is_toll_free_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(8885551234, P11),
    toll_free = phonenumber_util:get_number_type(P12),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(803123, P21),
    toll_free = phonenumber_util:get_number_type(P22),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(800123456, P31),
    toll_free = phonenumber_util:get_number_type(P32),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(49, P4),
    P42 = phonenumber:set_national_number(8001234567, P41),
    toll_free = phonenumber_util:get_number_type(P42),

    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(800, P5),
    P52 = phonenumber:set_national_number(12345678, P51),
    toll_free = phonenumber_util:get_number_type(P52).

is_mobile_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(2423570000, P11),
    mobile = phonenumber_util:get_number_type(P12),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(3123456789, P21),
    mobile = phonenumber_util:get_number_type(P22),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(7912345678, P31),
    mobile = phonenumber_util:get_number_type(P32),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(49, P4),
    P42 = phonenumber:set_national_number(15123456789, P41),
    mobile = phonenumber_util:get_number_type(P42),

    P5 = phonenumber:new(),
    P51 = phonenumber:set_country_code(54, P5),
    P52 = phonenumber:set_national_number(91187654321, P51),
    mobile = phonenumber_util:get_number_type(P52).

is_fixed_line_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(2423651234, P11),
    fixed_line = phonenumber_util:get_number_type(P12),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(39, P2),
    P22 = phonenumber:set_national_number(236618300, P21),
    P23 = phonenumber:set_italian_leading_zero(true, P22), 
    fixed_line = phonenumber_util:get_number_type(P23),

    P3 = phonenumber:new(),
    P31 = phonenumber:set_country_code(44, P3),
    P32 = phonenumber:set_national_number(2012345678, P31),
    fixed_line = phonenumber_util:get_number_type(P32),

    P4 = phonenumber:new(),
    P41 = phonenumber:set_country_code(49, P4),
    P42 = phonenumber:set_national_number(301234, P41),
    fixed_line = phonenumber_util:get_number_type(P42).

is_fixed_line_and_mobile_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(6502531111, P11),
    fixed_line_or_mobile = phonenumber_util:get_number_type(P12),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(54, P2),
    P22 = phonenumber:set_national_number(1987654321, P21),
    unknown = phonenumber_util:get_number_type(P22).

is_shared_cost_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(44, P1),
    P12 = phonenumber:set_national_number(8431231234, P11),
    shared_cost = phonenumber_util:get_number_type(P12).

is_personal_number_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(44, P1),
    P12 = phonenumber:set_national_number(7031231234, P11),
    personal_number = phonenumber_util:get_number_type(P12).

is_voip_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(44, P1),
    P12 = phonenumber:set_national_number(5623456789, P11),
    voip = phonenumber_util:get_number_type(P12).

is_unknown_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(1, P1),
    P12 = phonenumber:set_national_number(65025311111, P11),
    unknown = phonenumber_util:get_number_type(P12).

get_country_code_for_region_test() ->
    1 = phonenumber_util:get_country_code_for_region(<<"US">>), 
    64 = phonenumber_util:get_country_code_for_region(<<"NZ">>),
    0 = phonenumber_util:get_country_code_for_region(<<"ZZ">>), %% RegionCode::GetUnknown()
    0 = phonenumber_util:get_country_code_for_region(<<"001">>),
    %% CS is already deprecated so the library doesn't support it.
    0 = phonenumber_util:get_country_code_for_region(<<"CS">>).

get_ndd_prefix_for_region_test() ->
    <<"1">> = phonenumber_util:get_ndd_prefix_for_region(<<"US">>, false), 
    <<"1">> = phonenumber_util:get_ndd_prefix_for_region(<<"BS">>, false), 
    <<"0">> = phonenumber_util:get_ndd_prefix_for_region(<<"NZ">>, false), 
    <<>> = phonenumber_util:get_ndd_prefix_for_region(<<"AO">>, false),
    <<>> = phonenumber_util:get_ndd_prefix_for_region(<<"AO">>, true),
    <<"">> = phonenumber_util:get_ndd_prefix_for_region(<<"ZZ">>, false),
    <<"">> = phonenumber_util:get_ndd_prefix_for_region(<<"001">>, false),
    <<"">> = phonenumber_util:get_ndd_prefix_for_region(<<"CS">>, false).

country_with_no_number_desc_test() ->
    %% Andorra is a country where we don't have PhoneNumberDesc info in the
    %% metadata.
    AD = phonenumber:new(),
    AD1 = phonenumber:set_country_code(376, AD),
    AD12 = phonenumber:set_national_number(12345, AD1),
    <<"+376 12345">> = phonenumber_util:format(AD12, international),
    <<"+37612345">> = phonenumber_util:format(AD12, e164), 
    <<"12345">> = phonenumber_util:format(AD12, national), 
    unknown = phonenumber_util:get_number_type(AD12),
    false = phonenumber_util:is_valid_number(AD12),

    %% Test dialing a US number from within Andorra.
    US = phonenumber:new(),
    US1 = phonenumber:set_country_code(1, US),
    US12 = phonenumber:set_national_number(6502530000, US1),
    <<"00 1 650-253-0000">> = phonenumber_util:format_out_of_country_calling_number(US12, <<"AD">>). 

unknown_country_calling_code_test() ->
    P = phonenumber:new(),
    P1 = phonenumber:set_country_code(kInvalidCountryCode, P),
    P12 = phonenumber:set_national_number(12345, P1),

    false = phonenumber_util:is_valid_number(P12),

    %% It's not very well defined as to what the E164 representation for a number
    %% with an invalid country calling code is, but just prefixing the country
    %% code and national number is about the best we can do.

    <<"+012345">> = phonenumber_util:format(P12, e164).

is_number_match_test() ->
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(64, P1),
    P12 = phonenumber:set_national_number(33316005, P11),

    P2 = phonenumber:new(),
    P21 = phonenumber:set_country_code(64, P2),
    P22 = phonenumber:set_national_number(33316005, P21),

    exact_match = phonenumber_util:is_number_match(P12, P22). 

is_number_match_with_two_strings_test() ->
    exact_match = phonenumber_util:is_number_match_with_two_strings(<<"+64 3 331 6005">>, <<"+64 03 331 6005">>), 
    exact_match = phonenumber_util:is_number_match_with_two_strings(<<"+64 3 331-6005">>, <<"tel:+64-3-331-6005;isub=123">>).

is_number_match_with_one_string_test() -> 
    P1 = phonenumber:new(),
    P11 = phonenumber:set_country_code(64, P1),
    P12 = phonenumber:set_national_number(33316005, P11),
    P13 = phonenumber:set_extension(<<"3456">>, P12),

    exact_match = phonenumber_util:is_number_match_with_one_string(P13, <<"+643 331 6005 ext 3456">>), 

    P14 = phonenumber:clear_extension(P13),  
    exact_match = phonenumber_util:is_number_match_with_one_string(P14, <<"+643 331 6005">>).