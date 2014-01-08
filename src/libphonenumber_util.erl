-module(libphonenumber_util).
-export([
    hello/1,
    get_supported_regions/0,
    get_region_codes_for_country_calling_code/1,
    is_alpha_number/1,
    convert_alpha_characters_in_number/1,
    normalize_digits_only/1,
    get_national_significant_number/1,
    get_length_of_geograpical_area_code/1,
    format/2,
    format_by_pattern/3,
    format_national_number_with_carrier_code/2,
    format_national_number_with_preferred_carrier_code/2,
    format_number_for_mobile_dialing/3,
    format_out_of_country_calling_number/2,
    format_in_original_format/2,
    format_out_of_country_keeping_alpha_chars/2,
    truncate_too_long_number/1,
    get_number_type/1,
    is_valid_number/1,
    is_valid_number_for_region/2,
    get_region_code_for_number/1,
    get_country_code_for_region/1,
    get_region_code_for_country_code/1,
    is_nanpa_country/1,
    get_ndd_prefix_for_region/2,
    is_possible_number_with_reason/1,
    is_possible_number/1,
    is_possible_number_for_string/2,
    get_example_number/1,
    get_example_number_for_type/2,
    get_example_number_for_non_geo_entity/1,
    parse/2,
    parse_and_keep_raw_input/2,
    is_number_match/2,
    is_number_match_with_two_strings/2,
    is_number_match_with_one_string/2
    ]).

-on_load(init/0).

-include("../include/libphonenumber.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% local functions:

init() ->
    case erlang:system_info(smp_support) of
        true ->
            SoName = filename:join(priv_dir(), ?MODULE),
            case erlang:load_nif(filename:absname(SoName)++"_nif", 0) of 
                ok -> ok;
                {error, {reload, _}} -> ok;
                {error, {upgrade, _}} -> ok
            end;
        false ->
            error(no_smp_support)
    end.

priv_dir() ->
    case code:priv_dir(libphonenumber) of
        PrivDir when is_list(PrivDir) ->
            PrivDir;
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(Ebin), "priv")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%TEST
hello(_) ->
      "NIF library not loaded".

-spec get_supported_regions() -> list(binary()).

%% @doc Convenience method to get a list of what regions the library has metadata
%% for.

get_supported_regions() ->
    exit(nif_library_not_loaded).

-spec get_region_codes_for_country_calling_code(
    CountryCallingCode::non_neg_integer()
    ) -> list(binary()).

get_region_codes_for_country_calling_code(_CountryCallingCode) ->
    exit(nif_library_not_loaded).

-spec is_alpha_number(Number::binary()) -> boolean().

%% @doc Returns true if the number is a valid vanity (alpha) number such as 800
%% MICROSOFT. A valid vanity number will start with at least 3 digits and will
%% have three or more alpha characters. This does not do region-specific
%% checks - to work out if this number is actually valid for a region, it
%% should be parsed and methods such as IsPossibleNumberWithReason or
%% IsValidNumber should be used.

is_alpha_number(_Number) ->
    exit(nif_library_not_loaded).

-spec convert_alpha_characters_in_number(Number::binary()) -> binary().

%% @doc Converts all alpha characters in a number to their respective digits on
%% a keypad, but retains existing formatting.

convert_alpha_characters_in_number(_Number) ->
    exit(nif_library_not_loaded).

-spec normalize_digits_only(Number::binary()) -> binary().

%% @doc Normalizes a string of characters representing a phone number. This
%% converts wide-ascii and arabic-indic numerals to European numerals, and
%% strips punctuation and alpha characters.

normalize_digits_only(_Number) ->
    exit(nif_library_not_loaded).

-spec get_national_significant_number(
    PhoneNumber::phonenumber()
    ) -> NationalSignificantNum::binary().

%% @doc Gets the national significant number of a phone number. Note a national
%% significant number doesn't contain a national prefix or any formatting.

get_national_significant_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec get_length_of_geograpical_area_code(
    PhoneNumber::phonenumber()
    ) -> non_neg_integer().

get_length_of_geograpical_area_code(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec format(
    PhoneNumber::phonenumber(), 
    PhoneNumberFormat::phonenumber_format()
    ) -> FormattedNumber::binary().

%% @doc Formats a phone number in the specified format using default rules. Note
%% that this does not promise to produce a phone number that the user can
%% dial from where they are - although we do format in either NATIONAL or
%% INTERNATIONAL format depending on what the client asks for, we do not
%% currently support a more abbreviated format, such as for users in the
%% same area who could potentially dial the number without area code.

format(_PhoneNumber, _PhoneNumberFormat) ->
    exit(nif_library_not_loaded).

-spec format_by_pattern(
    PhoneNumber::phonenumber(),
    PhoneNumberFormat::phonenumber_format(),
    UserDefinedFormats::list(phonenumber_format())
    ) -> FormattedNumber::binary(). %%TODO

format_by_pattern(_PhoneNumber, _PhoneNumberFormat, _UserDefinedFormats) ->
    exit(nif_library_not_loaded).

-spec format_national_number_with_carrier_code(
    PhoneNumber::phonenumber(),
    CarrierCode::binary()
    ) -> FormattedNumber::binary().

format_national_number_with_carrier_code(_PhoneNumber, _CarrierCode) ->
    exit(nif_library_not_loaded).

-spec format_national_number_with_preferred_carrier_code(
    PhoneNumber::phonenumber(),
    FallbackCarrierCode::binary()
    ) -> FormattedNumber::binary().

format_national_number_with_preferred_carrier_code(_PhoneNumber, _FallbackCarrierCode) ->
    exit(nif_library_not_loaded).

-spec format_number_for_mobile_dialing(
    PhoneNumber::phonenumber(),
    RegionCallingFrom::binary(),
    WithFormatting::boolean()
    ) -> FormattedNumber::binary(). %%TODO

format_number_for_mobile_dialing(_PhoneNumber, _RegionCallingFrom, _WithFormatting) ->
    exit(nif_library_not_loaded).

-spec format_out_of_country_calling_number(
    PhoneNumber::phonenumber(),
    CallingFrom::binary()
    ) -> FormattedNumber::binary().

format_out_of_country_calling_number(_PhoneNumber, _CallingFrom) ->
    exit(nif_library_not_loaded).

-spec format_in_original_format(
    PhoneNumber::phonenumber(),
    RegionCallingFrom::binary()
    ) -> FormattedNumber::binary().

format_in_original_format(_PhoneNumber, _RegionCallingFrom) ->
    exit(nif_library_not_loaded).

-spec format_out_of_country_keeping_alpha_chars(
    PhoneNumber::phonenumber(),
    CallingFrom::binary()
    ) -> FormattedNumber::binary().

format_out_of_country_keeping_alpha_chars(_PhoneNumber, _CallingFrom) ->
    exit(nif_library_not_loaded).

-spec truncate_too_long_number(
    PhoneNumber::phonenumber()
    ) -> ValidPhoneNumber::phonenumber() | {error, no_valid_number}.

truncate_too_long_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec get_number_type(PhoneNumber::phonenumber()) -> phonenumber_type().

get_number_type(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec is_valid_number(PhoneNumber::phonenumber()) -> boolean().

is_valid_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec is_valid_number_for_region(
    PhoneNumber::phonenumber(),
    Region::binary()
    ) -> boolean().

is_valid_number_for_region(_PhoneNumber, _Region) ->
    exit(nif_library_not_loaded).

-spec get_region_code_for_number(
    PhoneNumber::phonenumber()
    ) -> RegionCode::binary().

%% @doc Returns the region where a phone number is from. This could be used for
%% geo-coding at the region level.

get_region_code_for_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec get_country_code_for_region(
    RegionCode::binary()
    ) -> CountryCode::non_neg_integer().

%% @doc Returns the country calling code for a specific region. For example,
%% this would be 1 for the United States, and 64 for New Zealand.

get_country_code_for_region(_RegionCode) ->
    exit(nif_library_not_loaded).

-spec get_region_code_for_country_code(
    CountryCode::non_neg_integer()
    ) -> RegionCode::binary().

get_region_code_for_country_code(_CountryCode) ->
    exit(nif_library_not_loaded).

-spec is_nanpa_country(RegionCode::binary()) -> boolean().

is_nanpa_country(_RegionCode) ->
    exit(nif_library_not_loaded).

-spec get_ndd_prefix_for_region(
    RegionCode::binary(), 
    StripNonDigits::boolean()
    ) -> NationalPrefix::binary().

%% @doc Returns the national dialling prefix for a specific region. For example,
%% this would be 1 for the United States, and 0 for New Zealand. Set
%% strip_non_digits to true to strip symbols like "~" (which indicates a wait
%% for a dialling tone) from the prefix returned. If no national prefix is
%% present, we return an empty string.

get_ndd_prefix_for_region(_RegionCode, _StripNonDigits) ->
    exit(nif_library_not_loaded).

-spec is_possible_number_with_reason(
    PhoneNumber::phonenumber()
    ) -> ValidationResult::validation_result().

%% @doc Checks whether a phone number is a possible number. It provides a more
%% lenient check than IsValidNumber() in the following sense:
%%   1. It only checks the length of phone numbers. In particular, it doesn't
%%      check starting digits of the number.
%%   2. It doesn't attempt to figure out the type of the number, but uses
%%      general rules which applies to all types of phone numbers in a
%%      region. Therefore, it is much faster than IsValidNumber().
%%   3. For fixed line numbers, many regions have the concept of area code,
%%      which together with subscriber number constitute the national
%%      significant number. It is sometimes okay to dial the subscriber
%%      number only when dialing in the same area. This function will return
%%      true if the subscriber-number-only version is passed in. On the other
%%      hand, because IsValidNumber() validates using information on both
%%      starting digits (for fixed line numbers, that would most likely be
%%      area codes) and length (obviously includes the length of area codes
%%      for fixed line numbers), it will return false for the
%%      subscriber-number-only version.

is_possible_number_with_reason(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec is_possible_number(
    PhoneNumber::phonenumber()
    ) -> boolean().

%% @doc Convenience wrapper around IsPossibleNumberWithReason. Instead of returning
%% the reason for failure, this method returns a boolean value.

is_possible_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec is_possible_number_for_string(
    Number::binary(),
    RegionDialingFrom::binary()
    ) -> boolean().

%% @doc Checks whether a phone number is a possible number given a number in the
%% form of a string, and the country where the number could be dialed from.
%% It provides a more lenient check than is_valid_number/1. 
%% See is_possible_number/1 for details.
%% 
%% This method first parses the number, then invokes
%% is_possible_number with the resultant PhoneNumber
%% object.
%% 
%% region_dialing_from represents the region that we are expecting the number
%% to be dialed from. Note this is different from the region where the number
%% belongs. For example, the number +1 650 253 0000 is a number that belongs
%% to US. When written in this form, it could be dialed from any region. When
%% it is written as 00 1 650 253 0000, it could be dialed from any region
%% which uses an international dialling prefix of 00. When it is written as
%% 650 253 0000, it could only be dialed from within the US, and when written
%% as 253 0000, it could only be dialed from within a smaller area in the US
%% (Mountain View, CA, to be more specific).
%% @see is_possible_number. 

is_possible_number_for_string(_Number, _RegionDialingFrom) ->
    exit(nif_library_not_loaded).

-spec get_example_number(
    RegionCode::binary()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_region}.

%% @doc Gets a valid fixed-line number for the specified region. Returns {error, unknown_region} if
%% the region was unknown, or the region 001 is passed in. For 001
%% (representing non-geographical numbers), call
%% GetExampleNumberForNonGeoEntity instead.

get_example_number(_RegionCode) ->
    exit(nif_library_not_loaded).

-spec get_example_number_for_type(
    RegionCode::binary(),
    PhoneNumberType::phonenumber_type()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_region}.


get_example_number_for_type(_RegionCode, _PhoneNumberType) ->
    exit(nif_library_not_loaded).

-spec get_example_number_for_non_geo_entity(
    CountryCallingCode::binary()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_code}.

get_example_number_for_non_geo_entity(_CountryCallingCode) ->
    exit(nif_library_not_loaded).

-spec parse(
    NumberToParse::binary(),
    DefaultRegion::binary()
    ) -> PhoneNumber::phonenumber() | {error, term()}.

parse(_NumberToParse, _DefaultRegion) ->
    exit(nif_library_not_loaded).

-spec parse_and_keep_raw_input(
    NumberToParse::binary(),
    DefaultRegion::binary()
    ) -> PhoneNumber::phonenumber() | {error, term()}.

%% @doc Parses a string and returns it in proto buffer format. This method differs
%% from parse/2 in that it always populates the raw_input field of the
%% protocol buffer with number_to_parse as well as the country_code_source
%% field.

parse_and_keep_raw_input(_NumberToParse, _DefaultRegion) ->
    exit(nif_library_not_loaded).

-spec is_number_match(
    FirstNumber::phonenumber(),
    SecondNumber::phonenumber()
    ) -> match_type().

is_number_match(_FirstNumber, _SecondNumber) ->
    exit(nif_library_not_loaded).

-spec is_number_match_with_two_strings(
    FirstNumber::binary(),
    SecondNumber::binary()
    ) -> match_type().

is_number_match_with_two_strings(_FirstNumber, _SecondNumber) ->
    exit(nif_library_not_loaded).

-spec is_number_match_with_one_string(
    FirstNumber::phonenumber(),
    SecondNumber::binary()
    ) -> match_type().

is_number_match_with_one_string(_FirstNumber, _SecondNumber) ->
    exit(nif_library_not_loaded).