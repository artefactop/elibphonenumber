-module(phonenumber_util).
-export([
    hello/1,
    get_supported_regions/0,
    is_alpha_number/1,
    convert_alpha_characters_in_number/1,
    normalize_digits_only/1,
    %normalize_diallable_chars_only/1,
    get_national_significant_number/1,
    get_length_of_geograpical_area_code/1,
    %get_length_of_national_destination_code/1,
    %get_country_mobile_token/1,
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
    get_region_codes_for_country_calling_code/1,
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

%% @doc TEST.

hello(_) ->
      "NIF library not loaded".

-spec get_supported_regions() -> list(binary()).

%% @doc Convenience method to get a list of what regions the library has metadata
%% for.

get_supported_regions() ->
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

%% @doc TODO

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
    ) -> FormattedNumber::binary(). 

%% @doc TODO

format_by_pattern(_PhoneNumber, _PhoneNumberFormat, _UserDefinedFormats) ->
    exit(nif_library_not_loaded).

-spec format_national_number_with_carrier_code(
    PhoneNumber::phonenumber(),
    CarrierCode::binary()
    ) -> FormattedNumber::binary().

%% @doc Formats a phone number in national format for dialing using the carrier as
%% specified in the carrier_code. The carrier_code will always be used
%% regardless of whether the phone number already has a preferred domestic
%% carrier code stored. If carrier_code contains an empty string, return the
%% number in national format without any carrier code.

format_national_number_with_carrier_code(_PhoneNumber, _CarrierCode) ->
    exit(nif_library_not_loaded).

-spec format_national_number_with_preferred_carrier_code(
    PhoneNumber::phonenumber(),
    FallbackCarrierCode::binary()
    ) -> FormattedNumber::binary().

%% @doc Formats a phone number in national format for dialing using the carrier as
%% specified in the preferred_domestic_carrier_code field of the PhoneNumber
%% object passed in. If that is missing, use the fallback_carrier_code passed
%% in instead. If there is no preferred_domestic_carrier_code, and the
%% fallback_carrier_code contains an empty string, return the number in
%% national format without any carrier code.
%%
%% Use FormatNationalNumberWithCarrierCode instead if the carrier code passed
%% in should take precedence over the number's preferred_domestic_carrier_code
%% when formatting.

format_national_number_with_preferred_carrier_code(_PhoneNumber, _FallbackCarrierCode) ->
    exit(nif_library_not_loaded).

-spec format_number_for_mobile_dialing(
    PhoneNumber::phonenumber(),
    RegionCallingFrom::binary(),
    WithFormatting::boolean()
    ) -> FormattedNumber::binary().

%% @doc Returns a number formatted in such a way that it can be dialed from a
%% mobile phone in a specific region. If the number cannot be reached from
%% the region (e.g. some countries block toll-free numbers from being called
%% outside of the country), the method returns an empty string.

format_number_for_mobile_dialing(_PhoneNumber, _RegionCallingFrom, _WithFormatting) ->
    exit(nif_library_not_loaded).

-spec format_out_of_country_calling_number(
    PhoneNumber::phonenumber(),
    CallingFrom::binary()
    ) -> FormattedNumber::binary().

%% @doc Formats a phone number for out-of-country dialing purposes.
%%
%% Note this function takes care of the case for calling inside of NANPA
%% and between Russia and Kazakhstan (who share the same country calling
%% code). In those cases, no international prefix is used. For regions which
%% have multiple international prefixes, the number in its INTERNATIONAL
%% format will be returned instead.

format_out_of_country_calling_number(_PhoneNumber, _CallingFrom) ->
    exit(nif_library_not_loaded).

-spec format_in_original_format(
    PhoneNumber::phonenumber(),
    RegionCallingFrom::binary()
    ) -> FormattedNumber::binary().

%% @doc Formats a phone number using the original phone number format that the
%% number is parsed from. The original format is embedded in the
%% country_code_source field of the PhoneNumber object passed in. If such
%% information is missing, the number will be formatted into the NATIONAL
%% format by default. When the number is an invalid number, the method returns
%% the raw input when it is available.

format_in_original_format(_PhoneNumber, _RegionCallingFrom) ->
    exit(nif_library_not_loaded).

-spec format_out_of_country_keeping_alpha_chars(
    PhoneNumber::phonenumber(),
    CallingFrom::binary()
    ) -> FormattedNumber::binary().

%% @doc Formats a phone number for out-of-country dialing purposes.
%%
%% Note that in this version, if the number was entered originally using alpha
%% characters and this version of the number is stored in raw_input, this
%% representation of the number will be used rather than the digit
%% representation. Grouping information, as specified by characters such as
%% "-" and " ", will be retained.
%%
%% Caveats:
%% 1) This will not produce good results if the country calling code is both
%% present in the raw input _and_ is the start of the national number. This
%% is not a problem in the regions which typically use alpha numbers.
%% 2) This will also not produce good results if the raw input has any
%% grouping information within the first three digits of the national number,
%% and if the function needs to strip preceding digits/words in the raw input
%% before these digits. Normally people group the first three digits together
%% so this is not a huge problem - and will be fixed if it proves to be so.

format_out_of_country_keeping_alpha_chars(_PhoneNumber, _CallingFrom) ->
    exit(nif_library_not_loaded).

-spec truncate_too_long_number(
    PhoneNumber::phonenumber()
    ) -> ValidPhoneNumber::phonenumber() | {error, no_valid_number}.

%% @doc Attempts to extract a valid number from a phone number that is too long to
%% be valid, and resets the PhoneNumber object passed in to that valid
%% version. If no valid number could be extracted, the PhoneNumber object
%% passed in will not be modified. It returns true if a valid phone number can
%% be successfully extracted.

truncate_too_long_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec get_number_type(PhoneNumber::phonenumber()) -> phonenumber_type().

%% @doc Gets the type of a phone number.

get_number_type(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec is_valid_number(PhoneNumber::phonenumber()) -> boolean().

%% @doc Tests whether a phone number matches a valid pattern. Note this doesn't
%% verify the number is actually in use, which is impossible to tell by just
%% looking at a number itself.

is_valid_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec is_valid_number_for_region(
    PhoneNumber::phonenumber(),
    Region::binary()
    ) -> boolean().

%% @doc Tests whether a phone number is valid for a certain region. Note this
%% doesn't verify the number is actually in use, which is impossible to tell
%% by just looking at a number itself. If the country calling code is not the
%% same as the country calling code for the region, this immediately exits
%% with false. After this, the specific number pattern rules for the region
%% are examined.
%% This is useful for determining for example whether a particular number is
%% valid for Canada, rather than just a valid NANPA number.
%% Warning: In most cases, you want to use IsValidNumber instead. For
%% example, this method will mark numbers from British Crown dependencies
%% such as the Isle of Man as invalid for the region "GB" (United Kingdom),
%% since it has its own region code, "IM", which may be undesirable.

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

%% @doc Returns the region code that matches the specific country code. Note that
%% it is possible that several regions share the same country calling code
%% (e.g. US and Canada), and in that case, only one of the regions (normally
%% the one with the largest population) is returned. If the
%% countryCallingCode entered is valid but doesn't match a specific region
%% (such as in the case of non-geographical calling codes like 800) the
%% RegionCode 001 will be returned (corresponding to the value for World in
%% the UN M.49 schema).

get_region_code_for_country_code(_CountryCode) ->
    exit(nif_library_not_loaded).

-spec get_region_codes_for_country_calling_code(
    CountryCallingCode::non_neg_integer()
    ) -> list(binary()).

%% @doc Populates a list with the region codes that match the specific country
%% calling code. For non-geographical country calling codes, the region code
%% 001 is returned. Also, in the case of no region code being found, the list
%% is left unchanged.

get_region_codes_for_country_calling_code(_CountryCallingCode) ->
    exit(nif_library_not_loaded).

-spec is_nanpa_country(RegionCode::binary()) -> boolean().

%% @doc Checks if this is a region under the North American Numbering Plan
%% Administration (NANPA).

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

%% @doc TODO Checks whether a phone number is a possible number. It provides a more
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

%% @doc TODO Convenience wrapper around IsPossibleNumberWithReason. Instead of returning
%% the reason for failure, this method returns a boolean value.

is_possible_number(_PhoneNumber) ->
    exit(nif_library_not_loaded).

-spec is_possible_number_for_string(
    Number::binary(),
    RegionDialingFrom::binary()
    ) -> boolean().

%% @doc TODO Checks whether a phone number is a possible number given a number in the
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

%% @doc TODO Gets a valid fixed-line number for the specified region. Returns {error, unknown_region} if
%% the region was unknown, or the region 001 is passed in. For 001
%% (representing non-geographical numbers), call
%% GetExampleNumberForNonGeoEntity instead.

get_example_number(_RegionCode) ->
    exit(nif_library_not_loaded).

-spec get_example_number_for_type(
    RegionCode::binary(),
    PhoneNumberType::phonenumber_type()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_region}.

%% @doc TODO

get_example_number_for_type(_RegionCode, _PhoneNumberType) ->
    exit(nif_library_not_loaded).

-spec get_example_number_for_non_geo_entity(
    CountryCallingCode::binary()
    ) -> ValidPhoneNumber::phonenumber() | {error, unknown_code}.

%% @doc TODO

get_example_number_for_non_geo_entity(_CountryCallingCode) ->
    exit(nif_library_not_loaded).

-spec parse(
    NumberToParse::binary(),
    DefaultRegion::binary()
    ) -> PhoneNumber::phonenumber() | {error, term()}.

%% @doc Parses a string and returns it in proto buffer format. This method will
%% return an error like INVALID_COUNTRY_CODE if the number is not considered
%% to be a possible number, and NO_PARSING_ERROR if it parsed correctly. Note
%% that validation of whether the number is actually a valid number for a
%% particular region is not performed. This can be done separately with
%% IsValidNumber().
%%
%% number_to_parse can also be provided in RFC3966 format.
%%
%% default_region represents the country that we are expecting the number to
%% be from. This is only used if the number being parsed is not written in
%% international format. The country_code for the number in this case would be
%% stored as that of the default country supplied. If the number is guaranteed
%% to start with a '+' followed by the country calling code, then
%% "ZZ" can be supplied.

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

%% @doc TODO

is_number_match(_FirstNumber, _SecondNumber) ->
    exit(nif_library_not_loaded).

-spec is_number_match_with_two_strings(
    FirstNumber::binary(),
    SecondNumber::binary()
    ) -> match_type().

%% @doc TODO

is_number_match_with_two_strings(_FirstNumber, _SecondNumber) ->
    exit(nif_library_not_loaded).

-spec is_number_match_with_one_string(
    FirstNumber::phonenumber(),
    SecondNumber::binary()
    ) -> match_type().

%% @doc TODO

is_number_match_with_one_string(_FirstNumber, _SecondNumber) ->
    exit(nif_library_not_loaded).