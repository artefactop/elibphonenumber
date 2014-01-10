-module(phonenumber).

-include("../include/libphonenumber.hrl").

 -export([
    new/0,
    has_country_code/1,
    get_country_code/1,
    set_country_code/2,
    clear_country_code/1,
    has_national_number/1,
    get_national_number/1,
    set_national_number/2,
    clear_national_number/1,
    has_extension/1,
    get_extension/1,
    set_extension/2,
    clear_extension/1,
    set_italian_leading_zero/2,
%   is_italian_leading_zero/1,
%   clear_italian_leading_zero/1,
%   has_raw_input/1,
%   get_raw_input/1,
    set_raw_input/2,
%   clear_raw_input/1,
%   has_country_code_source/1,
%   get_country_code_source/1,
%   set_country_code_source/2,
%   clear_country_code_source/1,
%   has_preferred_domestic_carrier_code/1,
%   get_preferred_domestic_carrier_code/1,
    set_preferred_domestic_carrier_code/2,
%   clear_preferred_domestic_carrier_code/1,
    clear/1
%   merge_from/2,
%   exactly_same_as/2,
%   hash_code/1
    ]).

-spec new() -> PhoneNumber::phonenumber().

new() ->
    #phonenumber{
        has_country_code = false,
        country_code = 0,
        has_national_number = false,
        national_number = 0,
        has_extension = false,
        extension = <<>>,
        has_number_of_leading_zeros = false,
        number_of_leading_zeros = 1,
        has_italian_leading_zero = false,
        italian_leading_zero = false,
        has_raw_input = false,
        raw_input = <<>>,
        has_country_code_source = false,
        country_code_source = from_number_with_plus_sign,
        has_preferred_domestic_carrier_code = false,
        preferred_domestic_carrier_code = <<>>
    }.

%% country_code

-spec has_country_code(PhoneNumber::phonenumber()) -> boolean().

has_country_code(#phonenumber{has_country_code=Hcc}) ->
    Hcc.

-spec get_country_code(PhoneNumber::phonenumber()) -> non_neg_integer().

get_country_code(#phonenumber{country_code=CC}) ->
    CC.

-spec set_country_code(CountryCode::non_neg_integer(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_country_code(_CountryCode, #phonenumber{}=PhoneNumber) when is_integer(_CountryCode)  ->
    PhoneNumber#phonenumber{has_country_code = true, country_code = _CountryCode};
set_country_code(_CountryCode, #phonenumber{}=PhoneNumber) ->
    PhoneNumber.

-spec clear_country_code(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

clear_country_code(#phonenumber{}=PhoneNumber) -> 
    PhoneNumber#phonenumber{has_country_code=false, country_code = 0}.

%% national_number

-spec has_national_number(PhoneNumber::phonenumber()) -> boolean().

has_national_number(#phonenumber{has_national_number=Hnn}) ->
    Hnn.

-spec get_national_number(PhoneNumber::phonenumber()) -> non_neg_integer().

get_national_number(#phonenumber{national_number=NN}) ->
    NN.

-spec set_national_number(NationalNumber::non_neg_integer(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_national_number(_NationalNumber, #phonenumber{}=PhoneNumber) when is_integer(_NationalNumber) ->
    PhoneNumber#phonenumber{has_national_number=true, national_number = _NationalNumber};
set_national_number(_NationalNumber, #phonenumber{}=PhoneNumber) ->
    PhoneNumber.

-spec clear_national_number(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

clear_national_number(#phonenumber{}=PhoneNumber) -> 
    PhoneNumber#phonenumber{has_national_number=false, national_number = 0}.

%% extension

-spec has_extension(PhoneNumber::phonenumber()) -> boolean().

has_extension(#phonenumber{has_extension=Hex}) ->
    Hex.

-spec get_extension(PhoneNumber::phonenumber()) -> binary().

get_extension(#phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber.extension.

-spec set_extension(Extension::binary(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_extension(_Extension, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{has_extension = true, extension = _Extension}.

-spec clear_extension(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

clear_extension(#phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{has_extension=false, extension = <<>>}.

% -spec has_italian_leading_zero(PhoneNumber::phonenumber()) -> boolean().
% -spec is_italian_leading_zero(PhoneNumber::phonenumber()) -> boolean().

-spec set_italian_leading_zero(ItalianLeadingZero::boolean(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_italian_leading_zero(_ItalianLeadingZero, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{has_italian_leading_zero = true, italian_leading_zero = _ItalianLeadingZero}.

-spec clear_italian_leading_zero(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

clear_italian_leading_zero(#phonenumber{}=PhoneNumber) -> 
    PhoneNumber#phonenumber{has_italian_leading_zero = false, italian_leading_zero = false}.

% -spec has_raw_input(PhoneNumber::phonenumber()) -> boolean().
% -spec get_raw_input(PhoneNumber::phonenumber()) -> binary().
-spec set_raw_input(RawInput::binary(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_raw_input(_RawInput, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{has_raw_input=true, raw_input = _RawInput}.

% -spec clear_raw_input(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().



% -spec has_country_code_source(PhoneNumber::phonenumber()) -> boolean().
% -spec get_country_code_source(PhoneNumber::phonenumber()) -> non_neg_integer().
% -spec set_country_code_source(CountryCodeSource::non_neg_integer(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().
% -spec clear_country_code_source(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().



% -spec has_preferred_domestic_carrier_code(PhoneNumber::phonenumber()) -> boolean().
% -spec get_preferred_domestic_carrier_code(PhoneNumber::phonenumber()) -> binary().
-spec set_preferred_domestic_carrier_code(
    PreferredDomesticCarrierCode::binary(),
    PhoneNumber::phonenumber()
    ) -> NewPhoneNumber::phonenumber().

set_preferred_domestic_carrier_code(_PreferredDomesticCarrierCode, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{
        has_preferred_domestic_carrier_code = true,
        preferred_domestic_carrier_code = _PreferredDomesticCarrierCode
    }.

% -spec clear_preferred_domestic_carrier_code(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().



-spec clear(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

clear(#phonenumber{}) ->
    new().

% -spec merge_from(PhoneNumberFrom::phonenumber(), PhoneNumberTo::phonenumber()) -> NewPhoneNumber::phonenumber().

% -spec exactly_same_as(PhoneNumber::phonenumber(), PhoneNumberOther::phonenumber()) -> boolean().

% -spec hash_code(PhoneNumber::phonenumber()) -> Hash::non_neg_integer().