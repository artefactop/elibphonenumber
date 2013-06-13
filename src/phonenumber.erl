-module(phonenumber).

-include("../include/libphonenumber.hrl").

 -export([
    new/0,
%   has_country_code/1,
%   get_country_code/1,
    set_country_code/2,
%   clear_country_code/1,
%   has_national_number/1,
%   get_national_number/1,
    set_national_number/2,
%   clear_national_number/1,
%   has_extension/1,
%   get_extension/1,
%   set_extension/2,
%   clear_extension/1,
    set_italian_leading_zero/2,
%   is_italian_leading_zero/1,
%   clear_italian_leading_zero/1,
%   has_raw_input/1,
%   get_raw_input/1,
   set_raw_input/2
%   clear_raw_input/1,
%   has_country_code_source/1,
%   get_country_code_source/1,
%   set_country_code_source/2,
%   clear_country_code_source/1,
%   has_preferred_domestic_carrier_code/1,
%   get_preferred_domestic_carrier_code/1,
%   set_preferred_domestic_carrier_code/2,
%   clear_preferred_domestic_carrier_code/1,
%   clear/1,
%   merge_from/2,
%   exactly_same_as/2,
%   hash_code/1
    ]).

-spec new() -> PhoneNumber::phonenumber().

new() ->
    #phonenumber{
        national_number = 0,
        country_code = 0,
        italian_leading_zero = false,
        extension = <<"">>,
        raw_input = <<"">>,
        preferred_domestic_carrier_code = <<"">>,
        country_code_source = 0
    }.

% -spec has_country_code(PhoneNumber::phonenumber()) -> boolean().
% -spec get_country_code(PhoneNumber::phonenumber()) -> non_neg_integer().

-spec set_country_code(CountryCode::non_neg_integer(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_country_code(_CountryCode, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{country_code = _CountryCode}.

% -spec clear_country_code(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().


% -spec has_national_number(PhoneNumber::phonenumber()) -> boolean().
% -spec get_national_number(PhoneNumber::phonenumber()) -> non_neg_integer().

-spec set_national_number(NationalNumber::non_neg_integer(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_national_number(_NationalNumber, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{national_number = _NationalNumber}.

% -spec clear_national_number(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().


% -spec has_extension(PhoneNumber::phonenumber()) -> boolean().
% -spec get_extension(PhoneNumber::phonenumber()) -> binary().
% -spec set_extension(Extension::binary(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().
% -spec clear_extension(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().


% -spec has_italian_leading_zero(PhoneNumber::phonenumber()) -> boolean().
% -spec is_italian_leading_zero(PhoneNumber::phonenumber()) -> boolean().

-spec set_italian_leading_zero(ItalianLeadingZero::boolean(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_italian_leading_zero(_ItalianLeadingZero, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{italian_leading_zero = _ItalianLeadingZero}.

% -spec clear_italian_leading_zero(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().


% -spec has_raw_input(PhoneNumber::phonenumber()) -> boolean().
% -spec get_raw_input(PhoneNumber::phonenumber()) -> binary().
-spec set_raw_input(RawInput::binary(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

set_raw_input(_RawInput, #phonenumber{}=PhoneNumber) ->
    PhoneNumber#phonenumber{raw_input = _RawInput}.

% -spec clear_raw_input(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().



% -spec has_country_code_source(PhoneNumber::phonenumber()) -> boolean().
% -spec get_country_code_source(PhoneNumber::phonenumber()) -> non_neg_integer().
% -spec set_country_code_source(CountryCodeSource::non_neg_integer(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().
% -spec clear_country_code_source(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().



% -spec has_preferred_domestic_carrier_code(PhoneNumber::phonenumber()) -> boolean().
% -spec get_preferred_domestic_carrier_code(PhoneNumber::phonenumber()) -> binary().
% -spec set_preferred_domestic_carrier_code(PreferredDomesticCarrierCode::binary(), PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().
% -spec clear_preferred_domestic_carrier_code(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().



% -spec clear(PhoneNumber::phonenumber()) -> NewPhoneNumber::phonenumber().

% -spec merge_from(PhoneNumberFrom::phonenumber(), PhoneNumberTo::phonenumber()) -> NewPhoneNumber::phonenumber().

% -spec exactly_same_as(PhoneNumber::phonenumber(), PhoneNumberOther::phonenumber()) -> boolean().

% -spec hash_code(PhoneNumber::phonenumber()) -> Hash::non_neg_integer().