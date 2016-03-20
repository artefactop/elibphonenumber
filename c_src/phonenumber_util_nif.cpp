#include <erl_nif.h>
#include <phonenumber.pb.h>
#include <phonenumberutil.h>
#include <memory>
#include <iostream>
#include <set>
#include <string>

using namespace std;
using namespace i18n::phonenumbers;

// gcc -fPIC -shared -o phonenumber_util_nif.so  phonenumber_util_nif.cpp -I /usr/lib/erlang/erts-5.9.2/include/ -I /usr/include/phonenumbers/ -I /usr/include/ -l phonenumber -L /usr/lib/ -DI18N_PHONENUMBERS_NO_THREAD_SAFETY 



static bool enif_get_phonenumber_format(ErlNifEnv* env, const ERL_NIF_TERM term, PhoneNumberUtil::PhoneNumberFormat* number_format){
    unsigned int len;

    if (!enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1)){
        return false;
    }

    char buf[len+1];
    if (!enif_get_atom(env, term, buf, len+1, ERL_NIF_LATIN1)) {
        return false;
    } 

    if( strcmp (buf, "e164") == 0 ){
        *number_format = PhoneNumberUtil::E164;
    } else if ( strcmp (buf, "international") == 0 ){
        *number_format = PhoneNumberUtil::INTERNATIONAL;
    } else if ( strcmp (buf, "national") == 0 ){
        *number_format = PhoneNumberUtil::NATIONAL;
    } else if ( strcmp (buf, "rfc3966") == 0 ){
        *number_format = PhoneNumberUtil::RFC3966;
    }else{
        return false;
    }

    return true;
}

static bool enif_get_phonenumber_country_code_source(ErlNifEnv* env, const ERL_NIF_TERM term, PhoneNumber::CountryCodeSource* country_code_source){
    unsigned int len;

    if (!enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1)){
        return false;
    }

    char buf[len+1];
    if (!enif_get_atom(env, term, buf, len+1, ERL_NIF_LATIN1)) {
        return false;
    } 

    if( strcmp (buf, "from_number_with_plus_sign") == 0 ){
        *country_code_source = PhoneNumber::FROM_NUMBER_WITH_PLUS_SIGN;
    } else if ( strcmp (buf, "from_number_with_idd") == 0 ){
        *country_code_source = PhoneNumber::FROM_NUMBER_WITH_IDD;
    } else if ( strcmp (buf, "from_number_without_plus_sign") == 0 ){
        *country_code_source = PhoneNumber::FROM_NUMBER_WITHOUT_PLUS_SIGN;
    } else if ( strcmp (buf, "from_default_country") == 0 ){
        *country_code_source = PhoneNumber::FROM_DEFAULT_COUNTRY;
    }else{
        return false;
    }

    return true;
}

static bool enif_get_phonenumber_type(ErlNifEnv* env, const ERL_NIF_TERM term, PhoneNumberUtil::PhoneNumberType* type){
    unsigned int len;

    if (!enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1)){
        return false;
    }

    char buf[len+1];
    if (!enif_get_atom(env, term, buf, len+1, ERL_NIF_LATIN1)) {
        return false;
    } 

    if( strcmp (buf, "fixed_line") == 0 ){
        *type = PhoneNumberUtil::FIXED_LINE;
    } else if ( strcmp (buf, "mobile") == 0 ){
        *type = PhoneNumberUtil::MOBILE;
    } else if ( strcmp (buf, "fixed_line_or_mobile") == 0 ){
        *type = PhoneNumberUtil::FIXED_LINE_OR_MOBILE;
    } else if ( strcmp (buf, "toll_free") == 0 ){
        *type = PhoneNumberUtil::TOLL_FREE;
    } else if ( strcmp (buf, "premium_rate") == 0 ){
        *type = PhoneNumberUtil::PREMIUM_RATE;
    } else if ( strcmp (buf, "shared_cost") == 0 ){
        *type = PhoneNumberUtil::SHARED_COST;
    } else if ( strcmp (buf, "voip") == 0 ){
        *type = PhoneNumberUtil::VOIP;
    } else if ( strcmp (buf, "personal_number") == 0 ){
        *type = PhoneNumberUtil::PERSONAL_NUMBER;
    } else if ( strcmp (buf, "pager") == 0 ){
        *type = PhoneNumberUtil::PAGER;
    } else if ( strcmp (buf, "uan") == 0 ){
        *type = PhoneNumberUtil::UAN;
    } else if ( strcmp (buf, "voicemail") == 0 ){
        *type = PhoneNumberUtil::VOICEMAIL;
    } else if ( strcmp (buf, "unknown") == 0 ){
        *type = PhoneNumberUtil::UNKNOWN;
    }else{
        return false;
    }

    return true;
}

static bool enif_get_boolean(ErlNifEnv* env, const ERL_NIF_TERM term, bool* boolean){
    unsigned int len;    

    if (!enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1)){
        return false;
    }

    char buf[len+1];
    if (!enif_get_atom(env, term, buf, len+1, ERL_NIF_LATIN1)) {
        return false;
    } 

    if ( strcmp (buf, "true") == 0 ){
        *boolean = true;
    } else if ( strcmp (buf, "false") == 0 ){
        *boolean = false;
    } else {
        return false;
    }

    return true;
}

static ERL_NIF_TERM enif_make_boolean(ErlNifEnv* env, bool boolean){
    if (boolean){
        return enif_make_atom(env, "true");
    }else{
        return enif_make_atom(env, "false");
    }
}

static ERL_NIF_TERM enif_make_phonenumber_type(ErlNifEnv* env, PhoneNumberUtil::PhoneNumberType type){

     switch(type){
        case PhoneNumberUtil::FIXED_LINE:
            return enif_make_atom(env, "fixed_line");
        case PhoneNumberUtil::MOBILE:
            return enif_make_atom(env, "mobile");
        case PhoneNumberUtil::FIXED_LINE_OR_MOBILE:
            return enif_make_atom(env, "fixed_line_or_mobile");
        case PhoneNumberUtil::TOLL_FREE:
            return enif_make_atom(env, "toll_free");
        case PhoneNumberUtil::PREMIUM_RATE:
            return enif_make_atom(env, "premium_rate");
        case PhoneNumberUtil::SHARED_COST:
            return enif_make_atom(env, "shared_cost");
        case PhoneNumberUtil::VOIP:
            return enif_make_atom(env, "voip");
        case PhoneNumberUtil::PERSONAL_NUMBER:
            return enif_make_atom(env, "personal_number");
        case PhoneNumberUtil::PAGER:
            return enif_make_atom(env, "pager");
        case PhoneNumberUtil::UAN:
            return enif_make_atom(env, "uan");
        case PhoneNumberUtil::VOICEMAIL:
            return enif_make_atom(env, "voicemail");
        case PhoneNumberUtil::UNKNOWN:
            return enif_make_atom(env, "unknown");
     }

    return enif_make_atom(env, "unknown");
}

static ERL_NIF_TERM enif_make_phonenumber_validation_result(ErlNifEnv* env, PhoneNumberUtil::ValidationResult validation_result){

    switch(validation_result){
        case PhoneNumberUtil::IS_POSSIBLE:
            return enif_make_atom(env, "is_possible");
        case PhoneNumberUtil::INVALID_COUNTRY_CODE:
            return enif_make_atom(env, "invalid_country_code");
        case PhoneNumberUtil::TOO_SHORT:
            return enif_make_atom(env, "too_short");
        case PhoneNumberUtil::TOO_LONG:
            return enif_make_atom(env, "too_long");
    }

    return enif_make_atom(env, "invalid_country_code");
}

static ERL_NIF_TERM enif_make_phonenumber_country_code_source(ErlNifEnv* env, PhoneNumber::CountryCodeSource country_code_source){

    switch(country_code_source){
        case PhoneNumber::FROM_NUMBER_WITH_PLUS_SIGN:
            return enif_make_atom(env, "from_number_with_plus_sign");
        case PhoneNumber::FROM_NUMBER_WITH_IDD:
            return enif_make_atom(env, "from_number_with_idd");
        case PhoneNumber::FROM_NUMBER_WITHOUT_PLUS_SIGN:
            return enif_make_atom(env, "from_number_without_plus_sign");
        case PhoneNumber::FROM_DEFAULT_COUNTRY:
            return enif_make_atom(env, "from_default_country");
    }

    return enif_make_atom(env, "from_default_country");
}

static ERL_NIF_TERM enif_make_phonenumber_match_type(ErlNifEnv* env, PhoneNumberUtil::MatchType match_type){

    switch(match_type){
        case PhoneNumberUtil::INVALID_NUMBER:
            return enif_make_atom(env, "invalid_number");
        case PhoneNumberUtil::NO_MATCH:
            return enif_make_atom(env, "no_match");
        case PhoneNumberUtil::SHORT_NSN_MATCH:
            return enif_make_atom(env, "short_nsn_match");
        case PhoneNumberUtil::NSN_MATCH:
            return enif_make_atom(env, "nsn_match");
        case PhoneNumberUtil::EXACT_MATCH:
            return enif_make_atom(env, "exact_match");
    }

    return enif_make_atom(env, "invalid_number");
}

static ERL_NIF_TERM enif_make_phonenumber(ErlNifEnv* env, PhoneNumber phoneNumber){
    unsigned char *buffer;
    ERL_NIF_TERM raw_input;
    ERL_NIF_TERM extension;
    ERL_NIF_TERM preferred_domestic_carrier_code;

    ERL_NIF_TERM record = enif_make_atom(env, "phonenumber");

    // required int32 country_code = 1;
    ERL_NIF_TERM has_country_code = enif_make_boolean(env, phoneNumber.has_country_code());
    ERL_NIF_TERM country_code = enif_make_int(env, phoneNumber.country_code());

    // required uint64 national_number = 2;
    ERL_NIF_TERM has_national_number = enif_make_boolean(env, phoneNumber.has_national_number());
    ERL_NIF_TERM national_number = enif_make_ulong(env, phoneNumber.national_number());

    // optional string extension = 3;
    ERL_NIF_TERM has_extension = enif_make_boolean(env, phoneNumber.has_extension());
    buffer = enif_make_new_binary(env, phoneNumber.extension().size(), &extension);
    std::copy(phoneNumber.extension().begin(), phoneNumber.extension().end(), buffer);

    // optional bool italian_leading_zero = 4;
    ERL_NIF_TERM has_italian_leading_zero = enif_make_boolean(env, phoneNumber.has_italian_leading_zero());
    ERL_NIF_TERM italian_leading_zero = enif_make_boolean(env, phoneNumber.italian_leading_zero());

    // optional int32 number_of_leading_zeros = 8 [default = 1];
    ERL_NIF_TERM has_number_of_leading_zeros = enif_make_boolean(env, phoneNumber.has_number_of_leading_zeros());
    ERL_NIF_TERM number_of_leading_zeros = enif_make_int(env, phoneNumber.number_of_leading_zeros());
    
    // optional string raw_input = 5;
    ERL_NIF_TERM has_raw_input = enif_make_boolean(env, phoneNumber.has_raw_input());
    buffer = enif_make_new_binary(env, phoneNumber.raw_input().size(), &raw_input);
    std::copy(phoneNumber.raw_input().begin(), phoneNumber.raw_input().end(), buffer);

    // optional .i18n.phonenumbers.PhoneNumber.CountryCodeSource country_code_source = 6;
    ERL_NIF_TERM has_country_code_source = enif_make_boolean(env, phoneNumber.has_country_code_source());
    ERL_NIF_TERM country_code_source = enif_make_phonenumber_country_code_source(env, phoneNumber.country_code_source());

    // optional string preferred_domestic_carrier_code = 7;
    ERL_NIF_TERM has_preferred_domestic_carrier_code = enif_make_boolean(env, phoneNumber.has_preferred_domestic_carrier_code());
    buffer = enif_make_new_binary(env, phoneNumber.preferred_domestic_carrier_code().size(), &preferred_domestic_carrier_code);
    std::copy(phoneNumber.preferred_domestic_carrier_code().begin(), phoneNumber.preferred_domestic_carrier_code().end(), buffer);

    return enif_make_tuple(env, 17,
        record,
        has_country_code,
        country_code,
        has_national_number,
        national_number,
        has_extension,
        extension,
        has_number_of_leading_zeros,
        number_of_leading_zeros,
        has_italian_leading_zero,
        italian_leading_zero,
        has_raw_input,
        raw_input,
        has_country_code_source,
        country_code_source,
        has_preferred_domestic_carrier_code,
        preferred_domestic_carrier_code
    );
}

static bool enif_inspect_phonenumber(ErlNifEnv* env, const ERL_NIF_TERM term, PhoneNumber* phoneNumber){
    int integer;
    unsigned long longer; //FIXME should be unsingned long long
    bool boolean;
    ErlNifBinary bin;
    int PhoneNumberElems = 17;

    const ERL_NIF_TERM* array;

    // Get Term
    if (!enif_get_tuple(env, term, &PhoneNumberElems, &array)){
        return false;
    }

    // required int32 country_code = 1;
    if (!enif_get_int(env, array[2], &integer)) {
        return false;
    }
    phoneNumber->set_country_code(integer);

    // required uint64 national_number = 2;
    if (!enif_get_ulong(env, array[4], &longer)) {
        return false;
    }
    phoneNumber->set_national_number(longer);

    // optional string extension = 3;
    if (!enif_get_boolean(env, array[5], &boolean)) {
        return false;
    }
    if (boolean){
        if (!enif_inspect_iolist_as_binary(env, array[6], &bin)){
            return false;
        }
        phoneNumber->set_extension(std::string ( (char*) bin.data, bin.size));
    }

    // optional int32 number_of_leading_zeros = 8 [default = 1];
    if (!enif_get_boolean(env, array[7], &boolean)) {
        return false;
    }
    if (boolean){
        if (!enif_get_int(env, array[8], &integer)) {
            return false;
        }
        phoneNumber->set_number_of_leading_zeros(integer);
    }

    // optional bool italian_leading_zero = 4;
    if (!enif_get_boolean(env, array[9], &boolean)) {
        return false;
    }
    if (boolean){
        if (!enif_get_boolean(env, array[10], &boolean)) {
            return false;
        }
        phoneNumber->set_italian_leading_zero(boolean);
    }
    //return true;

    // optional string raw_input = 5;
    if (!enif_get_boolean(env, array[11], &boolean)) {
        return false;
    }
    if (boolean){
        if (!enif_inspect_iolist_as_binary(env, array[12], &bin)){
            return false;
        }
        phoneNumber->set_raw_input(std::string ( (char*) bin.data, bin.size));
    }

    // optional .i18n.phonenumbers.PhoneNumber.CountryCodeSource country_code_source = 6;
    if (!enif_get_boolean(env, array[13], &boolean)) {
        return false;
    }
    if (boolean){
        PhoneNumber::CountryCodeSource country_code_source;
        if (!enif_get_phonenumber_country_code_source(env, array[14], &country_code_source)) { //TODO enif_get_country_code_source
            return false;
        }
        phoneNumber->set_country_code_source(country_code_source);
    }

    // Get Preferred Domestic Carrier Code
    if (!enif_get_boolean(env, array[15], &boolean)) {
        return false;
    }
    if (boolean){
        if (!enif_inspect_iolist_as_binary(env, array[16], &bin)){
            return false;
        }
        phoneNumber->set_preferred_domestic_carrier_code(std::string ( (char*) bin.data, bin.size));
    }

    return true;
}

// NIF functions

static ERL_NIF_TERM GetSupportedRegions_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    set<string> regions;
    phone_util_->GetSupportedRegions(&regions);
    unsigned int cnt = regions.size();
    ERL_NIF_TERM arr[cnt];

    unsigned int i = 0;
    ERL_NIF_TERM ret;
    unsigned char *region;

    for (set<string>::iterator it=regions.begin(); it!=regions.end(); ++it, i++){
        region = enif_make_new_binary(env, it->size(), &ret);
        std::copy(it->begin(), it->end(), region);
        arr[i] = ret;
        ret = 0;
    }
    return enif_make_list_from_array(env, arr, cnt);
}

static ERL_NIF_TERM IsAlphaNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    std::string str( (char*) bin.data, bin.size);
    return enif_make_boolean(env, phone_util_->IsAlphaNumber(str));
}

static ERL_NIF_TERM ConvertAlphaCharactersInNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    std::string str( (char*) bin.data, bin.size);
    phone_util_->ConvertAlphaCharactersInNumber(&str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM NormalizeDigitsOnly_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    std::string str( (char*) bin.data, bin.size);
    phone_util_->NormalizeDigitsOnly(&str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM NormalizeDiallableCharsOnly_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    std::string str( (char*) bin.data, bin.size);
    phone_util_->NormalizeDiallableCharsOnly(&str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM GetNationalSignificantNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string str;
    phone_util_->GetNationalSignificantNumber(phoneNumber, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM GetLengthOfGeographicalAreaCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    int length = phone_util_->GetLengthOfGeographicalAreaCode(phoneNumber);
    
    return enif_make_int(env, length);
}

static ERL_NIF_TERM GetLengthOfNationalDestinationCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    int length = phone_util_->GetLengthOfNationalDestinationCode(phoneNumber);
    
    return enif_make_int(env, length);
}

static ERL_NIF_TERM GetCountryMobileToken_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int code;
    if (!enif_get_int(env, argv[0], &code)) {
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    std::string mobile_token;
    
    phone_util_->GetCountryMobileToken(code, &mobile_token);

    ERL_NIF_TERM ret;
    unsigned char *token = enif_make_new_binary(env, mobile_token.size(), &ret);
    std::copy(mobile_token.begin(), mobile_token.end(), token);
    
    return ret;
}

static ERL_NIF_TERM Format_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil::PhoneNumberFormat phoneNumberFormat;
    if (!enif_get_phonenumber_format(env, argv[1], &phoneNumberFormat)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string str;
    phone_util_->Format(phoneNumber, phoneNumberFormat, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

/*
static ERL_NIF_TERM FormatByPattern_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil::PhoneNumberFormat phoneNumberFormat;
    if (!enif_get_phonenumber_format(env, argv[1], &phoneNumberFormat)){
        return enif_make_badarg(env);
    }

    CreatePhoneNumberFromNif(&phoneNumber, &phoneNumber);

    //TODO parse pattern list

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string str;
    phone_util_->Format(phoneNumber, phoneNumberFormat, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}
*/

static ERL_NIF_TERM FormatNationalNumberWithPreferredCarrierCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string fb_carrier_code( (char*) bin.data, bin.size);
    std::string str;
    phone_util_->FormatNationalNumberWithPreferredCarrierCode(phoneNumber, fb_carrier_code, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM FormatNationalNumberWithCarrierCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string carrier_code( (char*) bin.data, bin.size);
    std::string str;
    phone_util_->FormatNationalNumberWithCarrierCode(phoneNumber, carrier_code, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM FormatNumberForMobileDialing_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)){
        return enif_make_badarg(env);
    }

    bool with_formatting;
    if (!enif_get_boolean(env, argv[2], &with_formatting)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string region_calling_from( (char*) bin.data, bin.size);
    std::string str;
    phone_util_->FormatNumberForMobileDialing(phoneNumber, region_calling_from, with_formatting, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM FormatOutOfCountryCallingNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string calling_from( (char*) bin.data, bin.size);
    std::string str;
    phone_util_->FormatOutOfCountryCallingNumber(phoneNumber, calling_from, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM FormatInOriginalFormat_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string region_code( (char*) bin.data, bin.size);
    std::string str;
    phone_util_->FormatInOriginalFormat(phoneNumber, region_code, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM FormatOutOfCountryKeepingAlphaChars_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string calling_from( (char*) bin.data, bin.size);
    std::string str;
    phone_util_->FormatOutOfCountryKeepingAlphaChars(phoneNumber, calling_from, &str);

    ERL_NIF_TERM ret;
    unsigned char *number = enif_make_new_binary(env, str.size(), &ret);
    std::copy(str.begin(), str.end(), number);
    
    return ret;
}

static ERL_NIF_TERM TruncateTooLongNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    phone_util_->TruncateTooLongNumber(&phoneNumber);

    return enif_make_phonenumber(env, phoneNumber);
}

static ERL_NIF_TERM GetNumberType_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    PhoneNumberUtil::PhoneNumberType type = phone_util_->GetNumberType(phoneNumber);

    return enif_make_phonenumber_type(env, type); 
}

static ERL_NIF_TERM IsValidNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    bool valid = phone_util_->IsValidNumber(phoneNumber);

    return enif_make_boolean(env, valid);
}

static ERL_NIF_TERM IsValidNumberForRegion_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin)){
        return enif_make_badarg(env);
    }


    std::string region_code( (char*) bin.data, bin.size);

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    bool valid = phone_util_->IsValidNumberForRegion(phoneNumber, region_code);

    return enif_make_boolean(env, valid);
}

static ERL_NIF_TERM GetRegionCodeForNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    std::string region_code;
    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    phone_util_->GetRegionCodeForNumber(phoneNumber, &region_code);

    ERL_NIF_TERM ret;
    unsigned char *region = enif_make_new_binary(env, region_code.size(), &ret);
    std::copy(region_code.begin(), region_code.end(), region);
    
    return ret;
}

static ERL_NIF_TERM GetCountryCodeForRegion_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string region_code( (char*) bin.data, bin.size);
    int code = phone_util_->GetCountryCodeForRegion(region_code);
    
    return enif_make_int(env, code);
}

static ERL_NIF_TERM GetRegionCodeForCountryCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int code;
    if (!enif_get_int(env, argv[0], &code)) {
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    std::string region_code;
    
    phone_util_->GetRegionCodeForCountryCode(code, &region_code);

    ERL_NIF_TERM ret;
    unsigned char *region = enif_make_new_binary(env, region_code.size(), &ret);
    std::copy(region_code.begin(), region_code.end(), region);
    
    return ret;
}

static ERL_NIF_TERM GetRegionCodesForCountryCallingCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int code;
    if (!enif_get_int(env, argv[0], &code)) {
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    list<string> regions;

    phone_util_->GetRegionCodesForCountryCallingCode(code, &regions);
    unsigned int cnt = regions.size();
    ERL_NIF_TERM arr[cnt];

    unsigned int i = 0;
    ERL_NIF_TERM ret;
    unsigned char *region;

    for (list<string>::iterator it=regions.begin(); it!=regions.end(); ++it, i++){
        region = enif_make_new_binary(env, it->size(), &ret);
        std::copy(it->begin(), it->end(), region);
        arr[i] = ret;
        ret = 0;
    }
    return enif_make_list_from_array(env, arr, cnt);
}

static ERL_NIF_TERM IsNANPACountry_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary regionCodeNif;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &regionCodeNif)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string region_code( (char*) regionCodeNif.data, regionCodeNif.size);
    bool nanpa = phone_util_->IsNANPACountry(region_code);

    return enif_make_boolean(env, nanpa);
}

static ERL_NIF_TERM GetNddPrefixForRegion_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary regionCodeNif;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &regionCodeNif)){
        return enif_make_badarg(env);
    }

    bool stripNonDigits;
    if (!enif_get_boolean(env, argv[1], &stripNonDigits)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    std::string region_code( (char*) regionCodeNif.data, regionCodeNif.size);
    std::string national_prefix;
    phone_util_->GetNddPrefixForRegion(region_code, stripNonDigits, &national_prefix);

    ERL_NIF_TERM ret;
    unsigned char *prefix = enif_make_new_binary(env, national_prefix.size(), &ret);
    std::copy(national_prefix.begin(), national_prefix.end(), prefix);
    
    return ret;
}

static ERL_NIF_TERM IsPossibleNumberWithReason_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    PhoneNumberUtil::ValidationResult validation_result = phone_util_->IsPossibleNumberWithReason(phoneNumber);

    return enif_make_phonenumber_validation_result(env, validation_result);
}

static ERL_NIF_TERM IsPossibleNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    bool possible = phone_util_->IsPossibleNumber(phoneNumber);

    return enif_make_boolean(env, possible);
}

static ERL_NIF_TERM IsPossibleNumberForString_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary nifNumber;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &nifNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary nifRegionDialingFrom;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &nifRegionDialingFrom)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string region_dialing_from( (char*) nifRegionDialingFrom.data, nifRegionDialingFrom.size);
    std::string number( (char*) nifNumber.data, nifNumber.size);
    bool possible = phone_util_->IsPossibleNumberForString(number, region_dialing_from);

    return enif_make_boolean(env, possible);
}

static ERL_NIF_TERM GetExampleNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;
    std::string region_code( (char*) bin.data, bin.size);

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    bool result = phone_util_->GetExampleNumber(region_code, &phoneNumber);

    if (result){
        return enif_make_phonenumber(env, phoneNumber);
    }

    return enif_make_boolean(env, "false");
}

static ERL_NIF_TERM GetExampleNumberForType_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil::PhoneNumberType type;
    if (!enif_get_phonenumber_type(env, argv[1], &type)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;
    std::string region_code( (char*) bin.data, bin.size);

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    bool result = phone_util_->GetExampleNumberForType(region_code, type, &phoneNumber);

    if (result){
        return enif_make_phonenumber(env, phoneNumber);
    }

    return enif_make_boolean(env, false);
}

static ERL_NIF_TERM GetExampleNumberForNonGeoEntity_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int code;
    if (!enif_get_int(env, argv[0], &code)) {
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    bool result = phone_util_->GetExampleNumberForNonGeoEntity(code, &phoneNumber);

    if (result){
        return enif_make_phonenumber(env, phoneNumber);
    }

    return enif_make_boolean(env, false);
}

static ERL_NIF_TERM Parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary numberToParse;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &numberToParse)){
        return enif_make_badarg(env);
    }

    ErlNifBinary defaultRegion;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &defaultRegion)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string region_code( (char*) defaultRegion.data, defaultRegion.size);
    std::string number_to_parse( (char*) numberToParse.data, numberToParse.size);
    phone_util_->Parse(number_to_parse, region_code, &phoneNumber);

    return enif_make_phonenumber(env, phoneNumber);
}

static ERL_NIF_TERM ParseAndKeepRawInput_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary numberToParse;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &numberToParse)){
        return enif_make_badarg(env);
    }

    ErlNifBinary defaultRegion;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &defaultRegion)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string region_code( (char*) defaultRegion.data, defaultRegion.size);
    std::string number_to_parse( (char*) numberToParse.data, numberToParse.size);
    phone_util_->ParseAndKeepRawInput(number_to_parse, region_code, &phoneNumber);

    return enif_make_phonenumber(env, phoneNumber);
}

static ERL_NIF_TERM IsNumberMatch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber1;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber1)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber2;
    if (!enif_inspect_phonenumber(env, argv[1], &phoneNumber2)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    PhoneNumberUtil::MatchType match_type = phone_util_->IsNumberMatch(phoneNumber1, phoneNumber2);

    return enif_make_phonenumber_match_type(env, match_type);
}

static ERL_NIF_TERM IsNumberMatchWithTwoStrings_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary nifFirstNumber;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &nifFirstNumber)){
        return enif_make_badarg(env);
    }

    ErlNifBinary nifSecondNumber;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &nifSecondNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string first_number( (char*) nifFirstNumber.data, nifFirstNumber.size);
    std::string second_number( (char*) nifSecondNumber.data, nifSecondNumber.size);
    PhoneNumberUtil::MatchType match_type =phone_util_->IsNumberMatchWithTwoStrings(first_number, second_number);

    return enif_make_phonenumber_match_type(env, match_type);
}

static ERL_NIF_TERM IsNumberMatchWithOneString_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumber phoneNumber1;
    if (!enif_inspect_phonenumber(env, argv[0], &phoneNumber1)){
        return enif_make_badarg(env);
    }

    ErlNifBinary nifSecondNumber;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &nifSecondNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    std::string second_number( (char*) nifSecondNumber.data, nifSecondNumber.size);
    PhoneNumberUtil::MatchType match_type = phone_util_->IsNumberMatchWithOneString(phoneNumber1, second_number);

    return enif_make_phonenumber_match_type(env, match_type);
}

//TEST
static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool boolean;
    if (!enif_get_boolean(env, argv[0], &boolean))
        return enif_make_badarg(env);

    return enif_make_boolean(env, boolean);
}

static ErlNifFunc nif_funcs[] = {
    {"get_supported_regions", 0, GetSupportedRegions_nif},
    {"is_alpha_number", 1, IsAlphaNumber_nif},
    {"convert_alpha_characters_in_number", 1, ConvertAlphaCharactersInNumber_nif},
    {"normalize_digits_only", 1, NormalizeDigitsOnly_nif},
    {"normalize_diallable_chars_only", 1, NormalizeDiallableCharsOnly_nif},
    {"get_national_significant_number", 1, GetNationalSignificantNumber_nif},
    {"get_length_of_geograpical_area_code", 1, GetLengthOfGeographicalAreaCode_nif},
    {"get_length_of_national_destination_code", 1, GetLengthOfNationalDestinationCode_nif},
    {"get_country_mobile_token", 1, GetCountryMobileToken_nif},
    {"format", 2, Format_nif},
    //{"format_by_pattern", 3, FormatByPattern_nif},
    {"format_national_number_with_carrier_code", 2, FormatNationalNumberWithCarrierCode_nif},
    {"format_national_number_with_preferred_carrier_code", 2, FormatNationalNumberWithPreferredCarrierCode_nif},
    {"format_number_for_mobile_dialing", 3, FormatNumberForMobileDialing_nif},
    {"format_out_of_country_calling_number", 2, FormatOutOfCountryCallingNumber_nif},
    {"format_in_original_format", 2, FormatInOriginalFormat_nif},
    {"format_out_of_country_keeping_alpha_chars", 2, FormatOutOfCountryKeepingAlphaChars_nif},
    {"truncate_too_long_number", 1, TruncateTooLongNumber_nif},
    {"get_number_type", 1, GetNumberType_nif},
    {"is_valid_number", 1, IsValidNumber_nif},
    {"is_valid_number_for_region", 2, IsValidNumberForRegion_nif},
    {"get_region_code_for_number", 1, GetRegionCodeForNumber_nif},
    {"get_country_code_for_region", 1, GetCountryCodeForRegion_nif},
    {"get_region_code_for_country_code", 1, GetRegionCodeForCountryCode_nif},
    {"get_region_codes_for_country_calling_code", 1, GetRegionCodesForCountryCallingCode_nif},
    {"is_nanpa_country", 1, IsNANPACountry_nif},
    {"get_ndd_prefix_for_region", 2, GetNddPrefixForRegion_nif},
    {"is_possible_number_with_reason", 1, IsPossibleNumberWithReason_nif},
    {"is_possible_number", 1, IsPossibleNumber_nif},
    {"is_possible_number_for_string", 2, IsPossibleNumberForString_nif},
    {"get_example_number", 1, GetExampleNumber_nif},
    {"get_example_number_for_type", 2, GetExampleNumberForType_nif},
    {"get_example_number_for_non_geo_entity", 1, GetExampleNumberForNonGeoEntity_nif},
    {"parse", 2, Parse_nif},
    {"parse_and_keep_raw_input", 2, ParseAndKeepRawInput_nif},
    {"is_number_match", 2, IsNumberMatch_nif},
    {"is_number_match_with_two_strings", 2, IsNumberMatchWithTwoStrings_nif},
    {"is_number_match_with_one_string", 2, IsNumberMatchWithOneString_nif},

    {"hello", 1, hello}
};





ERL_NIF_INIT(phonenumber_util, nif_funcs, NULL, NULL, NULL, NULL)
