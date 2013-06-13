#include <erl_nif.h>
#include <phonenumber.h>
#include <phonenumberutil.h>
#include <memory>
#include <iostream>
#include <set>
#include <string>

using namespace std;
using namespace i18n::phonenumbers;

// gcc -fPIC -shared -o libphonenumber_util_nif.so  libphonenumber_util_nif.cpp -I /usr/lib/erlang/erts-5.9.2/include/ -I /usr/include/phonenumbers/ -I /usr/include/ -l phonenumber -L /usr/lib/ -DI18N_PHONENUMBERS_NO_THREAD_SAFETY 

typedef struct {
    unsigned long long national_number;
    int country_code;
    bool italian_leading_zero;
    std::string extension;
    std::string raw_input;
    std::string preferred_domestic_carrier_code;
    unsigned int country_code_source;
} ErlNifPhoneNumber;


static bool enif_get_phonenumber_format(ErlNifEnv* env, const ERL_NIF_TERM term, PhoneNumberUtil::PhoneNumberFormat* number_format){
    unsigned int len;
    char* buf;

    if (!enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1)){
        return false;
    }

    if (!enif_get_atom(env, term, buf, len, ERL_NIF_LATIN1)) {
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

static bool enif_get_boolean(ErlNifEnv* env, const ERL_NIF_TERM term, bool* boolean){
    unsigned int len;
    char* buf;

    if (!enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1)){
        return false;
    }  

    if (!enif_get_atom(env, term, buf, len, ERL_NIF_LATIN1)) {
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

static bool enif_inspect_phonenumber(ErlNifEnv* env, const ERL_NIF_TERM term, ErlNifPhoneNumber* nifPhoneNumber){
    int integer;
    unsigned long longer; //FIXME should be unsingned long long
    bool boolean;
    ErlNifBinary bin;
    int erlNifPhoneNumberElems = 8;

    const ERL_NIF_TERM* array;

    // Get Term
    if (!enif_get_tuple(env, term, &erlNifPhoneNumberElems, &array)){
        return false;
    }

    // Get National Number
    if (!enif_get_ulong(env, array[1], &longer)) {
        return false;
    }
    nifPhoneNumber->national_number = longer;

    // Get Country Code
    if (!enif_get_int(env, array[2], &integer)) {
        return false;
    }
    nifPhoneNumber->country_code = integer;

    // Get Italian Leading Zero
    if (!enif_get_boolean(env, array[3], &boolean)) {
        return false;
    }
    nifPhoneNumber->italian_leading_zero = boolean;
return true;
    // Get Extension
    if (!enif_inspect_iolist_as_binary(env, array[4], &bin)){
        return false;
    }
    nifPhoneNumber->extension = std::string ( (char*) bin.data, bin.size);

    // Get Raw Input
    if (!enif_inspect_iolist_as_binary(env, array[5], &bin)){
        return false;
    }
    nifPhoneNumber->raw_input = std::string ( (char*) bin.data, bin.size);

    // Get Preferred Domestic Carrier Code
    if (!enif_inspect_iolist_as_binary(env, array[6], &bin)){
        return false;
    }
    nifPhoneNumber->preferred_domestic_carrier_code = std::string ( (char*) bin.data, bin.size);

    // Get Country Code Source
    if (!enif_get_int(env, array[7], &integer)) {
        return false;
    }
    nifPhoneNumber->country_code_source = integer;

    return true;
}


static void CreatePhoneNumberFromNif(ErlNifPhoneNumber* nifPhoneNumber, PhoneNumber* phoneNumber){
    phoneNumber->set_country_code(nifPhoneNumber->country_code);
    phoneNumber->set_national_number(nifPhoneNumber->national_number);
    phoneNumber->set_italian_leading_zero(nifPhoneNumber->italian_leading_zero);
    phoneNumber->set_extension(nifPhoneNumber->extension);
    phoneNumber->set_raw_input(nifPhoneNumber->raw_input);
    phoneNumber->set_preferred_domestic_carrier_code(nifPhoneNumber->preferred_domestic_carrier_code);
    if (::i18n::phonenumbers::PhoneNumber_CountryCodeSource_IsValid(nifPhoneNumber->country_code_source)) {
            phoneNumber->set_country_code_source(static_cast< ::i18n::phonenumbers::PhoneNumber_CountryCodeSource >(nifPhoneNumber->country_code_source));
    }
}


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
        region[0] = (*it)[0]; 
        region[1] = (*it)[1];
        arr[i] = ret;
        ret = 0;
    }
    return enif_make_list_from_array(env, arr, cnt);
}

static ERL_NIF_TERM GetRegionCodesForCountryCallingCode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();

    list<string> regions;
    int code;
    if (!enif_get_int(env, argv[0], &code)) {
        return enif_make_badarg(env);
    }
    phone_util_->GetRegionCodesForCountryCallingCode(code, &regions);
    unsigned int cnt = regions.size();
    ERL_NIF_TERM arr[cnt];

    unsigned int i = 0;
    ERL_NIF_TERM ret;
    unsigned char *region;

    for (list<string>::iterator it=regions.begin(); it!=regions.end(); ++it, i++){
        region = enif_make_new_binary(env, it->size(), &ret);
        region[0] = (*it)[0]; 
        region[1] = (*it)[1];
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

static ERL_NIF_TERM GetNationalSignificantNumber_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPhoneNumber nifPhoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &nifPhoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;
    CreatePhoneNumberFromNif(&nifPhoneNumber, &phoneNumber);

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
    ErlNifPhoneNumber nifPhoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &nifPhoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;
    CreatePhoneNumberFromNif(&nifPhoneNumber, &phoneNumber);

    PhoneNumberUtil *phone_util_ = PhoneNumberUtil::GetInstance();
    int length = phone_util_->GetLengthOfGeographicalAreaCode(phoneNumber);
    
    return enif_make_int(env, length);
}

static ERL_NIF_TERM Format_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPhoneNumber nifPhoneNumber;
    if (!enif_inspect_phonenumber(env, argv[0], &nifPhoneNumber)){
        return enif_make_badarg(env);
    }

    PhoneNumber phoneNumber;
    CreatePhoneNumberFromNif(&nifPhoneNumber, &phoneNumber);

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
    {"get_region_codes_for_country_calling_code", 1, GetRegionCodesForCountryCallingCode_nif},
    {"is_alpha_number", 1, IsAlphaNumber_nif},
    {"convert_alpha_characters_in_number", 1, ConvertAlphaCharactersInNumber_nif},
    {"normalize_digits_only", 1, NormalizeDigitsOnly_nif},
    {"get_national_significant_number", 1, GetNationalSignificantNumber_nif},
    {"get_length_of_geograpical_area_code", 1, GetLengthOfGeographicalAreaCode_nif},
    {"format", 2, Format_nif},

    {"hello", 1, hello}
};





ERL_NIF_INIT(libphonenumber_util, nif_funcs, NULL, NULL, NULL, NULL)