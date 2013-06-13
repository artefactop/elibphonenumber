-module(libphonenumber_test).










test_is_alpha_number() ->
	true = libphonenumber_util:is_alpha_number(<<"1800 six-flags">>), 
	true = libphonenumber_util:is_alpha_number(<<"1800 six-flags ext. 1234">>), 
	true = libphonenumber_util:is_alpha_number(<<"+800 six-flags">>), 
	true = libphonenumber_util:is_alpha_number(<<"180 six-flags">>), 
	false = libphonenumber_util:is_alpha_number(<<"1800 123-1234">>), 
	false = libphonenumber_util:is_alpha_number(<<"1 six-flags">>), 
	false = libphonenumber_util:is_alpha_number(<<"18 six-flags">>), 
	false = libphonenumber_util:is_alpha_number(<<"1800 123-1234 extension: 1234">>), 
	false = libphonenumber_util:is_alpha_number(<<"+800 1234-1234">>).