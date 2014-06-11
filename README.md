elibphonenumber
===============

Erlang port for use [libphonenumber 5.9](https://code.google.com/p/libphonenumber/) from erlang (in development)

## Documentation

[doc](http://artefactop.github.io/elibphonenumber/ "documentation")

## Compile and test
You need install libphonenumber-dev for compile and libphonenumber5 for run

```bash
svn checkout http://libphonenumber.googlecode.com/svn/trunk/ libphonenumber-read-only
cd libphonenumber-read-only
#install dependencies, less cpp/README
dpkg-buildpackage
cd ..
sudo dpkg -i libphonenumber-dev_5.9.2_amd64.deb
sudo dpkg -i libphonenumber5_5.9.2_amd64.deb
```

```bash
rebar compile eunit
```

NOTE: Maybe you have to copy some headers files manualy 
```
libphonenumber-read-only/cpp/src/phonenumbers$ sudo cp phonenumber.h /usr/include/phonenumbers/
libphonenumber-read-only/cpp/src/phonenumbers$ sudo cp base/template_util.h /usr/include/phonenumbers/base/
libphonenumber-read-only/cpp/src/phonenumbers$ sudo cp base/logging.h /usr/include/phonenumbers/base/
libphonenumber-read-only/cpp/src/phonenumbers$ sudo cp base/thread_checker.h /usr/include/phonenumbers/base/
libphonenumber-read-only/cpp/src/phonenumbers$ sudo cp base/memory/singleton_posix.h /usr/include/phonenumbers/base/memory/
```


TODO 

- finish phonenumber_util:format_by_pattern

