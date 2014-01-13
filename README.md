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

TODO 

- finish phonenumber_util:format_by_pattern