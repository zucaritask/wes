all:
	./rebar compile
	./rebar skip_deps=true xref

eunit: all
	./rebar skip_deps=true compile verbose=1 eunit
