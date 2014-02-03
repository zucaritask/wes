all: compile
	./rebar skip_deps=true xref

compile:
	./rebar compile

eunit: all
	./rebar skip_deps=true compile verbose=1 eunit

init_dialyzer:
	dialyzer --apps stdlib kernel erts -r deps --build_plt --output_plt .dialyzer.plt

check: compile
	dialyzer --no_native -Wno_undefined_callbacks -Wno_return -r ebin --plt .dialyzer.plt

clean:
	./rebar clean
