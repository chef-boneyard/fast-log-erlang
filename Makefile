## DEPS = deps/foo deps/bar
DEPS =

all: compile eunit dialyzer

clean:
	@rebar clean

distclean:
	@rebar skip_deps=true clean
	@rm -rf deps

compile: $(DEPS)
	@rebar compile

dialyzer:
	@dialyzer -Wrace_conditions -Wunderspecs -r ebin

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

test: eunit
