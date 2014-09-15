DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

all: compile xref eunit dialyze

compile:
	@./rebar compile

xref:
	@./rebar xref

clean:
	@./rebar clean

eunit:
	@./rebar eunit

edoc:
	@./rebar doc

start: compile
	erl -pa ebin

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib compiler crypto

dialyze: .dialyzer.plt compile
	dialyzer --plt .dialyzer.plt -r ebin $(DIALYZER_OPTS)
