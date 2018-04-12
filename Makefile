REBAR = rebar3

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref

test: compile
	$(REBAR) eunit

clean:
	rm -rf _build
	rm -rf priv/xaptum_tpm_erlang.so
