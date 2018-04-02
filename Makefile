PROJECT = xaptum-tpm-erlang
REBAR = rebar3

compile:
	$(REBAR) compile

test: compile
	$(REBAR) eunit

clean:
	rm -rf _build
	rm -rf priv/xaptum-tpm-erlang.so
