
make:
	./rebar co


cl:
	./rebar clean


st:
	erl +pc unicode +K true -pa ebin/


def:
	rm -rf src/auto/def
	escript t_def priv/def/ src/auto/def/


proto:
	rm -rf src/auto/proto
	escript t_proto priv/proto/ src/auto/proto/ priv/docroot ../../etc/def/global


mysql:
	erl -pa ebin/ ../../deps/*/ebin ../../apps/*/ebin ../../lib/*/ebin -config ../../etc/conf/gm/sys_192.168.2.51_26004 -pa -s tab_lookup main
	#-s erlang halt