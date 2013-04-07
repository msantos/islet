Screenshot
----------

Create 100 distributed Erlang nodes:

    $ ./start.sh
    Eshell V5.10.1  (abort with ^G)
    1> atoll:create(100).
    ok

Each node runs in with a unique uid in a namespace:

    43181    22702 21498  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.153
    53370    23665 21693  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.110
    41601    23695 21758  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.211
    55533    23731 21823  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.229
    38116    23783 21954  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.240
    61452    23828 21889  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.171
    54930    23847 22017  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.179
    41887    23909 22083  0 15:58 pts/0    00:00:00 /usr/local/lib/erlang/erts-5.10.1/bin/beam.smp -- -root /usr/local/lib/erlang -progname erl -- -home /home/islet -- -setcookie COOKIESSS -name islet@192.168.213.127
    <...>

The containers are created by libvirt:

    $ verx list --uri=lxc:///

    {ok,[{running,[{<<"atoll-25">>,
                    [{uuid,"a4a37a95-92d3-04ca-f698-285dbecab525"},{id,28220}]},
                   {<<"atoll-82">>,
                    [{uuid,"66c9e129-e517-ef07-bcdc-afd97c06880c"},{id,22680}]},
                   {<<"atoll-7">>,
                    [{uuid,"601ba750-eb1b-a765-6bd9-16d8eccd2ffb"},{id,30812}]},
                   {<<"atoll-23">>,
                    [{uuid,"9006cd14-e889-4315-ba6e-ce3ea924ab2c"},{id,28360}]},
                   {<<"atoll-50">>,
                    [{uuid,"89fa178a-effd-c8f8-8c79-331b1048bd1a"},{id,26440}]},
                   {<<"atoll-87">>,
                    [{uuid,"53125f7d-17f5-bdff-ede7-1ccf4f1b1ef2"},{id,22346}]},
                   {<<"atoll-21">>,
    <...>

The isolated nodes can be reached using Erlang distribution:

    (islet@192.168.213.54)5> rpc:call('islet@192.168.213.190', os, cmd, ["hostname"]).   
    "atoll-1\n"
    (islet@192.168.213.54)6> rpc:call('islet@192.168.213.191', os, cmd, ["hostname"]).
    "atoll-43\n"
    (islet@192.168.213.54)7> rpc:call('islet@192.168.213.192', os, cmd, ["hostname"]).
    "atoll-12\n"
