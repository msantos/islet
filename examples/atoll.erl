-module(atoll).

-export([
        create/1,
        destroy/1
    ]).

create(N) ->
    {ok, Ref} = islet:start(),
    spinup(Ref, N).

spinup(_Ref, 0) ->
    ok;
spinup(Ref, N) ->
    Name = "atoll-" ++ integer_to_list(N),
    ok = islet:create(Ref, [
            {name, Name},
            {setup, setup(Name)},
            {islet, islet()}
        ]),
    spinup(Ref, N-1).

destroy(N) ->
    {ok, Ref} = islet:start(),
    islet:destroy(Ref, "atoll-" ++ integer_to_list(N)).

setup(Name) ->
    Default = islet:setup(Name),
    Default ++ "
cat<<'EOF'>/tmp/udhcpc.script
#!/bin/static-sh

env
case \"$1\" in
	deconfig)
        ip addr flush dev $interface
        ;;

	renew|bound)
        # flush all the routes
        if [ -n \"$router\" ]; then
            ip route del default 2> /dev/null
        fi

        # check broadcast
        if [ -n \"$broadcast\" ]; then
            broadcast=\"broadcast $broadcast\"
        fi

        # add a new ip address
        ip addr add $ip/$mask $broadcast dev $interface

        if [ -n \"$router\" ]; then
            ip route add default via $router dev $interface
        fi
        ;;
esac
EOF
chmod +x /tmp/udhcpc.script
busybox udhcpc -s /tmp/udhcpc.script
".

islet() ->
"#!/bin/sh

cd $HOME
IPADDR=`ifconfig eth0 | gawk -F'[: ]+' '/inet addr/{ print $4 }'`
echo IPADDR=$IPADDR
erl -setcookie COOKIESSS -name islet@$IPADDR
".
