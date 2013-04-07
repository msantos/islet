-module(atoll).

-export([
        create/1, create/2,
        destroy/1, destroy/2
    ]).

create(N) ->
    create("atoll-", N).

create(Prefix, N) when is_list(Prefix) ->
    {ok, Ref} = islet:start(),
    spinup(Prefix, Ref, N).

spinup(_Prefix, Ref, 0) ->
    islet:stop(Ref);
spinup(Prefix, Ref, N) ->
    Name = Prefix ++ integer_to_list(N),
    ok = islet:create(Ref, [
            {name, Name},
            {setup, setup(Name)},
            {islet, islet()}
        ]),
    spinup(Prefix, Ref, N-1).

destroy(N) ->
    destroy("atoll-", N).

destroy(Prefix, N) when is_list(Prefix) ->
    {ok, Ref} = islet:start(),
    destroy(Prefix, Ref, N).

destroy(_Prefix, Ref, 0) ->
    islet:stop(Ref);
destroy(Prefix, Ref, N) ->
    islet:destroy(Ref, Prefix ++ integer_to_list(N)),
    destroy(N-1).

setup(Name) ->
    Default = islet:setup(Name),
    Default ++ "
mkdir /tmp/udhcpc
cat<<'EOF'>/tmp/udhcpc/script
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
chmod +x /tmp/udhcpc/script
busybox udhcpc -s /tmp/udhcpc/script
".

islet() ->
"#!/bin/sh

cd $HOME
IPADDR=`ifconfig eth0 | gawk -F'[: ]+' '/inet addr/{ print $4 }'`
echo IPADDR=$IPADDR
erl -setcookie COOKIESSS -name islet@$IPADDR
".
