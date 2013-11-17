Simple, safe isolation using Erlang.

QUICK USAGE
-----------

```erlang
Env = islet:env(),
ok = islet:prepare(Env),
{ok, Ref} = islet:spawn(Env),
ok = islet:send(Ref, <<"echo echo\n">>).
{ok,<<"echo echo\r\necho echo\r\n">>}
```

EXAMPLES
--------

Spawn a Container
=================

This example creates a new container when a client connects over a TCP/IP socket on port 31337:

```erlang
-module(tcpvm).

-export([
        start/0, start/1
    ]).

start() ->
    start([]).

start(Options) ->
    Port = proplists:get_value(port, Options, 31337),

    {ok, LSock} = gen_tcp:listen(Port, [binary,{active,false},{reuseaddr,true}]),
    accept(LSock, Options).

accept(LSock, Options) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    Pid = spawn(fun() -> shell(Socket, Options) end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    accept(LSock, Options).

shell(Socket, Options) ->
    Console = self(),
    Pid = spawn_link(fun() -> socket(Socket, Console) end),
    ok = gen_tcp:controlling_process(Socket, Pid),

    Env = islet:env(Options),
    ok = islet:prepare(Env),
    {ok, Ref} = islet:spawn(Env),

    islet:console(Ref, Pid, fun(Data) -> gen_tcp:send(Socket, Data) end).

socket(Socket, Console) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            Console ! {islet_tty, self(), Data},
            socket(Socket, Console);
        {tcp_closed, Socket} ->
            error_logger:error_report([{socket, closed}]),
            ok;
        {tcp_error, Socket, Error} ->
            error_logger:error_report([{socket, Error}]),
            ok
    end.
```

Connect to the socket using netcat:

```shell
hypervisor$ nc localhost 31337
$ hostname
hostname
islet-4026565933
$ ^C
hypervisor$ nc localhost 31337
$ hostname
hostname
islet-4026582938
$ ^C
```
