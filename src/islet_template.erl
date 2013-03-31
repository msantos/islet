-module(islet_template).
-compile(export_all).

name() ->
    {N1,N2,N3} = erlang:now(),
    io_lib:format("c-~w-~w-~w", [N1,N2,N3]).

memory() ->
    131072.

description() ->
    "islet".

interface(Ctx) ->
    Name = mustache:get(name, Ctx),

    Iface = [
        "br0"
    ],

    Interfaces = [ {Dev, islet:macaddr(Name ++ "-" ++ Dev)} || Dev <- Iface ],

    [ dict:from_list([{source, Source}, {macaddr, Address}])
        || {Source, Address} <- Interfaces ].

filesystem(Ctx) ->
    Path = mustache:get(path, Ctx),
    Name = mustache:get(name, Ctx),

    Filesystems = [
        [{source, Path ++ "/" ++ Name ++ "/rootfs"},
         {target, "/"},
         {readonly, true}]
    ] ++ islet:filesystems(),

    [ dict:from_list(proplists:unfold(FS))
        || FS <- Filesystems ].

readonly() ->
    true.
