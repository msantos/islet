%%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(islet).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include_lib("verx/include/verx.hrl").
-include_lib("islet/include/islet.hrl").

-export([
        env/0, env/1,
        prepare/1,

        spawn/1, spawn/2,
        kill/1,

        send/2,
        recv/1, recv/2,

        script/1, script/2,
        console/1, console/3
    ]).
-export([
        env_to_xml/1
    ]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(conn, {
        pid,
        domain
    }).

-record(state, {
        pid,
        env = [],
        cfg,
        conn = #conn{}
    }).

-define(ISLET_VERSION, "0.2.1").

env() ->
    env([]).

env(Options) ->
    Name = proplists:get_value(name, Options, default(name)),

    Memory = proplists:get_value(memory, Options, default(memory)),
    Description = proplists:get_value(memory, Options, default(description)),

    Interfaces = proplists:get_value(interface, Options, []),

    SandboxPath = proplists:get_value(sandbox_path, Options, priv_dir("/islets/" ++ Name)),
    ChrootPath = proplists:get_value(chroot_path, Options, priv_dir("/rootfs")),

    Exec = proplists:get_value(exec, Options, priv_dir("/islet_exec")),

    Init = proplists:get_value(init, Options, script(init, [{name, Name}])),
    Islet = proplists:get_value(islet, Options, script(islet)),

    #islet{
        system = [
                {name, Name},
                {memory, Memory},
                {description, Description}
            ],

        interface = Interfaces,

        sandbox = #islet_root{
            path = SandboxPath,
            directory = [
                "/boot",
                "/etc/default",
                "/etc/pam.d",
                "/etc/security",
                "/etc/skel",
                "/etc/ssh/authorized_keys"
            ],

            file = [
                {copy, "/etc/default/locale"},
                {copy, "/etc/environment"},
                {copy, "/etc/hosts"},
                {copy, "/etc/security/limits.conf"},
                {copy, "/etc/security/pam_env.conf"},

                {copy, "/boot/islet_exec", Exec},

                {write, join(SandboxPath, "/boot/init"), #file_info{mode = 8#755}, Init},
                {write, join(SandboxPath, "/boot/islet"), #file_info{mode = 8#755}, Islet}
            ]
        },

        chroot = #islet_root{
            path = ChrootPath,

            directory = [
                    "/dev",
                    "/home",
                    "/home/islet",
                    "/proc",
                    "/root",
                    "/run/shm",
                    "/selinux",
                    "/sys",
                    "/tmp",
                    "/var",
                    "/var/log",
                    "/var/run",

                    {mount, "/", ChrootPath},
                    {mount, "/bin"},
                    {mount, "/sbin"},
                    {mount, "/lib"},
                    {mount, "/lib64"},
                    {mount, "/usr"},

                    {mount, "/boot", join(SandboxPath, "/boot")},
                    {mount, "/etc", join(SandboxPath, "/etc")}
                ],

            file = [
                "/var/log/lastlog",
                "/var/log/wtmp",
                "/var/run/utmp"
                ]
        }
    }.

prepare(#islet{sandbox = Sandbox, chroot = Chroot}) ->
    prepare(Sandbox),
    prepare(Chroot);
prepare(#islet_root{} = Dir) ->
    chroot(Dir).

console(Ref) ->
    Self = self(),
    Pid = spawn_link(fun() -> tty_setup(Self) end),
    console(Ref, Pid, fun(Buf) -> io:format("~s", [Buf]) end).

console(Ref, Read, Write) when is_pid(Read), is_function(Write,1) ->
    Conn = getstate(Ref, conn),
    console_xfr(Ref, Conn, Read, Write).

console_xfr(Ref, Conn, Read, Write) ->
    receive
        {verx, Conn, {#remote_message_header{
                    type = <<?REMOTE_STREAM:32>>,
                    status = <<?REMOTE_OK:32>>}, []}} ->
            ok;
        {verx, Conn, {#remote_message_header{
                    type = <<?REMOTE_STREAM:32>>,
                    status = <<?REMOTE_CONTINUE:32>>}, Buf}} ->
            Write(Buf),
            console(Ref, Read, Write);
        {islet_tty, Read, Buf} ->
            case send(Ref, Buf) of
                ok ->
                    console(Ref, Read, Write);
                Error ->
                    Error
            end
    end.

tty_setup(Pid) ->
    ok = io:setopts(standard_io, [binary]),
    tty_read(Pid).

tty_read(Pid) ->
    Buf = io:get_line(""),
    Pid ! {islet_tty, self(), Buf},
    tty_read(Pid).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
spawn(Env) ->
    ?MODULE:spawn(Env, []).
spawn(Env, Options) ->
    start_link(Env, Options).

kill(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, kill, infinity).

send(Ref, Data) ->
    Conn = getstate(Ref, conn),
    verx_client:send(Conn, Data).

recv(Ref) ->
    recv(Ref, 5000).
recv(Ref, Timeout) ->
    Conn = getstate(Ref, conn),
    verx_client:recv(Conn, Timeout).

start_link(Env, Options) ->
    Pid = self(),
    gen_server:start_link(?MODULE, [Pid, Env, Options], []).


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Pid, Env, Options]) ->
    process_flag(trap_exit, true),

    Cfg = env_to_xml(Env),

    {ok, Conn} = verx_client:start(Options),
    ok = verx:open(Conn, ["lxc:///", 0]),
    {ok, [Domain]} = verx:domain_define_xml(Conn, [verx_config:to_xml(Cfg)]),
    ok = verx:domain_create(Conn, [Domain]),
    ok = verx:domain_undefine(Conn, [Domain]),

    % Open a connection to the system console
    ok = verx:domain_open_console(Conn, [Domain, void, 0]),

    {ok, #state{
            pid = Pid,
            env = Env,
            cfg = Cfg,

            conn = #conn{
                pid = Conn,
                domain = Domain
            }
    }}.

handle_call({state, Field}, _From, State) ->
    {reply, state(Field, State), State};

handle_call(kill, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({verx, Conn, _} = Data, #state{pid = Pid, conn = #conn{pid = Conn}} = State) ->
    Pid ! Data,
    {noreply, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{conn = #conn{pid = Conn, domain = Domain}}) ->
    verx:domain_destroy(Conn, [Domain]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
default(name) ->
    N = integer_to_list(16#F0000000 +
        erlang:phash2({node(),self()}, 16#ffff)),
    "islet-" ++ N;
default(memory) ->
    integer_to_list(128 * 1024);
default(description) ->
    "islet v" ++ ?ISLET_VERSION.

macaddr(Name) ->
    <<Bytes:5/bytes, _/binary>> = erlang:md5(Name),
    MAC = ["52"] ++ [ integer_to_list(N, 16) || <<N:8>> <= Bytes ],
    string:join(MAC, ":").

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            filename:join([
                    filename:dirname(code:which(?MODULE)),
                    "..",
                    "priv"
                ]);
        Dir ->
            Dir
    end.

priv_dir(Path) ->
    join(priv_dir(), Path).

%%
%% VM chroot
%%

join(Path0, Path1) ->
    % Removes redundant path separators
    filename:join([Path0 ++ "/" ++ Path1]).

% Create a directory
mkdir(Root, Dir) ->
    filelib:ensure_dir(join(Root, Dir) ++ "/").

% Create an empty file
touch(Root, File) ->
    case file:open(join(Root, File), [write]) of
        {ok, FH} ->
            file:close(FH);
        Error ->
            Error
    end.

copy(Root, Source, File) ->
    Dest = join(Root, File),
    {ok, _} = file:copy(Source, Dest),
    {ok, Info} = file:read_file_info(Source),
    file:change_mode(Dest, Info#file_info.mode).

% Set up the chroot directory for the VM
chroot(#islet_root{path = Path, directory = Dir, file = File}) ->
    chroot(directory, Path, Dir),
    chroot(file, Path, File).

chroot(directory, Root, Dirs) ->
    lists:foreach(fun
            ({mount, Dir}) ->
                ok = mkdir(Root, Dir);
            ({mount, Dir, _}) ->
                ok = mkdir(Root, Dir);
            (Dir) when is_list(Dir) ->
                ok = mkdir(Root, Dir);
            (_) ->
                ok
        end,
        Dirs);
chroot(file, Root, Files) ->
    lists:foreach(fun
            ({copy, File}) ->
                ok = copy(Root, File, File);
            ({copy, File, Source}) ->
                ok = copy(Root, Source, File);
            ({write, File, Perms, Source}) ->
                ok = file:write_file(File, Source),
                ok = file:write_file_info(File, Perms);
            (File) when is_list(File) ->
                ok = touch(Root, File);
            (_) ->
                ok
        end,
        Files).

%%
%% libvirt XML
%%
set(Node, Cfg) when is_atom(Node); is_tuple(Node) ->
    set([Node], Cfg);
set(Nodes, Cfg) ->
    lists:foldl(fun({Key, Value}, X) -> verx_config:set(Key, Value, X) end,
        Cfg,
        Nodes).

add(Node, Cfg) when is_atom(Node); is_tuple(Node) ->
    add([Node], Cfg);
add(Nodes, Cfg) ->
    lists:foldl(fun({Key, Value}, X) -> verx_config:append(Key, Value, X) end,
        Cfg,
        Nodes).

% Convert the islet environment to XML format
env_to_xml(#islet{
        system = System,
        interface = Interface,
        chroot = #islet_root{directory = Dir}
    }) ->
    Base = [{[os,type],"exe"},
            {[os,init],"/boot/init"},
            {[devices,{console,[{type,["pty"]}]}],[]}],
    Cfg = add(Base, verx_config:init([{type, "lxc"}])),

    lists:foldl(fun(T,A) -> env_to_xml(T,A) end,
        set(System, Cfg),
        [{interface, Interface},
            {directory, Dir}]).

env_to_xml({directory, Dirs}, Cfg) ->
    N = lists:foldl(fun
            ({mount, Dir}, Acc) ->
                FS = {devices, {filesystem, [{type, ["mount"]}],
                                            [{source, [{dir, [Dir]}], []},
                                             {target, [{dir, [Dir]}], []},
                                             readonly]}},
                    [FS|Acc];
            ({mount, Target, Source}, Acc) ->
                FS = {devices, {filesystem, [{type, ["mount"]}],
                                            [{source, [{dir, [Source]}], []},
                                             {target, [{dir, [Target]}], []},
                                             readonly]}},
                    [FS|Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        Dirs),
    add(N, Cfg);
env_to_xml({interface, Ifaces}, Cfg) ->
    N = lists:foldl(fun
            ({bridge, Dev}, Acc) ->
                N = integer_to_list(erlang:phash2(self())),
                IF = {devices, {interface, [{type, ["bridge"]}],
                                           [{mac, [{address, [macaddr(N ++ Dev)]}], []},
                                            {source, [{bridge, [Dev]}], []}]}},
                [IF|Acc];
            ({bridge, Dev, MAC}, Acc) ->
                IF = {devices, {interface, [{type, ["bridge"]}],
                                           [{mac, [{address, [MAC]}], []},
                                            {source, [{bridge, [Dev]}], []}]}},

                [IF|Acc];
            ({network, Name}, Acc) ->
                IF = {devices, {interface, [{type, ["network"]}],
                                           [{source, [{network, [Name]}], []}]}},
                [IF|Acc];
            ({network, Name, MAC}, Acc) ->
                IF = {devices, {interface, [{type, ["network"]}],
                                           [{mac, [{address, [MAC]}], []},
                                            {source, [{network, [Name]}], []}]}},
                [IF|Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        Ifaces),
    add(N, Cfg).

getstate(Ref, Key) when is_atom(Key) ->
    [{Key, Value}] = gen_server:call(Ref, {state, [Key]}, infinity),
    Value.

state(Fields, State) ->
    state(Fields, State, []).
state([], _State, Acc) ->
    lists:reverse(Acc);
state([Field|Fields], State, Acc) ->
    state(Fields, State, [{Field, field(Field, State)}|Acc]).

field(conn, #state{conn = #conn{pid = Conn}}) -> Conn;
field(domain, #state{conn = #conn{domain = Domain}}) -> Domain;

field(cfg, #state{cfg = Cfg}) -> Cfg;
field(env, #state{env = Env}) -> Env;
field(pid, #state{pid = Pid}) -> Pid;

field(_, _) -> unsupported.

script(init) ->
    script(init, []);
script(islet) ->
    script(islet, []).

script(init, Options) ->
    Name = proplists:get_value(name, Options, default(name)),
    Tmp = proplists:get_value(tmp, Options, "32M"),
    Home = proplists:get_value(home, Options, "64M"),
    Setup = proplists:get_value(setup, Options, ""),
    UID = proplists:get_value(uid, Options,
        integer_to_list(16#F0000000 + erlang:phash2({node(),self()}, 16#ffff))),
    GID = proplists:get_value(gid, Options, UID),

    "#!/bin/sh
set -e
export PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/sbin
hostname -b " ++ Name ++ "
mount -t tmpfs -o noatime,mode=1777,nosuid,size=" ++ Tmp ++ " tmpfs /tmp
mount -t tmpfs -o uid=" ++ UID ++ ",gid=" ++ GID ++ ",noatime,mode=0755,nosuid,size=" ++ Home ++ " tmpfs /home/islet
" ++ Setup ++ "
exec /boot/islet_exec " ++ UID ++ " " ++ GID ++ " /boot/islet
";

script(islet, _Options) ->
    "#!/bin/sh
set -e
exec /bin/sh
".
