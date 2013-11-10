%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(islet).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-export([
        env/0, env/1,
        prepare/1,

        spawn/1, spawn/2,
        kill/1,

        send/2,
        recv/1, recv/2
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

-define(ISLET_VERSION, "0.2.0").


env() ->
    % XXX relies on 'directory' preceeding 'file'
    [
        {name, undefined},

        {memory, default(memory)},
        {description, default(description)},

        {directory, [
                "/dev",
                "/etc/default",
                "/etc/pam.d",
                "/etc/security",
                "/etc/skel",
                "/etc/ssh/authorized_keys",
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

                {mount, "/", default(root)},
                {mount, "/bin"},
                {mount, "/sbin"},
                {mount, "/lib"},
                {mount, "/lib64"},
                {mount, "/usr"}
            ]},

        {file, [
                "/var/log/lastlog",
                "/var/log/wtmp",
                "/var/run/utmp",

                {copy, "/etc/default/locale"},
                {copy, "/etc/environment"},
                {copy, "/etc/group"},
                {copy, "/etc/hosts"},
                {copy, "/etc/security/limits.conf"},
                {copy, "/etc/security/pam_env.conf"},

                {copy, "/init", priv_dir("init")}
            ]}
    ].

env(Env) when is_list(Env) ->
    lists:ukeysort(1, Env ++ env()).

prepare(Env) when is_list(Env) ->
    chroot(Env).

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

handle_call(kill, _From, #state{conn = #conn{pid = Conn, domain = Domain}} = State) ->
    verx:domain_destroy(Conn, [Domain]),
    verx:domain_undefine(Conn, [Domain]),
    verx_client:stop(Conn),
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
    verx:domain_undefine(Conn, [Domain]),
    verx_client:stop(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
default(root) ->
    priv_dir("/islet/rootfs");
default(name) ->
    N = erlang:phash2(self()),
    "islet-" ++ integer_to_list(N);
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

root(Env) ->
    Dir = proplists:get_value(directory, Env),
    case lists:keyfind("/", 2, Dir) of
        {mount, "/", Root} ->
            Root;
        {mount, "/"} ->
            "/"
    end.

% Set up the chroot directory for the VM
chroot(Env) ->
    Root = root(Env),
    chroot(Root, Env).

chroot(_Root, []) ->
    ok;
chroot(Root, [{directory, Dirs}|Tail]) ->
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
        Dirs),
    chroot(Root, Tail);
chroot(Root, [{file, Files}|Tail]) ->
    lists:foreach(fun
            ({copy, File}) ->
                ok = copy(Root, File, File);
            ({copy, File, Source}) ->
                ok = copy(Root, Source, File);
            (File) when is_list(File) ->
                ok = touch(Root, File);
            (_) ->
                ok
        end,
        Files),
    chroot(Root, Tail);
chroot(Root, [_|Tail]) ->
    chroot(Root, Tail).

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
env_to_xml(Env) when is_list(Env) ->
    % XXX Allow setting the name to 'undefined' in the template
    Name = case proplists:get_value(name, Env, undefined) of
        undefined ->
            default(name);
        N ->
            error_logger:info_report([{name, N}]),
            N
    end,
    Base = [{[os,type],"exe"},
            {[os,init],"/init"},
            {[devices,{console,[{type,["pty"]}]}],[]}],
    Cfg0 = set({name, Name}, verx_config:init([{type, "lxc"}])),
    Cfg = add(Base, Cfg0),
    env_to_xml(Name, proplists:delete(name, Env), Cfg).

env_to_xml(_, [], Cfg) ->
    Cfg;
env_to_xml(Name, [{directory, Dirs}|Tail], Cfg) ->
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
    env_to_xml(Name, Tail, add(N, Cfg));
env_to_xml(Name, [{interfaces, Ifaces}|Tail], Cfg) ->
    N = lists:foldl(fun
            ({dev, Dev}, Acc) ->
                IF = {devices, {interface, [{type, ["bridge"]}],
                                           [{mac, [{address, [macaddr(Name ++ Dev)]}], []},
                                            {source, [{bridge, [Dev]}], []}]}},
                [IF|Acc];
            ({dev, Dev, MAC}, Acc) ->
                IF = {devices, {interface, [{type, ["bridge"]}],
                                           [{mac, [{address, [MAC]}], []},
                                            {source, [{bridge, [Dev]}], []}]}},
                [IF|Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        Ifaces),
    env_to_xml(Name, Tail, add(N, Cfg));
env_to_xml(Name, [{Skip, _}|Tail], Cfg) when Skip =:= file ->
    env_to_xml(Name, Tail, Cfg);
env_to_xml(Name, [{_, _} = Spec|Tail], Cfg) ->
    env_to_xml(Name, Tail, set(Spec, Cfg));
env_to_xml(Name, [_|Tail], Cfg) ->
    env_to_xml(Name, Tail, Cfg).


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
