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
-module(sshvm).

-export([
        start/0, start/1,
        session/1
    ]).
-export([shell/1]).

start() ->
    start([]).

start(Options) ->
    Port = proplists:get_value(port, Options, 2323),
    Password = proplists:get_value(password, Options, "islet"),
    System = proplists:get_value(system_dir, Options, "priv/ssh/system"),
    User = proplists:get_value(user_dir, Options, "priv/ssh/user"),

    key(System ++ "/ssh_host_rsa_key"),
%    key(User ++ "/id_rsa"),

    ssh:start(),
    Res = ssh:daemon(Port, [
            {password, Password},
            {system_dir, System},
            {user_dir, User},
            {shell, {?MODULE, shell, [Options]}}
        ]),

    io:format("Login using: ssh localhost -p ~B -o UserKnownHostsFile=~s/known_hosts~n",
        [Port, filename:absname(User)]),

    Res.

key(Key) ->
    ok = filelib:ensure_dir(Key),
    case file:read_file_info(Key) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            os:cmd("ssh-keygen -q -t rsa -N \"\" -f " ++ Key)
    end.

shell(Options) ->
    spawn(fun() -> session(Options) end).

session(Options) ->
    Env = islet:env(Options),
    ok = islet:prepare(Env),
    {ok, Ref} = islet:spawn(Env),
    islet:console(Ref).
