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

-include_lib("kernel/include/file.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([
        start/0,
        create/1, create/2,
        destroy/2,

        chroot/1,
        template/1,

        chroot_template/2,
        chroot_files/2,
        chroot_empty/2,

        macaddr/1,

        dirs/0,
        filesystems/0,
        setup/1
    ]).

-define(ISLET_ROOT, filename:absname("priv/archipelago")).


start() ->
    {ok, Ref} = verx_client:start(),
    ok = verx:open(Ref, ["lxc:///", 0]),
    {ok, Ref}.

create(Ref) ->
    create(Ref, []).
create(Ref, Options0) ->
    Name = proplists:get_value(name, Options0, islet_template:name()),
    Path = proplists:get_value(path, Options0, ?ISLET_ROOT),

    Options = [
        {name, Name},
        {path, Path}
    ] ++ Options0,

    ok = chroot(Options),

    XML = template(Options),

    {ok, [Domain]} = verx:domain_define_xml(Ref, [XML]),
    ok = file:write_file(Path ++ "/" ++ Name ++ "/islet.xml", XML),

    verx:domain_create(Ref, [Domain]).

destroy(Ref, Name) ->
    case verx:lookup(Ref, {domain, Name}) of
        {ok, [Domain]} -> destroy_1(Ref, Domain);
        {error, Error} -> {error, Error}
    end.

destroy_1(Ref, Domain) ->
    case verx:domain_destroy(Ref, [Domain]) of
        ok -> destroy_2(Ref, Domain);
        {error, Error} -> {error, Error}
    end.

destroy_2(Ref, Domain) ->
    % Retrieve the rootfs path
    {ok, [XML]} = verx:domain_get_xml_desc(Ref, [Domain, 0]),
    {Xmerl, _} = xmerl_scan:string(binary_to_list(XML)),

    Res = xmerl_xpath:string("string(/domain/devices/filesystem/target[@dir='/']/../source/@dir)", Xmerl),

    % Ensure the path is valid
    "sftoor/" ++ Toor = lists:reverse(Res#xmlObj.value),

    Root = lists:reverse(Toor),

    % XXX file:del_dir/1
    os:cmd("/bin/rm -rf " ++ Root),

    verx:domain_undefine(Ref, [Domain]).

template(Options) ->
    Name = proplists:get_value(name, Options),
    Path = proplists:get_value(path, Options),
    Memory = integer_to_list(proplists:get_value(memory, Options, islet_template:memory())),

    Iface = [ {Dev, islet:macaddr(Name ++ "-" ++ Dev)} || Dev <- interfaces() ],

    Mount = [ {Path ++ "/" ++ Name ++ "/rootfs", "/"} ] ++ filesystems(),

    Interfaces = proplists:get_value(interfaces, Options, Iface),
    Filesystems = proplists:get_value(filesystems, Options, Mount),

    Ctx = [
        {name, Name},
        {path, Path},
        {memory, Memory},
        {interfaces, [ dict:from_list([{source, Source}, {macaddr, Address}])
                || {Source, Address} <- Interfaces ]},
        {filesystems, [ dict:from_list([{source, Source}, {target, Target}])
                || {Source, Target} <- Filesystems ]}
    ],

    Template = mustache:compile(islet_template),
    mustache:render(islet_template, Template, dict:from_list(Ctx)).

chroot(Options) ->
    Name = proplists:get_value(name, Options),
    Path0 = proplists:get_value(path, Options),

    MinUid = proplists:get_value(min_uid, Options, 16#8000),
    MaxUid = proplists:get_value(max_uid, Options, 16#ff00),
    Uid = proplists:get_value(uid, Options, uid(MinUid, MaxUid)),

    Setup = proplists:get_value(setup, Options, setup(Name)),
    Init = proplists:get_value(init, Options, init(Uid, Setup)),
    Islet = proplists:get_value(islet, Options, islet()),

    Path = Path0 ++ "/" ++ Name,

    Root = Path ++ "/rootfs",
    ok = filelib:ensure_dir(Root ++ "/."),
    ok = file:change_mode(Path, 8#0700),

    islet:chroot_template(Root, dirs()),
    islet:chroot_files(Root, files()),
    islet:chroot_empty(Root, empty()),

    ok = file:write_file(Path ++ "/rootfs/etc/passwd", passwd(Uid)),
    file:change_mode(Path ++ "/rootfs/etc/passwd", 8#444),

    ok = file:write_file(Path ++ "/rootfs/init", Init),
    file:change_mode(Path ++ "/rootfs/init", 8#755),

    ok = file:write_file(Path ++ "/rootfs/islet", Islet),
    file:change_mode(Path ++ "/rootfs/islet", 8#755).

%%
%% Utilities to prepare the chroot
%%

% Create a chroot hierarchy
chroot_template(Chroot0, Template) ->
    Chroot = maybe_binary(Chroot0),
    true = filelib:is_dir(Chroot),

    [ ok = filelib:ensure_dir(<<Chroot/binary, "/", (maybe_binary(Path))/binary, "/.">>)
        || Path <- Template ],

    ok.

% Copy files into a chroot
chroot_files(Chroot0, Files) ->
    Chroot = maybe_binary(Chroot0),
    true = filelib:is_dir(Chroot),

    [ begin
        Copy = <<Chroot/binary, "/", (maybe_binary(File))/binary>>,
        {ok, _} =  file:copy(File, Copy),
        {ok, FI} = file:read_file_info(File),
        ok = file:change_mode(Copy, FI#file_info.mode)
      end
        || File <- Files ],

    ok.

% Touch empty files in chroot
chroot_empty(Chroot0, Files) ->
    Chroot = maybe_binary(Chroot0),
    true = filelib:is_dir(Chroot),

    [ begin
        {ok, FH} = file:open(<<Chroot/binary, "/", (maybe_binary(File))/binary>>, [write]), 
        ok = file:close(FH)
      end || File <- Files ],

    ok.

maybe_binary(N) when is_list(N) -> list_to_binary(N);
maybe_binary(N) when is_binary(N) -> N.

macaddr([]) ->
    macaddr(crypt:rand_bytes(5));
macaddr(Name) ->
    <<Bytes:5/bytes, _/binary>> = erlang:md5(Name),
    "52:" ++ string:join([ httpd_util:integer_to_hexlist(N)
    || <<N:8>> <= Bytes ], ":").


%%
%% Defaults
%%
dirs() ->
    [
        "/bin",
        "/dev",
        "/etc/default",
        "/etc/pam.d",
        "/etc/security",
        "/etc/skel",
        "/etc/ssh/authorized_keys",
        "/home",
        "/home/islet",
        "/lib",
        "/lib64",
        "/proc",
        "/root",
        "/run/shm",
        "/sbin",
        "/selinux",
        "/sys",
        "/tmp",
        "/usr",
        "/var",
        "/var/log",
        "/var/run"
    ].

files() ->
    [
        "/etc/default/locale",
        "/etc/environment",
        "/etc/group",
        "/etc/hosts",
        "/etc/security/limits.conf",
        "/etc/security/pam_env.conf"
    ].

empty() ->
    [
        "/var/log/lastlog",
        "/var/log/wtmp",
        "/var/run/utmp"
    ].

interfaces() ->
    [
        "br0"
    ].

filesystems() ->
    [
        {"/bin", "/bin"},
        {"/sbin", "/sbin"},
        {"/lib", "/lib"},
        {"/lib64", "/lib64"},
        {"/usr", "/usr"}
    ].

uid(Min, Max) ->
    crypto:rand_uniform(Min, Max).

passwd(Uid0) ->
    Uid = integer_to_list(Uid0),
"root:x:0:0:root:/root:/usr/sbin/nologin
islet:x:" ++ Uid ++ ":" ++ Uid ++ "::/home/islet:/usr/sbin/nologin
".

setup(Name) ->
    "
export PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
hostname -b " ++ Name ++ "
chmod 755 /home/islet
chown islet /home/islet
".

init(Uid0, Setup) ->
    Uid = integer_to_list(Uid0),
"#!/bin/sh
set -e
" ++ Setup ++ "
exec /sbin/start-stop-daemon --start --verbose --exec /islet --chuid " ++ Uid.

islet() ->
    "#!/bin/sh
set -e
while read l; do
    echo $l
done
".
