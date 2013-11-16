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
