{application, fresh, [
    {description, "The freshest Erlang web framework"},
    {vsn, "0.1"},
    {applications, [kernel, stdlib, sasl]},
    {modules, [fresh, fresh_server, fresh_handler, fresh_registry,
               fresh_util]},
    {registered, [fresh_registry]},
    {mod, {fresh, []}},
    {env, []}
]}.
