
%% typically this would be in something like (global|procs).config
{global_proc_config, [
    {startup, [
        {handler, systest_cli},
        {link_to_parent, true},
        {detached, false},
        {rpc_enabled, {true, {init, stop, []}}},
        {cover, true}
    ]},
    {flags, [
        {start, [
            {program, "${settings.base_dir}/resources/test/start"},
            {args, ["${proc.id}"]},
            {environment, [
                {"LOGDIR", "%{TMPDIR}/logs/${proc.id}.log"},
                {"DUMPDIR", "${ct.priv_dir}/dump/${proc.id}.log"},
                {"PORT", "${proc.user.port}"}
            ]}
        ]},
        {stop,  [{program, "${settings.base_dir}/resources/test/stop"},
                 {args, ["${proc.id}"]}]}
    ]}
]}.

{example_proc_config, [
    {startup, [
        {handler, systest_slave},
        {link_to_parent, true},
        {detached, true},
        {rpc_enabled, default}
    ]},
    {flags, [
        {start, [{program, "${settings.base_dir}/resources/test/start"},
                 {args, ["${proc.id}"]}]},
        {stop,  [{program, "{{ base_dir }}/resources/test/stop"},
                 {args, ["${proc.id}"]}]}
    ]}
]}.

{bad_rpc_detached_mismatch, [
    {startup, [{detached, true}, {rpc_enabled, false}]}
]}.

{bad_rpc_exit_eof_mismatch, [
    {startup, [{rpc_enabled, false}, {exit_on_eof, false}]}
]}.

{bad_rpc_combo, [
    {sut, [{localhost, [n1]}]},
    {processes, [
        {n1, [global_proc_config, bad_rpc_detached_mismatch]}
    ]}
]}.

{bad_eof_combo, [
    {sut, [{localhost, [n1]}]},
    {processes, [
        {n1, [global_proc_config, bad_rpc_exit_eof_mismatch]}
    ]}
]}.

{fork_proc_config, [
    {startup, [
        {handler, systest_cli},
        {link_to_parent, true},
        {detached, false},
        {rpc_enabled, {true, {init, stop, []}}}
    ]},
    {flags, [
        {start, [
            {program, "${settings.base_dir}/resources/test/start-fork"},
            {args, ["${proc.id}"]}
        ]},
        {stop,  [{program, "${settings.base_dir}/resources/test/stop"},
                 {args, ["${proc.id}"]}]}
    ]}
]}.

{exit_on_eof, [
    {startup, [{exit_on_eof, auto}]}
]}.

{forking_processes, [
    {sut, [{localhost, [n1, n2]}]},
    {processes, [
        {n1, [fork_proc_config, exit_on_eof]},
        {n2, [fork_proc_config, exit_on_eof]},
        {n3, [fork_proc_config, exit_on_eof]}
    ]}
]}.

{run_proc_config, [
    {startup, [
        {handler, systest_cli},
        {detached, false},
        {rpc_enabled, false},
        {exit_on_eof, true}
    ]},
    {flags, [
        {start, [{program, "${settings.base_dir}/resources/test/pipe-read"},
                 {args, ["${proc.name}"]},
                 {environment, [
                     {"PIPE_DIR", "${ct.priv_dir}/${ct.scope}/.PIPEDIR"}
                 ]}
        ]},
        {stop, [{program, "${settings.base_dir}/resources/test/pipe-write"},
                {args, ["${proc.name}", "${user.stop_argv}"]},
                {environment, [
                     {"PIPE_DIR", "${ct.priv_dir}/${ct.scope}/.PIPEDIR"}
                ]}
        ]}
    ]}
]}.

{rpc_proc_config, [
    {startup, [
        {handler, systest_cli},
        {detached, false},
        {rpc_enabled, {true, {erlang, halt, []}}}
    ]},
    {flags, [
        {start, [
            {program, "${settings.base_dir}/resources/test/start"},
            {args, ["${proc.id}"]}
        ]}
    ]}
]}.

{rpc_stop_enabled, [
    {sut, [{localhost, [n1]}]},
    {processes, [
        {n1, [rpc_proc_config]}
    ]}
]}.

{start_explicit, [
    {sut, []},
    {processes, [
        {n1, [run_proc_config]},
        {n2, [run_proc_config]}
    ]},
    {user_data, [
        {n1, [{stop_argv, "quit"}]},
        {n2, [{stop_argv, "bang"}]}
    ]}
]}.

{systest_cli_config_example, [
    {sut, [{localhost, [red, green]}]},
    {processes, [
        {red,   [example_proc_config]},
        {green, [example_proc_config]}
    ]},
    {user_data, [
        {red, [{port, 5432}]},
        {green, [{port, 5431}]}
    ]}
]}.

{cli_2node_cluster, [
    {sut, [{localhost, [red, green]}]},
    {processes, [
        {red,   [global_proc_config]},
        {green, [global_proc_config]}
    ]},
    {user_data, [
        {red, [{port, 5432}]},
        {green, [{port, 5431}]}
    ]}
]}.

{detached_proc, [
    {startup, [
        {handler, systest_cli},
        {link_to_parent, false},
        {detached, true},
        {rpc_enabled, {true, {init, stop, []}}}
    ]},
    {flags, [
        {start, [{program, "${settings.base_dir}/resources/test/start-daemon"},
                 {args, ["${proc.id}"]}]},
        {stop,  [{program, "${settings.base_dir}/resources/test/stop"},
                 {args, ["${proc.id}"]}]}
    ]}
]}.

{handling_detached_processes, [
    {sut, [{localhost, [yellow, blue]}]},
    {processes, [
        {yellow, [detached_proc]},
        {blue, [detached_proc]}
    ]}
]}.

{systest_cli_SUITE, [{all, cli_2node_cluster},
                     {detached_config_handling_failure, no_config},
                     {rpc_exit_config_handling_failure, no_config},
                     {handling_attached_processes_with_exec,
                      forking_processes},
                     {generate_exit_on_eof_wrapper,
                      forking_processes},
                     {handling_normal_exit_status,
                      start_explicit},
                     {handling_non_zero_exit_status,
                      start_explicit}]}.

{start_cold, [
    {startup, [
        {activate_on_start, false}
    ]}
]}.

{manually_starting_processes, [
    {sut, [{localhost, [n1, n2]}]},
    {processes, [
        {n1, [global_proc_config]},
        {n2, [global_proc_config, start_cold]}
    ]}
]}.

{systest_proc_SUITE, [{restarting_manually_started_processes,
                        manually_starting_processes}]}.
