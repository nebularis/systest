
%% typically this would be in something like (global|procs).config
{global_proc_config, [
    {startup, [
        {handler, systest_cli},
        {link_to_parent, true},
        {detached, false},
        {rpc_enabled, {true, {init, stop, []}}}
    ]},
    {flags, [
        {start, [
            {program, "{{ base_dir }}/resources/test/start"},
            {args, ["${proc.id}"]},
            {environment, [
                {"LOGDIR", "%{TMPDIR}/logs/${proc.id}.log"},
                {"DUMPDIR", "${ct.priv_dir}/dump/${proc.id}.log"},
                {"PORT", "${proc.user.port}"}
            ]}
        ]},
        {stop,  [{program, "{{ base_dir }}/resources/test/stop"},
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
        {start, [{program, "{{ base_dir }}/resources/test/start"},
                 {args, ["${proc.id}"]}]},
        {stop,  [{program, "{{ base_dir }}/resources/test/stop"},
                 {args, ["${proc.id}"]}]}
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
    {startup, [{link_to_parent, false},
               {detached,       true}]},
    {flags, [
        {start, [{program, "{{ base_dir }}/resources/test/start-daemon"},
                 {args, ["${proc.id}"]}]}
    ]}
]}.

{handling_detached_processes, [
    {sut, [{localhost, [yellow, blue]}]},
    {processes, [
        {yellow, [global_proc_config, detached_proc]},
        {blue, [global_proc_config, detached_proc]}
    ]}
]}.

{systest_cli_SUITE, [{all, cli_2node_cluster}]}.

