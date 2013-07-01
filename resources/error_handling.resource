
{clean_node, [
    {startup, [
        {handler, systest_slave},
        {link_to_parent, true}
    ]},
    {flags, [
        {start, "-boot start_sasl -pa ebin "
                "-sasl sasl_error_logger false"}
    ]}
]}.

{bad_on_join, [
    {on_join, [
        {local, no_such_module, no_such_function, []}
    ]}
]}.

{bad_on_start, [
    {on_start, [
        {local, erlang, error, []}
    ]}
]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{systest_error_handling_SUITE, [{timetrap_failure, cli_2node_cluster}]}.

{sut_start_scripts_badly_configured, [
    {sut, [{localhost, [badnode]}]},
    {processes, [
        {badnode, [bad_cli]}
    ]}
]}.

{failing_proc_on_start_hook, [
    {sut, [{localhost, [n1]}]},
    {processes, [
        {n1, [clean_node, bad_on_start]}
    ]}
]}.

{failing_sut_on_start_hook, [
    {sut, [{localhost, [n1, n2]}]},
    {processes, [
        {n1, [clean_node]},
        {n2, [clean_node]}
    ]},
    {on_start, [
        {local, erlang, error, []}
    ]}
]}.

{failing_proc_on_joined_hook, [
    {sut, [{localhost, [n1]}]},
    {processes, [
         {n1, [clean_node, bad_on_join]}
    ]}
]}.

%%%%%%%%%%%%%%%%%%%%%%%%%

{no_start, [
    {on_start, [{timer, sleep, [16#ffffffff]}]}
]}.

{no_stop, [
    {on_stop, [{timer, sleep, [16#ffffffff]}]}
]}.

{simple_test_case_pass_through, [
    {sut, [{localhost, [n1, n2]}]},
    {processes, [
         {n1, [clean_node, no_stop]},
         {n2, [clean_node, no_stop]}
    ]}
]}.

{test_teardown_timeouts, [
    {sut, [{localhost, [n3]}]},
    {processes, [{n3, [clean_node, no_stop]}]}
]}.

{hang_on_startup, [
    {sut, [{localhost, [n1, n2]}]},
    {processes, [
         {n1, [clean_node, no_start]},
         {n2, [clean_node, no_start]}
    ]}
]}.
