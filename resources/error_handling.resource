
{clean_node, [
    {startup, [
        {handler, systest_slave}
    ]},
    {flags, [
        {start, "-boot start_sasl -pa ebin "
                "-sasl sasl_error_logger false"}
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
