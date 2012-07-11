
{sasl_node, [
    {startup, [{handler, systest_slave},
               {link_to_parent, true}]},
    {flags, [{start, "-boot start_sasl +W w +K true +A30 +P 1048576"
                     " -pa ${settings.base_dir}/ebin "
                     "-systest scratch_dir '{file,\"%{SCRATCH_DIR}\"}' "
                     "-sasl sasl_error_logger false"}]},
    {on_start, [{systest, write_pid_file, []}]},
    {apps, [{sasl, [{errlog_type, error}]}]}
]}.

{restarting_procs, [
    {sut, [{localhost, [node1, node2]}]},
    {processes, [{node1, [sasl_node]}, {node2, [sasl_node]}]}
]}.

