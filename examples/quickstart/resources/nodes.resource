{sasl_node, [
    {startup, [{handler, systest_slave}]},
    {flags, [{start, "-boot start_sasl +W w +K true +A30 +P 1048576"
                     " -pa ${settings.base_dir}/ebin "
                     "-sasl sasl_error_logger false"}]},
    {apps, [{sasl, [{errlog_type, error}]}]}
]}.
