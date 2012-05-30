
{logdir, "{{ tmpdir }}/systest/suites/cli/logs"}.
{config, "{{ base_dir }}/resources/systest_cli.config"}.

{alias, test, "{{ base_dir }}/test"}.
% {suites, test, systest_cli_SUITE}.
{cases, test, systest_cli_SUITE,
    [local_and_global_scope_configuration_handling,
     starting_and_stopping_nodes,
     killing_nodes]}.

{ct_hooks, [cth_log_redirect]}.
{enable_builtin_hooks, true}.

