
{logdir, "{{ tmpdir }}/systest/suites/cli/logs"}.
{config, "{{ base_dir }}/resources/systest_cli.config"}.

{alias, test, "{{ base_dir }}/test"}.
{suites, test, systest_cli_SUITE}.

{ct_hooks, [cth_log_redirect]}.
{enable_builtin_hooks, true}.
