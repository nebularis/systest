
{logdir, "{{ tmpdir }}/systest/suites/cli/logs"}.
{config, "{{ base_dir }}/resources/systest_cli.config"}.
{config, "{{ base_dir }}/resources/systest_supervision.config"}.

{alias, test, "{{ base_dir }}/test"}.
% {suites, test, systest_cli_SUITE}.
{cases, test, systest_supervision_SUITE, all}.

{ct_hooks, [cth_log_redirect, systest_cth]}.
{enable_builtin_hooks, true}.

