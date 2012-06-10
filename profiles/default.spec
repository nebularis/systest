
{logdir, "{{ tmpdir }}/systest/suites/cli/logs"}.
{config, "{{ base_dir }}/resources/systest_cli.config"}.
% {config, "{{ base_dir }}/resources/systest_nodes.config"}.
{config, "{{ base_dir }}/resources/systest_supervision.config"}.

{alias, test, "{{ base_dir }}/test-ebin"}.
{suites, test, all}.
% {cases, test, systest_supervision_SUITE, all}.

{ct_hooks, [cth_log_redirect,
            {systest_supervision_cth, [], 0},
            {systest_cth, [], 1000}]}.
{enable_builtin_hooks, true}.
