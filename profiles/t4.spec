
{config, "{{ base_dir }}/resources/systest_cli.config"}.
{config, "{{ base_dir }}/resources/systest_nodes.config"}.
{config, "{{ base_dir }}/resources/systest_supervision.config"}.
{config, "{{ base_dir }}/resources/trace.config"}.

{alias, test, "{{ base_dir }}/ebin"}.
{suites, test, all}.
% {cases, test, systest_cli_SUITE, starting_and_stopping_nodes}.

{ct_hooks, [cth_log_redirect,
            {systest_supervision_cth, [], 0},
            {systest_cth, [], 1000}]}.
{enable_builtin_hooks, true}.

