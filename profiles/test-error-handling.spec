
{config, "{{ base_dir }}/resources/systest_cli.config"}.
{config, "{{ base_dir }}/resources/systest_procs.config"}.
{config, "{{ base_dir }}/resources/systest_supervision.config"}.

{alias, test, "{{ base_dir }}/ebin"}.
{suites, test, [systest_error_handling_SUITE]}.

{event_handler, systest_event}.
{ct_hooks, [cth_log_redirect,
            {systest_verify_cth, [], 1000}]}.
{enable_builtin_hooks, true}.
