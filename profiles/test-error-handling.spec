
{config, "{{ base_dir }}/resources/error_handling.config"}.

{alias, test, "{{ base_dir }}/ebin"}.
{suites, test, [systest_error_handling_SUITE]}.

{event_handler, systest_event}.
{ct_hooks, [cth_log_redirect,
            {systest_error_handling_cth, [], 1000}]}.
{enable_builtin_hooks, true}.
