
{logdir, "{{ tmpdir }}/systest/suites/cli/logs"}.
{config, "{{ base_dir }}/resources/systest_cli.config"}.

{alias, test, "{{ base_dir }}/test"}.
{suites, test, systest_cli_SUITE}.
