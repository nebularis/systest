
{resource,  "resources/default/*"}.
{targets,   [systest_cli_SUITE,
             systest_proc_SUITE,
             systest_supervision_SUITE]}.
{hooks,     [cth_log_redirect,
             {systest_supervision_cth, [], 0},
             {systest_cth, [], 1000}]}.
