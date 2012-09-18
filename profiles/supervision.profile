
{resource,  "resources/default/*"}.
{targets,   [systest_supervision_SUITE]}.
{hooks,     [cth_log_redirect,
             {systest_supervision_cth, [], 0},
             {systest_cth, [], 1000}]}.
