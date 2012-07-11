
{resource, "resources/error_handling.resource"}.
{targets,  [systest_error_handling_SUITE]}.
{hooks,    [cth_log_redirect, {systest_error_handling_cth, [], 1000}]}.
