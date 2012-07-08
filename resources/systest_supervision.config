
{basic_node, [
    {startup, [
        {handler, systest_slave}
    ]},
    {flags, [
        {start, "-boot start_sasl +W w "
                "+K true +A30 +P 1048576 "
                "-pa ebin "
                "-sasl sasl_error_logger false"}
    ]}
]}.

{bad_cli, [
    {startup, [
        {handler, systest_cli},
        {detached, false}
    ]},
    {flags, [
        {start, [
            {program, "this-script-does-not-exists"}
        ]},
        {stop, [
            {program, "neither-does-this"},
            {args, ["stop", "-n", "${proc.id}"]}
        ]}
    ]}
]}.

%% TODO: BAD HOOKS!!!!

{systest_supervision, [
    {sut, [
        {localhost, [red, blue]}
    ]},
    {processes, [
        {red,  [basic_node]},
        {blue, [basic_node]}
    ]}
]}.

{should_fail, [
    {sut, [{localhost, [nostart]}]},
    {processes, [
        {nostart, [basic_node]}
    ]}
]}.

{init_per_tc_manages_shutdown, [
    {sut, [{localhost, [a, b]}]},
    {processes, [
        {a, [basic_node]},
        {b, [basic_node]}
    ]}
]}.

{end_per_tc_automation, [
    {sut, [{localhost, [x,y,z]}]},
    {processes, [
        {x, [basic_node]},
        {y, [basic_node]},
        {z, [basic_node]}
    ]}
]}.

