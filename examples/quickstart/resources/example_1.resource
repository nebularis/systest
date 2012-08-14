{system_1, [
    {sut, [{localhost, [node1, node2]}]},
    {processes, [{node1, [sasl_node]}, {node2, [sasl_node]}]}
]}.

{example_1_SUITE, [{all, system_1}]}.
