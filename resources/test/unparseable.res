%% contains parse errors, which we use to exercise test cases.
{foobar, [
    {baz, a},
    {boz, b},   %% trailing commas aren't legal syntax...
]}.

