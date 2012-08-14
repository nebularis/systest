-type trace_loc()   :: [atom()] | 'all'.    %% nodes
%-type call_filter() :: module() |
%                       {module(), atom()} |
%                       {module(), atom(), integer()} |
%                       {module(), atom(), integer(), term()}.
%-type type()        :: 'gc' | 'sched' | 'send' | 'recv' | 'msg' | 'call'.
-type ptarget()     :: pid() | atom() | {'global', atom()} | 'all'.
%-type trace_spec()  :: call_filter()              |
%                       type()                     | %% means p(all, [..])
%                       {ptarget(), call_filter()} | %% implies 'c'
%                       {ptarget(), type(), call_filter()}.

%% {ok,_}=ttb:tp(M,F,A,[{'_',[],[{message,{caller}},{exception_trace}]}]).

-type trace_pattern_scope() :: 'global' | 'local'.

-record(trace_pattern, {
    scope       :: trace_pattern_scope(),
    module      :: module(),
    function    :: atom(),
    arity       :: integer() | '_',
    match_spec  :: term()  %% is there a type spec for this that we can borrow?
}).

-type trace_pattern() :: #trace_pattern{}.

-record(trace, {
    name            :: atom(),
    scope           :: atom(),  %% scope at which this trace is activated
    location        :: trace_loc(),
    process_filter  :: ptarget(),
    trace_flags     :: atom(),
    trace_pattern   :: trace_pattern()
}).

-type trace() :: #trace{}.

-record(trace_config, {
    trace_config    :: file:filename(),
    trace_db_dir    :: file:filename(),
    trace_data_dir  :: file:filename(),
    base_config     :: systest_config:config(),
    active          :: [trace()],
    flush           :: boolean(),
    console         :: boolean()
}).

-type trace_config() :: #trace_config{}.
