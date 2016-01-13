-module(exometer_report_librato).

-define(ENDPOINT, <<"https://metrics-api.librato.com/v1/metrics">>).

-compile(export_all).

%--- Callbacks ----------------------------------------------------------------
-export([exometer_init/1]).
-export([exometer_setopts/4]).
-export([exometer_subscribe/5]).
-export([exometer_report/5]).
-export([exometer_newentry/2]).
-export([exometer_info/2]).
-export([exometer_cast/2]).
-export([exometer_call/3]).
-export([exometer_unsubscribe/4]).
-export([exometer_terminate/2]).

%--- Callbacks ----------------------------------------------------------------

exometer_init(Options) ->
    {ok, #{
        endpoint => get(endpoint, Options, ?ENDPOINT),
        user     => get(user, Options),
        token    => get(token, Options),
        source   => source(get(source, Options, nodename)),
        prefix   => get(prefix, Options, []),
        interval => get(interval, Options, 10000),
        type_map => validate_type_map(get(type_map, Options, [])),
        last     => erlang:monotonic_time(milli_seconds),
        metrics  => #{
            counters => [],
            gauges   => []
        }
    }}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_subscribe(_Metric, _DataPoint, _Interval, _Extra, State) ->
    {ok, State}.

exometer_report(Metric, DataPoint, Extra, Value, State) ->
    % TODO: Clean up
    NewState = add_metric(Metric, DataPoint, Value, Extra, State),
    case check_timeout(NewState) of
        {timeout, NewNewState} ->
            {ok, send_metrics(NewNewState)};
        {wait, NewState} ->
            {ok, NewState}
    end.

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_info(Unknown, State) ->
    error_logger:info_report([
        {module, ?MODULE},
        {unknown_info, Unknown}
    ]),
    {ok, State}.

exometer_cast(Unknown, State) ->
    error_logger:info_report([
        {module, ?MODULE},
        {unknown_cast, Unknown}
    ]),
    {ok, State}.

exometer_call(state, _From, State) ->
    {reply, State, State};
exometer_call(Unknown, From, State) ->
    error_logger:info_report([
        {module, ?MODULE},
        {unknown_call, Unknown},
        {from, From}
    ]),
    {ok, State}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

exometer_terminate(_Reason, _State) ->
    ok.

%--- Internal -----------------------------------------------------------------

get(Option, Options) ->
    get(Option, Options, fun() -> error({missing_option, Option}) end).

get(Option, Options, Default) when is_function(Default) ->
    case proplists:get_value(Option, Options) of
        undefined -> Default();
        Value     -> Value
    end;
get(Option, Options, Default) ->
    get(Option, Options, fun() -> Default end).

source(nodename) ->
    list_to_binary(string:sub_word(atom_to_list(node()), 1, $@));
source(hostname) ->
    list_to_binary(string:sub_word(atom_to_list(node()), 2, $@));
source(Fun) when is_function(Fun) ->
    Fun();
source(Name) when is_list(Name) ->
    Name.

validate_type_map(TypeMap) -> [validate_type(T) || T <- TypeMap].

validate_type({_, counter} = Mapping) -> Mapping;
validate_type({_, gauge} = Mapping)   -> Mapping;
validate_type({_, spiral} = Mapping)  -> Mapping;
validate_type({_, Type})              -> error({unsupported_type, Type}).

add_metric(Metric, DataPoint, Value, Extra, State) ->
    #{metrics := Metrics, type_map := TypeMap} = State,
    Time = erlang:system_time(seconds),
    Type = case exometer_util:report_type(Metric, Extra, TypeMap) of
        {ok, counter} -> counters;
        {ok, spiral}  -> counters;
        {ok, gauge}   -> gauges;
        error         -> gauges
    end,
    State#{metrics := Metrics#{
        Type := [{Metric, DataPoint, Value, Time}|maps:get(Type, Metrics)]
    }}.

check_timeout(#{interval := Interval, last := Last} = State) ->
    Current = erlang:monotonic_time(milli_seconds),
    if
        Current - Last >= Interval -> {timeout, State#{last := Current}};
        true                       -> {wait, State}
    end.

send_metrics(#{metrics := Metrics} = State) ->
    try
        #{counters := Counters, gauges := Gauges} = Metrics,
        Payload = jiffy:encode(#{
            counters => [metric(State, C) || C <- Counters],
            gauges => [metric(State, G) || G <- Gauges]
        }),

        case post(uri(State), [auth(State)], Payload) of
            {ok, {{_, 200, _}, _Headers, _Body}} ->
                ok;
            {ok, {{_, Status, Reason}, Headers, Body}} ->
                error_logger:error_report([
                    unexpected_http_response,
                    {module, ?MODULE},
                    {status, Status},
                    {reason, Reason},
                    {headers, Headers},
                    {body, Body}
                ]);
            {error, Reason} ->
                error_logger:error_report([
                    unexpected_http_error,
                    {module, ?MODULE},
                    {reason, Reason}
                ])
        end
    catch
        Class:Exception ->
            error_logger:error_report([
                {module, ?MODULE},
                {class, Class},
                {reason, Exception},
                {stacktrace, erlang:get_stacktrace()}
            ])
    end,
    State#{metrics := #{counters => [], gauges => []}}.

uri(#{endpoint := Endpoint}) -> binary_to_list(Endpoint).

post(URL, Headers, Body) ->
    httpc:request(post, {URL, Headers, "application/json", Body}, [{timeout, 5000}], [{body_format, binary}]).

metric(State, {Metric, DataPoint, Value, Time}) ->
    #{source := Source, prefix := Prefix} = State,
    #{
        name => name(lists:flatten([Prefix, Metric, DataPoint])),
        value => Value,
        measure_time => Time,
        source => Source
    }.

name([Key|Metric]) ->
    iolist_to_binary([atom_to_binary(Key, utf8), names(Metric)]).

names([]) ->
    [];
names([value]) ->
    [];
names([Key|Metric]) ->
    [$., atom_to_binary(Key, utf8)|names(Metric)].

auth(#{user := User, token := Token}) ->
    Hash = base64:encode_to_string(lists:append([User, ":", Token])),
    {"Authorization", "Basic " ++ Hash}.
