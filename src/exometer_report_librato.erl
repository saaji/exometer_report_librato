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
    Source = fun() -> list_to_binary(string:sub_word(atom_to_list(node()), 1, $@)) end,
    {ok, #{
        endpoint => get(endpoint, Options, ?ENDPOINT),
        user     => get(user, Options),
        token    => get(token, Options),
        source   => get(source, Options, Source),
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

exometer_newentry(Entry, State) ->
    error_logger:info_msg("Unknown entry ~p~n", [Entry]),
    {ok, State}.

exometer_info(Unknown, State) ->
    error_logger:info_msg("Unknown info ~p~n", [Unknown]),
    {ok, State}.

exometer_cast(Unknown, State) ->
    error_logger:info_msg("Unknown cast ~p~n", [Unknown]),
    {ok, State}.

exometer_call(Unknown, From, State) ->
    error_logger:info_msg("Unknown call ~p from ~p~n", [Unknown, From]),
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

send_metrics(#{metrics := Metrics, source := Source} = State) ->
    try
        #{counters := Counters, gauges := Gauges} = Metrics,
        Payload = jiffy:encode(#{
            counters => [metric(C, Source) || C <- Counters],
            gauges => [metric(G, Source) || G <- Gauges]
        }),

        case post(uri(State), [auth(State)], Payload) of
            {ok, {{_, 200, _}, _Headers, _Body}} ->
                ok;
            {ok, {{_, Status, Reason}, _Headers, _Body}} ->
                error_logger:error_msg("unexpected response: ~p ~s~n", [Status, Reason]);
            {error, Reason} ->
                error_logger:error_msg("unexpected error: ~p~n", [Reason])
        end
    catch
        E:R -> error_logger:error_msg("unexpected error ~p ~p~n", [E, R])
    end,
    State#{metrics := #{counters => [], gauges => []}}.

uri(#{endpoint := Endpoint}) -> binary_to_list(Endpoint).

post(URL, Headers, Body) ->
    httpc:request(post, {URL, Headers, "application/json", Body}, [{timeout, 5000}], [{body_format, binary}]).

metric({Metric, DataPoint, Value, Time}, Source) ->
    #{
        name => name(Metric ++ [DataPoint]),
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
