# Librato Reporter for Exometer

An [Exometer][1] reporter for [Librato][2].

## Build

This project uses [Rebar3][3]. It is assumed you have `rebar3` on your path or
that you are using a Rebar 3 compatible build system.

To compile the code stand alone, run:

```
$ rebar3 compile
```

## Dependency

To add the reporter as a dependency, use the following dependency in your
`rebar.config`:

```erl
{exometer_report_librato, {git, "https://github.com/eproxus/exometer_report_librato.git"}}
```

If you have an Elixir project, use the following dependency in your `mix.exs`:

```elixir
{:exometer_report_librato, github: "eproxus/exometer_report_librato"}
```

## Configure

The reporter can be configured with the following options:

* `endpoint` *(string)*

    The HTTP endpoint to talk to.

    *Default:* `https://metrics-api.librato.com/v1/metrics`

* `user` *(string)* **(required)**

    The user name used for Librato authentication.

* `token` *(string)* **(required)**

    The API token used for Librato authentication.

* `source` *(string)*

    The source to report to Librato. The default value only uses the node name
    excluding the host name, because Librato does not support @ characters in a
    source name.

    If the source option is set to `nodename`, the name part of the Erlang node
    name will be used. If it is set to `hostname`, the host name part will be
    used.

    If the source option is a fun, that fun will be called when the reporter
    is initialized and the return value will be used as the source name.

    If the source option is a string, that string will be used.

    *Default:* `nodename`

* `interval` *(integer)*

    The interval in milliseconds to send metric batches to Librato.

    *Default:*  `10000`

* `type_map` *(proplist)*

    A type map proplist mapping Exometer metrics to Librato types. If no
    matching type is found in this map, this reporter defaults to `gauge`.

    Example: `[{[myapp, property], counter}, {[erlang, memory, '_'], gauge}]`

    *Default:* `[]`

[1]: https://github.com/Feuerlabs/exometer_core
[2]: https://www.librato.com/
[3]: http://www.rebar3.org/
