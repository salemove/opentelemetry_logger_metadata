# OpentelemetryLoggerMetadata

Adds OpenTelemetry trace identifiers to logs.

## Installation

### Elixir

The package can be installed by adding `opentelemetry_logger_metadata` to your
list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_logger_metadata, "~> 0.1.0"}
  ]
end
```

In your application start:

```elixir
    def start(_type, _args) do
      OpenTelemetry.register_application_tracer(:my_project)
      OpentelemetryLoggerMetadata.setup()

      # ...
    end
```

### Erlang

The package can be installed by adding `opentelemetry_logger_metadata` to your
list of dependencies:

```
  {deps, [{opentelemetry_logger_metadata, "~> 0.1.0"}]}.
```

In your application start:

```erlang
  opentelemetry_logger_metadata:setup()
```

or setup the logging filter yourself:

```erlang
  logger:add_primary_filter(opentelemetry_logger_metadata, {fun opentelemetry_logger_metadata:filter/2, []}),
```
