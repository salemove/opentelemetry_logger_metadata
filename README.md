# OpentelemetryLoggerMetadata

Adds OpenTelemetry trace identifiers to logs.

## Installation

The package can be installed by adding `opentelemetry_logger_metadata` to your
list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_logger_metadata, "~> 0.1.0-rc"}
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
