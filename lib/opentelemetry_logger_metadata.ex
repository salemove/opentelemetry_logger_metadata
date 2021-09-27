defmodule OpentelemetryLoggerMetadata do
  @moduledoc """
  Adds OpenTelemetry trace identifiers to logs.

  In your application start:

  ```elixir
      def start(_type, _args) do
        OpenTelemetry.register_application_tracer(:my_project)
        OpentelemetryLoggerMetadata.setup()

        # ...
      end
  ```
  """

  def setup do
    :opentelemetry_logger_metadata.setup()
  end
end
