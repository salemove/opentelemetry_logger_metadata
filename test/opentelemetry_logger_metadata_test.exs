defmodule OpentelemetryLoggerMetadataTest do
  use ExUnit.Case
  doctest OpentelemetryLoggerMetadata

  import ExUnit.CaptureLog

  require Logger
  require OpenTelemetry.Tracer

  setup do
    Logger.configure_backend(
      :console,
      format: {LogstashLoggerFormatter, :format},
      colors: [enabled: false],
      metadata: :all
    )

    OpentelemetryLoggerMetadata.setup()

    :ok
  end

  test "does not add trace identifiers when not in a trace" do
    {_, message} =
      with_log(
        [
          colors: [enabled: false],
          format: {LogstashLoggerFormatter, :format},
          metadata: :all
        ],
        fn ->
          Logger.warning("Test message")
        end
      )

    decoded_message = Jason.decode!(message)
    refute Map.has_key?(decoded_message, "trace_id")
    refute Map.has_key?(decoded_message, "span_id")
  end

  test "adds trace_id and span_id to log metadata when in a trace" do
    OpenTelemetry.Tracer.with_span "test span" do
      {_, message} =
        with_log(
          [
            colors: [enabled: false],
            format: {LogstashLoggerFormatter, :format},
            metadata: :all
          ],
          fn ->
            Logger.warning("Test message")
          end
        )

      decoded_message = Jason.decode!(message)
      assert Map.has_key?(decoded_message, "trace_id")
      assert Map.has_key?(decoded_message, "span_id")
    end
  end
end
