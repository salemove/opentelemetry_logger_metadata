defmodule OpentelemetryLoggerMetadata do
  @moduledoc """
  Documentation for OpentelemetryLoggerMetadata.
  """

  def setup do
    :logger.add_primary_filter(:opentelemetry_logger_metadata, {&filter/2, []})
  end

  def filter(log_event, _config) do
    ctx = OpenTelemetry.Tracer.current_span_ctx()

    if ctx == :undefined do
      # Pass the unmodified log event to the next filters
      :ignore
    else
      trace_id = OpenTelemetry.Span.trace_id(ctx)
      span_id = OpenTelemetry.Span.span_id(ctx)

      # Can be simplified after https://github.com/open-telemetry/opentelemetry-erlang/pull/270
      hex_trace_id = :erlang.iolist_to_binary(:io_lib.format("~32.16.0b", [trace_id]))
      hex_span_id = :erlang.iolist_to_binary(:io_lib.format("~16.16.0b", [span_id]))

      Map.update(log_event, :meta, %{}, fn meta ->
        meta
        |> Map.put(:trace_id, hex_trace_id)
        |> Map.put(:span_id, hex_span_id)
      end)
    end
  end
end
