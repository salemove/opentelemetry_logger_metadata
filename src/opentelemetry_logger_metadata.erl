-module(opentelemetry_logger_metadata).

-export([setup/0,
         filter/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-spec setup() -> ok.
setup() ->
    logger:add_primary_filter(opentelemetry_logger_metadata, {fun filter/2, []}),
    ok.

filter(LogEvent, _Config) ->
    case otel_tracer:current_span_ctx() of
        undefined ->
            ignore;
        SpanCtx ->
            TraceId = otel_span:trace_id(SpanCtx),
            SpanId = otel_span:span_id(SpanCtx),

            %% hex_trace_id and hex_span_id can be used after
            %% https://github.com/open-telemetry/opentelemetry-erlang/commit/a97f5b7f
            %% is released
            HexTraceId = iolist_to_binary(io_lib:format("~32.16.0b", [TraceId])),
            HexSpanId = iolist_to_binary(io_lib:format("~16.16.0b", [SpanId])),

            #{meta := Meta} = LogEvent,
            MetaWithTraceInformation = Meta#{trace_id => HexTraceId, span_id => HexSpanId},
            LogEvent#{meta => MetaWithTraceInformation}
    end.
