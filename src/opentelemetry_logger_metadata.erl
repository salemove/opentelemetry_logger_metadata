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
            HexTraceId = otel_span:hex_trace_id(SpanCtx),
            HexSpanId = otel_span:hex_span_id(SpanCtx),

            #{meta := Meta} = LogEvent,
            MetaWithTraceInformation = Meta#{trace_id => HexTraceId, span_id => HexSpanId},
            LogEvent#{meta => MetaWithTraceInformation}
    end.
