-module(lorareg).

-export([
    new/1,
    time_on_air/7,
    track_sent/3,
    track_sent/9
]).

-record(sent_packet, {
    sent_time :: number(),
    time_on_air :: number(),
    frequency :: number()
}).

-type eu() :: {eu, [#sent_packet{}]}.

-type us() :: {eu, [#sent_packet{}]}.

%% Time over which we keep sent packet statistics for duty-cycle
%% limited regions (EU).
%%
%% In order to calculate duty cycle, we track every single
%% transmission 'now' and the previous DUTY_CYCLE_PERIOD of time. Note
%% that 'now' is always changing and that transmissions older than
%% DUTY_CYCLE_PERIOD are only untracked when updating state or
%% calculating duty-cycle.
-define(DUTY_CYCLE_PERIOD, {hour, 1}).

%% Maximum time on air for dell-time limited regions (US).
-define(MAX_DWELL_TIME, {millisecond, 400}).

%% Time over which enforce MAX_DWELL_TIME.
-define(DWELL_TIME_PERIOD, {seconds, 20}).

%% @doc Updates State with time-on-air information.
%%
%% This function does not sent or transmit itself.
-spec track_sent(
    eu() | us(),
    number(),
    number(),
    integer(),
    integer(),
    integer(),
    boolean(),
    integer(),
    boolean()
) ->
    eu() | us().
track_sent(
    Handle,
    Frequency,
    Bandwidth,
    SpreadingFactor,
    CodeRate,
    PreambleSymbols,
    ExplicitHeader,
    PayloadLen,
    LowDatarateOptimized
) ->
    TimeOnAir = time_on_air(
        Bandwidth,
        SpreadingFactor,
        CodeRate,
        PreambleSymbols,
        ExplicitHeader,
        PayloadLen,
        LowDatarateOptimized
    ),
    track_sent(Handle, Frequency, TimeOnAir).

-spec track_sent(eu() | us(), number(), number()) -> eu() | us().
track_sent({Region, SentPackets}, Frequency, TimeOnAir) ->
    Now = erlang:monotonic_time(millisecond),
    NewSent = #sent_packet{
        frequency = Frequency,
        sent_time = Now,
        time_on_air = TimeOnAir
    },
    {Region, [NewSent | trim_sent(Region, SentPackets, Now)]}.

trim_sent(us, SentPackets, Now) ->
    %% TODO: use DWELL_TIME_PERIOD instead of hardcoded value
    Pred = fun (Sent) -> Sent#sent_packet.sent_time + 20000 > Now end,
    lists:takewhile(Pred, SentPackets);
trim_sent(eu, SentPackets, Now) ->
    %% TODO: use DUTY_CYCLE_PERIOD instead of hardcoded value
    Pred = fun (Sent) -> Sent#sent_packet.sent_time + 3600000 > Now end,
    lists:takewhile(Pred, SentPackets).

%% @doc Returns total time on air for packet sent with given
%% parameters.
%%
%% See Semtech Appnote AN1200.13, "LoRa Modem Designer's Guide"
-spec time_on_air(
    number(),
    number(),
    integer(),
    integer(),
    boolean(),
    integer(),
    boolean()
) ->
    float().
time_on_air(
    Bandwidth,
    SpreadingFactor,
    CodeRate,
    PreambleSymbols,
    ExplicitHeader,
    PayloadLen,
    LowDatarateOptimized
) ->
    SymbolDuration = symbol_duration(Bandwidth, SpreadingFactor),
    PayloadSymbols = payload_symbols(
        SpreadingFactor,
        CodeRate,
        ExplicitHeader,
        PayloadLen,
        LowDatarateOptimized
    ),
    SymbolDuration * (4.25 + PreambleSymbols + PayloadSymbols).

%% @doc Returns the number of payload symbols required to send payload.
-spec payload_symbols(integer(), integer(), boolean(), integer(), boolean()) -> number().
payload_symbols(
    SpreadingFactor,
    CodeRate,
    ExplicitHeader,
    PayloadLen,
    LowDatarateOptimized
) ->
    EH = b2n(ExplicitHeader),
    LDO = b2n(LowDatarateOptimized),
    8 +
        (erlang:max(
            math:ceil(
                (8 * PayloadLen - 4 * SpreadingFactor + 28 +
                    16 - 20 * (1 - EH)) /
                    (4 * (SpreadingFactor - 2 * LDO))
            ) * (CodeRate),
            0
        )).

-spec symbol_duration(number(), number()) -> float().
symbol_duration(Bandwidth, SpreadingFactor) ->
    math:pow(2, SpreadingFactor) / Bandwidth.

new(eu) ->
    {eu, []};
new(us) ->
    {us, []}.

-spec b2n(atom()) -> integer().
b2n(false) ->
    0;
b2n(true) ->
    1.
