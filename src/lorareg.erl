-module(lorareg).

-export([
    can_send/2,
    can_send/3,
    dwell_time/3,
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

-type region() :: eu | us.

-type handle() :: {region(), list(#sent_packet{})}.

%% @doc Time over which we keep sent packet statistics for duty-cycle
%% limited regions (EU).
%%
%% In order to calculate duty cycle, we track every single
%% transmission 'now' and the previous DUTY_CYCLE_PERIOD_MS of
%% time. Note that 'now' is always changing and that transmissions
%% older than DUTY_CYCLE_PERIOD_MS are only untracked when updating
%% state or calculating duty-cycle.
-define(DUTY_CYCLE_PERIOD_MS, 3600000).

%% Maximum time on air for dell-time limited regions (US).
%%
%% See 47 CFR 15.247.
-define(MAX_DWELL_TIME_MS, 400).

%% Time over which enforce MAX_DWELL_TIME_MS.
-define(DWELL_TIME_PERIOD_MS, 20000).

%% Updates Handle with time-on-air information.
%%
%% This function does not send/transmit itself.
-spec track_sent(
    Handle :: handle(),
    Frequency :: number(),
    Bandwidth :: number(),
    SpreadingFactor :: integer(),
    CodeRate :: integer(),
    PreambleSymbols :: integer(),
    ExplicitHeader :: boolean(),
    PayloadLen :: integer(),
    LowDatarateOptimized :: boolean()
) ->
    handle().
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

-spec track_sent(handle(), number(), number()) -> handle().
track_sent({Region, SentPackets}, Frequency, TimeOnAir) ->
    Now = erlang:monotonic_time(millisecond),
    NewSent = #sent_packet{
        frequency = Frequency,
        sent_time = Now,
        time_on_air = TimeOnAir
    },
    {Region, [NewSent | trim_sent(Region, SentPackets, Now)]}.

%% @doc trims list of previous transmissions that are too old and no
%% longer needed to compute regulatory compliance.
-spec trim_sent(region(), list(#sent_packet{}), integer()) -> list(#sent_packet{}).
trim_sent(us, SentPackets, Now) ->
    CutoffTime = Now - ?DWELL_TIME_PERIOD_MS,
    Pred = fun (Sent) -> Sent#sent_packet.sent_time > CutoffTime end,
    lists:takewhile(Pred, SentPackets);
trim_sent(eu, SentPackets, Now) ->
    CutoffTime = Now - ?DUTY_CYCLE_PERIOD_MS,
    Pred = fun (Sent) -> Sent#sent_packet.sent_time > CutoffTime end,
    lists:takewhile(Pred, SentPackets).

%% @doc Based on previously sent packets, returns a boolean value if
%% it is legal to send on Frequency.
-spec can_send(handle(), number()) -> boolean().
can_send(Handle, Frequency) ->
    Now = erlang:monotonic_time(millisecond),
    can_send(Handle, Frequency, Now).

%% @doc Based on previously sent packets, returns a boolean value if
%% it is legal to send on Frequency at time Now.
-spec can_send(handle(), number(), integer()) -> boolean().
can_send({us, SentPackets}, Frequency, Now) ->
    CutoffTime = Now - ?DWELL_TIME_PERIOD_MS,
    dwell_time(SentPackets, CutoffTime, Frequency) < ?MAX_DWELL_TIME_MS;
can_send({eu, _SentPackets}, _Frequency, _Now) ->
    false.

%% @doc Computes the total time on air for packets sent on Frequency
%% and no older than CutoffTime.
-spec dwell_time(list(#sent_packet{}), integer(), number()) -> number().
dwell_time(SentPackets, CutoffTime, Frequency) ->
    dwell_time(SentPackets, CutoffTime, Frequency, 0).

-spec dwell_time(list(#sent_packet{}), integer(), number(), number()) -> number().
dwell_time([P | _], CutoffTime, _Frequency, Acc)
        when P#sent_packet.sent_time < CutoffTime ->
    Acc;
dwell_time([P | T], CutoffTime, Frequency, Acc) when P#sent_packet.frequency == Frequency ->
    dwell_time(T, CutoffTime, Frequency, Acc + P#sent_packet.time_on_air);
dwell_time([], _CutoffTime, _Frequency, Acc) ->
    Acc.

%% @doc Returns total time on air for packet sent with given
%% parameters.
%%
%% See Semtech Appnote AN1200.13, "LoRa Modem Designer's Guide"
-spec time_on_air(
    Bandwidth :: number(),
    SpreadingFactor :: number(),
    CodeRate :: integer(),
    PreambleSymbols :: integer(),
    ExplicitHeader :: boolean(),
    PayloadLen :: integer(),
    LowDatarateOptimized :: boolean()
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

-spec b2n(boolean()) -> integer().
b2n(false) ->
    0;
b2n(true) ->
    1.
