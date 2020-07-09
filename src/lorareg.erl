-module(lorareg).

-export([new/1, sent/4, time_on_air/2]).

-record(sent_packet, {
    sent_time :: integer(),
    time_on_air :: integer(),
    frequency :: integer()
}).

-type eu() :: {eu, [#sent_packet{}]}.

-type us() :: {eu, [#sent_packet{}]}.

%% Time over which we keep sent packet statistics for duty-cycle limited regions (EU).
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

%% @doc Updates State with time-on-air information calculated from
%% DataRate and Size.
%%
%% This function does not sent or transmit itself. It assumes a packet
%% was sent with DataRate and Size outside of this module
%% successfully.
-spec sent(eu() | us(), integer(), number(), integer()) -> eu() | us().
sent({Region, SentPackets}, Frequency, DataRate, Size) ->
    NewSent = #sent_packet{
        frequency = Frequency,
        sent_time = erlang:monotonic_time(),
        time_on_air = time_on_air(DataRate, Size)
    },
    {Region, SentPackets ++ [NewSent]}.

-spec time_on_air(number(), integer()) -> integer().
time_on_air(_DataRate, _Size) ->
    100.

new(eu) ->
    {eu, []};
new(us) ->
    {us, []}.
