-module(lorareg_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test cases generated with https://www.loratools.nl/#/airtime and
%% truncated to milliseconds.
us_time_on_air_test() ->
    ?assertEqual(991, ms(lorareg:time_on_air(125.0e3, 12, 5, 8, true, 7, true))),
    ?assertEqual(2465, ms(lorareg:time_on_air(125.0e3, 12, 5, 8, true, 51, true))),

    ?assertEqual(495, ms(lorareg:time_on_air(125.0e3, 11, 5, 8, true, 7, true))),
    ?assertEqual(1314, ms(lorareg:time_on_air(125.0e3, 11, 5, 8, true, 51, true))),

    ?assertEqual(247, ms(lorareg:time_on_air(125.0e3, 10, 5, 8, true, 7, false))),
    ?assertEqual(616, ms(lorareg:time_on_air(125.0e3, 10, 5, 8, true, 51, false))),

    ?assertEqual(123, ms(lorareg:time_on_air(125.0e3, 9, 5, 8, true, 7, false))),
    ?assertEqual(328, ms(lorareg:time_on_air(125.0e3, 9, 5, 8, true, 51, false))),

    ?assertEqual(72, ms(lorareg:time_on_air(125.0e3, 8, 5, 8, true, 7, false))),
    ?assertEqual(184, ms(lorareg:time_on_air(125.0e3, 8, 5, 8, true, 51, false))),

    ?assertEqual(36, ms(lorareg:time_on_air(125.0e3, 7, 5, 8, true, 7, false))),
    ?assertEqual(102, ms(lorareg:time_on_air(125.0e3, 7, 5, 8, true, 51, false))),

    ok.

us915_dwell_time_test() ->
    MaxDwell = lorareg:max_dwell_time(millisecond),
    Period = lorareg:dwell_time_period(millisecond),
    %% There are no special frequencies in region US915, so the
    %% lorareg API doesn't care what values you use for Frequency
    %% arguments as long as they are distinct and comparable. We can
    %% use channel number instead like so.
    Chan0 = 0,
    Chan1 = 1,
    S0 = lorareg:new('US915'),
    S1 = lorareg:track_sent(S0, 0, Chan0, MaxDwell),
    S2 = lorareg:track_sent(S1, 0, Chan1, MaxDwell div 2),

    ?assertEqual(false, lorareg:can_send(S2, 0, Chan0, MaxDwell)),
    ?assertEqual(true, lorareg:can_send(S2, 0, Chan1, MaxDwell div 2)),

    ?assertEqual(false, lorareg:can_send(S2, 1, Chan0, MaxDwell)),
    ?assertEqual(true, lorareg:can_send(S2, 1, Chan1, MaxDwell div 2)),

    ?assertEqual(false, lorareg:can_send(S2, Period - 1, Chan0, MaxDwell)),
    ?assertEqual(true, lorareg:can_send(S2, Period - 1, Chan1, MaxDwell div 2)),

    ?assertEqual(true, lorareg:can_send(S2, Period, Chan0, MaxDwell)),
    ?assertEqual(true, lorareg:can_send(S2, Period, Chan1, MaxDwell)),

    ok.

eu868_duty_cycle_test() ->
    MaxDwell = lorareg:max_dwell_time(millisecond),
    S0 = lorareg:new('EU868'),
    Ten_ms = 10,
    Chan0 = 0,
    Chan1 = 1,

    ?assertEqual(true, lorareg:can_send(S0, 0, Chan0, MaxDwell)),
    ?assertEqual(false, lorareg:can_send(S0, 0, Chan0, MaxDwell + 1)),

    %% Send 3599 packets of duration 10ms on a single channel over the
    %% course of one hour. All should be accepted because 3599 * 10ms
    %% = 35.99s, or approx 0.9997 % duty-cycle.
    {S1, Now} = lists:foldl(
        fun (N, {State, _T}) ->
            Now = (N - 1) * 1000,
            ?assertEqual(true, lorareg:can_send(State, Now, Chan0, Ten_ms)),
            {lorareg:track_sent(State, Now, Chan0, Ten_ms), Now + 1000}
        end,
        {lorareg:new('EU868'), 0},
        lists:seq(1, 3599)
    ),

    %% Lets transmit on a different channel before rounding out 3600
    %% msg test, this is allowed because up to this point we've only
    %% transmitted on channel 0.
    ?assertEqual(true, lorareg:can_send(S1, Now, Chan1, Ten_ms)),

    %% This at this point, we can not send another packet at Now or
    %% else it would bring our duty-cycle to exactly 1 %
    ?assertEqual(false, lorareg:can_send(S1, Now, Chan0, Ten_ms)),

    ok.

%% Converts floating point seconds to integer seconds to remove
%% floating point ambiguity from test cases.
ms(Seconds) ->
    erlang:trunc(Seconds * 1000.0).
