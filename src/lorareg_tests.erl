-module(lorareg_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test cases generated with https://www.loratools.nl/#/airtime and
%% truncated to milliseconds.
time_on_air_test() ->
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

%% Converts floating point seconds to integer seconds to remove
%% floating point ambiguity from test cases.
ms(Seconds) ->
    erlang:trunc(Seconds * 1000.0).

dwell_time_test() ->
    S0 = lorareg:new(us),
    S1 = lorareg:track_sent(S0, 0, 915000000, 400),
    ?assertEqual(false, lorareg:can_send(S1, 100, 915000000)),
    ?assertEqual(true,  lorareg:can_send(S1, 500, 915100000)),

    ok.
