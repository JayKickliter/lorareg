-module(lorareg_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test cases generated with https://www.loratools.nl/#/airtime and
%% truncated to milliseconds.
time_on_air_test() ->
    991 = ms(lorareg:time_on_air(125.0e3, 12, 5, 8, true, 7, true)),
    2465 = ms(lorareg:time_on_air(125.0e3, 12, 5, 8, true, 51, true)),

    495 = ms(lorareg:time_on_air(125.0e3, 11, 5, 8, true, 7, true)),
    1314 = ms(lorareg:time_on_air(125.0e3, 11, 5, 8, true, 51, true)),

    247 = ms(lorareg:time_on_air(125.0e3, 10, 5, 8, true, 7, false)),
    616 = ms(lorareg:time_on_air(125.0e3, 10, 5, 8, true, 51, false)),

    123 = ms(lorareg:time_on_air(125.0e3, 9, 5, 8, true, 7, false)),
    328 = ms(lorareg:time_on_air(125.0e3, 9, 5, 8, true, 51, false)),

    72 = ms(lorareg:time_on_air(125.0e3, 8, 5, 8, true, 7, false)),
    184 = ms(lorareg:time_on_air(125.0e3, 8, 5, 8, true, 51, false)),

    36 = ms(lorareg:time_on_air(125.0e3, 7, 5, 8, true, 7, false)),
    102 = ms(lorareg:time_on_air(125.0e3, 7, 5, 8, true, 51, false)),

    ok.

%% Converts floating point seconds to integer seconds to remove
%% floating point ambiguity from test cases.
ms(Seconds) ->
    erlang:trunc(Seconds * 1000.0).
