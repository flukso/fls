%% @author Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
%% @copyright (C) 2011 Bart Van Der Meerssche
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%
%% @doc Sensor threshold alarm functions 

-module(alarm).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

% remove before flight!
-compile(export_all).

-export([init/0, run/1]).

-include("flukso.hrl").
-include("alarm.hrl").

init() ->
    io:format("test").

run(Sensor) ->
    {data, Alarms} = mysql:execute(pool, alarm_sensor_load, [Sensor]),
    lists:map(fun check/1, mysql:get_result_rows(Alarms)).

% helpers
check([_, _, _, _, ?DISABLED, _]) ->
    {ok, disabled};
check([Sensor, Type, ?NIGHT, Threshold, State, Start]) ->
    check(night, [Sensor, Type, ?DAY, Threshold, State, Start]);        
check(Alarm) ->
    check(base, Alarm).        

check(Rrd, [Sensor, Type, Resolution, Threshold, State, Start]) ->
    io:format("data: ~s, ~p, ~p, ~p, ~p, ~p~n",
              [Sensor, Type, Resolution, Threshold, State, Start]),

    End = check:unix(),

    case rrd:fetch(Rrd, Sensor, Start, End, Resolution, raw) of
        {ok, [], _Nans} ->
            {ok, nodata};

        {ok, Datapoints, _Nans} ->
            [Timestamp, Value] = lists:last(Datapoints),
            action(Type, State, Threshold, Value, Timestamp);

        {error, Reason} ->
            {error, Reason}
    end.

action(?HIGH, ?CLEARED, Threshold, Value, Timestamp) when Value >= Threshold ->
    {ok, raise};
action(?HIGH, ?RAISED,  Threshold, Value, Timestamp) when Value <  Threshold ->
    {ok, clear};
action(?LOW,  ?CLEARED, Threshold, Value, Timestamp) when Value =< Threshold ->
    {ok, raise};
action(?LOW,  ?RAISED,  Threshold, Value, Timestamp) when Value >  Threshold ->
    {ok, clear};
action(_, _, _, _, _) ->
    {ok, nochange}.
