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

-module(threshold).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

% remove before flight!
% -compile(export_all).

-export([run/1]).

-include("flukso.hrl").
-include("threshold.hrl").


run(Sensor) ->
    {data, Alarms} = mysql:execute(pool, alarm_sensor_load, [Sensor]),
    Thlds = lists:map(fun to_thld/1, mysql:get_result_rows(Alarms)),
    lists:map(fun check/1, Thlds).

% helpers
to_thld([Sensor, Type, Resolution, Threshold, State, Timestamp]) ->
    #thld{
        sensor = Sensor,
        type = Type,
        resolution = Resolution,
        threshold = Threshold,
        state = State,
        s_timestamp = Timestamp}. 


check(#thld{state = ?DISABLED}) ->
    {ok, disabled};
check(#thld{resolution = ?NIGHT} = Thld) ->
    check(night, Thld#thld{resolution = ?DAY});
check(Thld) ->
    check(base, Thld).        

check(Rrd, #thld{sensor = Sensor, s_timestamp = Start, resolution = Resolution} = Thld) ->
    io:format("data: ~s, ~p, ~p, ~p, ~p, ~p~n",
              [Thld#thld.sensor,
               Thld#thld.type,
               Thld#thld.resolution,
               Thld#thld.threshold,
               Thld#thld.state,
               Thld#thld.s_timestamp]),

    End = check:unix(),

    case rrd:fetch(Rrd, Sensor, Start, End, Resolution, raw) of
        {ok, [], _Nans} ->
            {ok, nodata};

        {ok, Datapoints, _Nans} ->
            [Timestamp, Value] = lists:last(Datapoints),
            action(Thld#thld{v_timestamp = Timestamp, value = Value});

        {error, Reason} ->
            {error, Reason}
    end.


action(#thld{type = ?HIGH, state = ?CLEARED} = Thld) when Thld#thld.value >= Thld#thld.threshold ->
    update(Thld#thld{state = ?RAISED,  s_timestamp = Thld#thld.v_timestamp});
action(#thld{type = ?HIGH, state = ?RAISED}  = Thld) when Thld#thld.value <  Thld#thld.threshold ->
    update(Thld#thld{state = ?CLEARED, s_timestamp = Thld#thld.v_timestamp});
action(#thld{type = ?LOW,  state = ?CLEARED} = Thld) when Thld#thld.value =< Thld#thld.threshold ->
    update(Thld#thld{state = ?RAISED,  s_timestamp = Thld#thld.v_timestamp});
action(#thld{type = ?LOW,  state = ?RAISED}  = Thld) when Thld#thld.value >  Thld#thld.threshold ->
    update(Thld#thld{state = ?CLEARED, s_timestamp = Thld#thld.v_timestamp});
action(_) ->
    {ok, nochange}.


update(#thld{sensor = Sensor, state = State, s_timestamp = Timestamp} = Thld) ->
    alarm:threshold(Thld),

    case mysql:execute(pool, alarm_sensor_update, [State, Timestamp, Sensor]) of
        {updated, _Result} -> {ok, updated};
        {error, Reason} -> {error, Reason}
    end.
