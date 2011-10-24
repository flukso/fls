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

-export([run/1,
         mail/1]).

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


mail(#thld{sensor = Sensor} = Thld) ->
    case mysql:execute(pool, alarm_sensor_props, [Sensor]) of
        {data, Result} ->
            [Props] = mysql:get_result_rows(Result),
            [Name, To, Type, Function] = lists:map(fun erlang:binary_to_list/1, Props),

            Subject = string:join(subject(Function, Thld), ""),
            Hdr = string:join(header(To, Subject), "\r\n"),
            Bdy = string:join(body(Thld, Name, Type, Function), "\r\n"),
            Msg = string:join([Hdr, Bdy], "\r\n"),

            esmtp:send(To, Msg);

        {error, Reason} ->
            {error, Reason}
    end.

subject(Function, #thld{resolution = Resolution, state = State}) ->
    ["[",
     Function,
     "] ",
     seconds_to_str(Resolution),
     " threshold alarm ",
     state_to_str(State)
    ].

header(To, Subject) ->
    ["To: " ++ To,
     "From: " ++ esmtp_app:config(default_from),
     "Organization: " ++ esmtp_app:config(organisation),
     "User-Agent: esmtp",
     "Errors-To: " ++ esmtp_app:config(postmaster),
     "Bounces-To: " ++ esmtp_app:config(postmaster),
     "MIME-Version: 1.0",
     "Content-Type: text/plain; charset=utf-8",
     "Content-Transfer-Encoding: 7bit",
     "Date: " ++ httpd_util:rfc1123_date(),
     "Subject: " ++ Subject
    ].

body(#thld{sensor = Sensor, type = ThldType, resolution = Resolution, threshold = Threshold, state = State, s_timestamp = Timestamp, value = Value}, Name, Type, Function) ->
    ["Hi " ++ Name ++ ",",
     "",
     "One of your threshold alarms has just changed state:",
     "",
     "sensor id: " ++ binary_to_list(Sensor),
     "sensor type: " ++ Type, 
     "sensor name: " ++ Function,
     "sensor value: " ++ value_to_str(Type, Value),
     "",
     "alarm type: " ++ type_to_str(ThldType),
     "alarm resolution: " ++ seconds_to_str(Resolution),
     "alarm threshold: "  ++ value_to_str(Type, Threshold),
     "alarm state: " ++ state_to_str(State),
     "alarm timestamp: " ++ unix_to_rfc1123(Timestamp),
     "",
     "Consult your charts on www.flukso.net to see what's going on.",
     "",
     "Happy metering!",
     esmtp_app:config(organisation)
    ].

seconds_to_str(?MINUTE) ->
    "minute";
seconds_to_str(?QUARTER) ->
    "15min";
seconds_to_str(?HOUR) ->
    "hour";
seconds_to_str(?DAY) ->
    "day";
seconds_to_str(?NIGHT) ->
    "night";
seconds_to_str(?WEEK) ->
    "week".

type_to_str(?HIGH) ->
    "high";
type_to_str(?LOW) ->
    "low".

state_to_str(?RAISED) ->
    "raised";
state_to_str(?CLEARED) ->
    "cleared".

value_to_str("electricity", Value) ->
    value_to_str(?FACTOR_WATT, Value, "watt");
value_to_str("water", Value) ->
    value_to_str(?FACTOR_LPERDAY, Value, "l/day");
value_to_str("gas", Value) ->
    value_to_str(?FACTOR_LPERDAY, Value, "l/day").

value_to_str(Factor, Value, Unit) ->
    integer_to_list(round(Factor * Value)) ++ " " ++ Unit.

unix_to_rfc1123(EpochSeconds) ->
    BaseDate  = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds   = BaseDate + EpochSeconds,
    DateTime  = calendar:gregorian_seconds_to_datetime(Seconds),
    httpd_util:rfc1123_date(DateTime).
