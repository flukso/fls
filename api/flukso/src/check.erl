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
%% @doc helper functions running checks against API parameters 

-module(check).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([version/1,
         version/2,

         sensor/1,
         device/1,
         uid/1,
 
         token/2,
         digest/1,
         session/1,
         session_name/0,

         times/4,

         unit/1,
         jsonp/1,
         param/1,
         update/1,

         unix/0]).

-include("flukso.hrl"). 


version(Version) ->
    case Version of
        "1.0" -> {Version, true};
        _ -> {false, false}
    end.

version(undefined, undefined) ->
    {false, false};
version(Version, undefined) ->
    version(Version);
version(undefined, Version) ->
    version(Version);
version(_, _) ->
    {false, false}.

sensor(Sensor) ->
    hex(Sensor, 32).

device(Device) ->
    hex(Device, 32).

uid(Uid) ->
    UidInt = (catch list_to_integer(Uid)),
    {UidInt, is_number(UidInt)}.

token(undefined, undefined) ->
    {false, false};
token(Token, undefined) ->
    hex(Token, 32);
token(undefined, Token) ->
    hex(Token, 32);
token(_, _) ->
    {false, false}.

digest(Digest) ->
    hex(Digest, 40).

session(undefined) ->
    {false, false};
session(Session) ->
    alphanum(Session, 26).

session_name() ->
    {ok, Cookie_domain} = application:get_env(flukso, cookie_domain),
    <<X:128/big-unsigned-integer>> = erlang:md5(Cookie_domain),
    "SESS" ++ lists:flatten(io_lib:format("~32.16.0b", [X])).

% local
hex(String, Length) ->
    case re:run(String, "[0-9a-f]+", []) of 
        {match, [{0, Length}]} -> {String, true};
        _ -> {false, false}
    end.

alphanum(String, Length) ->
    case re:run(String, "[0-9a-z]+", []) of 
        {match, [{0, Length}]} -> {String, true};
        _ -> {false, false}
    end.

times(undefined, undefined, _End, _Resolution) ->
    {false, false, false, false};
times(Interval, undefined, undefined, undefined) ->
    case default_resolution(Interval) of
        false ->
            {false, false, false, false};
        DefResolution ->
            times(Interval, undefined, undefined, DefResolution)
    end;
times(Interval, undefined, undefined, Resolution) ->
    Now = unix(),
    case {time_to_seconds(Interval), time_to_seconds(Resolution)} of
        {false, _} ->
            {false, false, false, false};
        {_, false} ->
            {false, false, false, false};
        {IntervalSec, ResolutionSec} -> 
            % since the current interval will always return NaN
            % we exclude it by subtracting ResolutionSec from Now
            AlignedEnd =
                align(up, Now - ResolutionSec, ResolutionSec),

            % make sure we return IntervalSec/ResolutionSec entries
            % so we add ResolutionSec to AlignedEnd - IntervalSec
            AlignedStart =
                align(down, AlignedEnd - IntervalSec + ResolutionSec, ResolutionSec),

            {AlignedStart, AlignedEnd, ResolutionSec, true}
    end;
times(undefined, Start, undefined, Resolution) ->
    times(undefined, Start, integer_to_list(unix()), Resolution);
times(undefined, Start, End, undefined) ->
    times(undefined, Start, End, "minute");
times(undefined, Start, End, Resolution) ->
    ReStart = re:run(Start, "[0-9]+", []),
    ReEnd = re:run(End, "[0-9]+", []),

    case {ReStart, ReEnd, time_to_seconds(Resolution)} of
        {_, _, false} ->
            {false, false, false, false};
        {{match, [{0,_}]}, {match, [{0,_}]}, ResolutionSec} ->
            AlignedStart =
                align(down, list_to_integer(Start), ResolutionSec),

            AlignedEnd =
                align(up, list_to_integer(End), ResolutionSec),

            {AlignedStart, AlignedEnd, ResolutionSec, true};

        _ -> {false, false, false, false}
    end;
times(_, _, _, _) ->
    {false, false, false, false}.

unit(Unit) ->
    Units = [{"watt", 3600},
             {"kwhperyear", 31536},
             {"eurperyear", 5676},
             {"audperyear", 5991},
             {"lpermin", 60},
             {"lperday", 24 * 60 * 60},
             {"m3peryear", 365 * 24 * 3600 / 1000}],

    case lists:keyfind(Unit, 1, Units) of
        false -> {false, false};
        {_Unit, Factor} -> {Factor, true}
    end.

jsonp(undefined) ->
    {undefined, true};
jsonp(Jsonp) ->
    Length = string:len(Jsonp),

    case re:run(Jsonp, "[0-9a-zA-Z_]+", []) of
        {match, [{0, Length}]} -> {Jsonp, true};
        _ -> {false, false}
    end.

param(undefined) ->
    {undefined, false};
param("all") ->
    {"all", true};
param(_) ->
    {false, false}.

update(undefined) ->
    {undefined, false};
update("true") ->
    {"true", true};
update(_) ->
    {false, false}.


%% helper functions
unix() ->
    {Megaseconds, Seconds, _Microseconds} = now(),
    Megaseconds*1000000 + Seconds.

align(down, Time, Resolution) ->
    (Time div Resolution) * Resolution;
align(up, Time, Resolution) ->
    % subtract 1 from Time to make sure we don't return
    % an extra entry when Time is a multiple of Resolution
    ((Time - 1) div Resolution) * Resolution.

default_resolution(Interval) ->
    DefResolutions = [{"15min", "minute"},
                      {"hour", "minute"},
                      {"day", "15min"},
                      {"week", "day"},
                      {"month", "day"},
                      {"year", "week"},
                      {"decade", "week"},
                      {"night", "day"}],

    case lists:keyfind(Interval, 1, DefResolutions) of
        false -> false;
        {_Interval, Defresolution} -> Defresolution
    end.

time_to_seconds(Time) ->
    Times = [{"minute", ?MINUTE},
             {"15min", ?QUARTER},
             {"hour", ?HOUR},
             {"day", ?DAY},
             {"week", ?WEEK},
             {"month", ?MONTH},
             {"year", ?YEAR},
             {"decade", ?DECADE},
             {"night", ?MONTH}],

    case lists:keyfind(Time, 1, Times) of
        false -> false;
        {_Time, TimeSec} -> TimeSec
    end.
