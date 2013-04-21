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
%% @doc erlrrd wrappers

-module(rrd).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([create/1,

         fetch/5,
         fetch/6,

         update/2,
         update/3,

         lastupdate/1,
         lastupdate/2]).

-include("flukso.hrl").


create(Sensor) ->
    case file:read_file_info(path(base, Sensor)) of
        {error, enoent} ->
            Arg_base = erlrrd:c([
                path(base, Sensor),
                "-b 1199487600",
                "-s 60",
                "DS:meter:DERIVE:8640000:-20:20",
                "RRA:AVERAGE:0.5:1:1440",
                "RRA:AVERAGE:0.5:15:672",
                "RRA:AVERAGE:0.5:1440:365",
                "RRA:AVERAGE:0.5:10080:520"
            ]),
            Arg_night = erlrrd:c([
                path(night, Sensor),
                "-b 1199487600",
                "-s 86400",
                "DS:meter:GAUGE:8640000:-20:20",
                "RRA:AVERAGE:0.5:1:365",
                "RRA:AVERAGE:0.5:7:520"
            ]),
            erlrrd:create(Arg_base),
            erlrrd:create(Arg_night);

        {ok, _Fileinfo} ->
            {error, eexist}
    end.

fetch(Sensor, Start, End, Resolution, Factor) ->
    fetch(base, Sensor, Start, End, Resolution, Factor).

fetch(Rrd, Sensor, Start, End, Resolution, Factor) when is_integer(Start) ->
    fetch(Rrd,
          Sensor,
          integer_to_list(Start),
          integer_to_list(End),
          integer_to_list(Resolution),
          Factor);
fetch(Rrd, Sensor, Start, End, Resolution, Factor) ->
    Query = erlrrd:c([path(Rrd, Sensor), "AVERAGE",
                     ["-s ", Start], ["-e ", End], ["-r ", Resolution]]),

    case erlrrd:fetch(Query) of
         {ok, Response} ->
            Filtered = [re:split(X, "[:][ ]", [{return,list}])
                    || [X] <- Response, string:str(X, ":") == 11],

            Datapoints = [[list_to_integer(X), y(Y, Factor)]
                    || [X, Y] <- Filtered, (string:len(Y) /= 3) and (string:len(Y) /= 4)],

            % filter out nan and -nan
            Nans = [[list_to_integer(X), <<"nan">>]
                    || [X, Y] <- Filtered, (string:len(Y) == 3) or (string:len(Y) == 4)],

            {ok, Datapoints, Nans};

        {error, Reason} ->
            {error, Reason}
    end.


update(Sensor, Data) ->
    update(base, Sensor, Data).

update(Rrd, Sensor, Data) ->
    erlrrd:update([path(Rrd, Sensor), " ", Data]).


lastupdate(Sensor) ->
    lastupdate(base, Sensor).

lastupdate(Rrd, Sensor) ->
    {ok, [_, _, [UpdateString]]} = erlrrd:lastupdate(path(Rrd, Sensor)),
    [Timestamp, Counter] = re:split(UpdateString, "[:][ ]", [{return,list}]),
    
    [list_to_integer(Timestamp),
        case Counter of
            "UNKN" -> <<"NaN">>;
            _ -> list_to_integer(Counter)
        end].


% helper functions
path(Rrd, Sensor) ->
    case Rrd of
        base  -> Path = ?RRD_BASE_PATH;
        night -> Path = ?RRD_NIGHT_PATH
    end,

    [Path, [Sensor|".rrd"]].


y(Y, raw) ->
    list_to_float(Y);
y(Y, Factor) when is_number(Factor) ->
    round(list_to_float(Y) * Factor).

