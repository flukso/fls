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

-export([fetch/4,
         fetch/5,

         update/2,
         update/3,

         lastupdate/1,
         lastupdate/2]).

-define(RRD_BASE_PATH, "var/data/base/").
-define(RRD_NIGHT_PATH, "var/data/night/").


fetch(Sensor, Start, End, Resolution) ->
    fetch(base, Sensor, Start, End, Resolution).

fetch(Rrd, Sensor, Start, End, Resolution) ->
    erlrrd:fetch(erlrrd:c([path(Rrd, Sensor), "AVERAGE",
                           ["-s ", Start], ["-e ", End], ["-r ", Resolution]])).


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
    
