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
         update/3]).

-define(RRD_BASE_PATH, "var/data/base/").
-define(RRD_NIGHT_PATH, "var/data/night/").


fetch(Sensor, Start, End, Resolution) ->
    fetch(?RRD_BASE_PATH, Sensor, Start, End, Resolution).

fetch(Path, Sensor, Start, End, Resolution) ->
    erlrrd:fetch(erlrrd:c([[Path, [Sensor|".rrd"]], "AVERAGE",
                           ["-s ", Start], ["-e ", End], ["-r ", Resolution]])).


update(Sensor, Data) ->
    update(?RRD_BASE_PATH, Sensor, Data).

update(Path, Sensor, Data) ->
    erlrrd:update([Path, [Sensor|".rrd"], " ", Data]).
