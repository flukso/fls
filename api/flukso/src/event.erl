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
%% @doc Wrapper for event handler 

-module(event).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([start_link/0,
         sensor_update/1]).

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),
    %% Load the threshold handler
    gen_event:add_handler(?MODULE, event_hdlr, []),
    {ok, Pid}.

sensor_update(Sensor) ->
    gen_event:notify(?MODULE, {sensor_update, Sensor}).
