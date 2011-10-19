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
%% @doc Wrapper for alarm handler

-module(alarm).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([start_link/0,
         threshold/1]).

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),
    %% Load the alarm handler
    gen_event:add_handler(?MODULE, alarm_hdlr, []),
    {ok, Pid}.

threshold(Thld) ->
    gen_event:notify(?MODULE, {threshold, Thld}).
