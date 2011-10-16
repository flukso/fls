%% @author Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
%% @copyright (C) 2009-2011 Bart Van Der Meerssche
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
%% @doc Flukso module spec 

-module(flukso).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.


%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    sql:prepare(),
    ensure_started(webmachine),
    flukso_sup:start_link().

%% @spec start() -> ok
%% @doc Start the flukso server.
start() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    sql:prepare(),
    ensure_started(webmachine),
    application:start(flukso).

%% @spec stop() -> ok
%% @doc Stop the flukso server.
stop() -> 
    Res = application:stop(flukso),
    application:stop(webmachine),
    application:stop(mysql),
    application:stop(erlrrd),
    application:stop(crypto),
    Res.
