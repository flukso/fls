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
%% @doc Flukso logging

-module(flog).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([drupal/5]).

-include("flukso.hrl").
 
% log to Drupal's watchdog table
drupal(Uid, Type, Message, Severity, ReqData) when Severity =< ?LOGLEVEL ->
    mysql:execute(pool, watchdog,
        [Uid,
         Type,
         Message,
         <<"a:0:{}">>,
         Severity,
         list_to_binary(wrq:raw_path(ReqData)),
         list_to_binary(wrq:peer(ReqData)),
         check:unix()
        ]);
drupal(_Uid, _Type, _Message, _Severity, _ReqData) ->
    true.
