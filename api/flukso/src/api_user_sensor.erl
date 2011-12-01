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
%% @doc Flukso API: /user/xyz/sensor resource specification 

-module(api_user_sensor).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("flukso.hrl").


init([]) -> 
    {ok, undefined}.

% debugging
%init(Config) ->
%   {{trace, "/tmp"}, Config}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

malformed_request(ReqData, _State) ->
    {_Version, ValidVersion} =
        check:version(wrq:get_req_header("X-Version", ReqData),
                      wrq:get_qs_value("version", ReqData)),

    {Uid, ValidUid} =
        check:uid(wrq:path_info(uid, ReqData)),

    {Session, ValidSession} =
        check:session(wrq:get_cookie_value(check:session_name(), ReqData)),

    {Jsonp, ValidJsonp} =
        check:jsonp(wrq:get_qs_value("callback", ReqData)),

    State = #state{uid     = Uid,
                   session = Session,
                   jsonp   = Jsonp},

    {case {ValidVersion, ValidUid, ValidSession, ValidJsonp} of
        {true, true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.

is_authorized(ReqData, #state{uid = ClientUid, session = Session} = State) ->
    {data, Result} = mysql:execute(pool, session, [Session]),

    case mysql:get_result_rows(Result) of
        [[SessionUid]] ->
            {case ClientUid of
                 SessionUid -> true;
                 _NonMatchingUids -> "Session does not match reported user id."
             end,
             ReqData, State};

        _NoSidMatch ->
            {"Session id entry not found.", ReqData, State}
    end.

content_types_provided(ReqData, State) -> 
        {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, #state{uid = Uid, jsonp = Jsonp} = State) ->
    {data, Result} = mysql:execute(pool, user_sensor, [Uid]),

    Data = [{struct, [{<<"sensor">>, Sensor},
                      {<<"type">>, Type},
                      {<<"function">>, Function}]}
              || [Sensor, Type, Function] <- mysql:get_result_rows(Result)],

    Reply = mochijson2:encode(Data),

    {case Jsonp of
        undefined -> Reply;
        _ -> [Jsonp, "(", Reply, ");"]
     end,
    ReqData, State}.
