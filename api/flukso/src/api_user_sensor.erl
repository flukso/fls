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

    {Token, ValidToken} =
        check:token(wrq:get_req_header("X-Token", ReqData),
                    wrq:get_qs_value("token", ReqData)),

    {Session, ValidSession} =
        check:session(wrq:get_cookie_value(check:session_name(), ReqData)),

    {Jsonp, ValidJsonp} =
        check:jsonp(wrq:get_qs_value("callback", ReqData)),

    State = #state{uid     = Uid,
                   token   = Token,
                   session = Session,
                   jsonp   = Jsonp},

    {case {ValidVersion, ValidUid, ValidToken or ValidSession, ValidJsonp} of
        {true, true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.

is_authorized(ReqData, #state{uid = RequestUid, token = Token, session = Session} = State) ->
    {data, Result} = mysql:execute(pool, session, [Session]),
    {data, Master} = mysql:execute(pool, master_token_uid, [RequestUid, Token]),

    TokenBin = case is_list(Token) of
        true -> list_to_binary(Token);
        false -> <<"">>
    end,

    case { mysql:get_result_rows(Result),
           mysql:get_result_rows(Master) } of
        {[[1]], []} ->
            {true, ReqData, State};
        {[[SessionUid]], []} when SessionUid == RequestUid ->
            {true, ReqData, State};
        {[[_SessionUid]], []} ->
            {data, Private} = mysql:execute(pool, user_private, [RequestUid]),

            {case mysql:get_result_rows(Private) of
                 [[0]] -> true;
                 [[1]] -> "This user prefers to keep his sensors private."
             end,
             ReqData, State};
        {[], [[TokenBin]]} ->
            {true, ReqData, State};
        _NoMatch ->
            {"Access refused", ReqData, State}
    end.

content_types_provided(ReqData, State) -> 
        {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, #state{uid = Uid} = State) when Uid == 0 ->
    to_json(ReqData, State#state{uid = ?UID_DEMO});
to_json(ReqData, #state{uid = Uid, jsonp = Jsonp} = State) ->
    {data, Result} = mysql:execute(pool, user_sensor, [Uid]),

    Data = [{struct, [{<<"sensor">>, Sensor},
                      {<<"type">>, Type},
                      {<<"function">>, Function},
                      {<<"ip">>, Ip},
                      {<<"port">>, Port}]}
              || [Sensor, Type, Function, Ip, Port] <- mysql:get_result_rows(Result)],

    Reply = mochijson2:encode(Data),

    {case Jsonp of
        undefined -> Reply;
        _ -> [Jsonp, "(", Reply, ");"]
     end,
    ReqData, State}.
