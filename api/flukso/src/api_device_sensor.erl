%% @author Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
%% @copyright (C) 2018 Bart Van Der Meerssche
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
%% @doc Flukso API: /device/xyz/sensor endpoint

-module(api_device_sensor).
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

    {Device, ValidDevice} =
        check:device(wrq:path_info(device, ReqData)),

    {Token, ValidToken} =
        check:token(wrq:get_req_header("X-Token", ReqData),
                    wrq:get_qs_value("token", ReqData)),

    {Session, ValidSession} =
        check:session(wrq:get_cookie_value(check:session_name(), ReqData)),

    {Jsonp, ValidJsonp} =
        check:jsonp(wrq:get_qs_value("callback", ReqData)),

    State = #state{device  = Device,
                   token   = Token,
                   session = Session,
                   jsonp   = Jsonp},
    ReqData1 = wrq:set_resp_header("Access-Control-Allow-Origin", "*", ReqData),

    {case {ValidVersion, ValidDevice, ValidToken or ValidSession, ValidJsonp} of
        {true, true, true, true} -> false;
        _ -> true
     end,
    ReqData1, State}.

is_authorized(ReqData, #state{device = Device, token = Token, session = Session} = State) ->
    {data, Result} = mysql:execute(pool, session, [Session]),
    {data, UidResult} = mysql:execute(pool, device_uid, [Device]),
    [[RequestUid]] = mysql:get_result_rows(UidResult),
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
                 [[1]] -> "This user prefers to keep his devices private."
             end,
             ReqData, State};
        {[], [[TokenBin]]} ->
            {true, ReqData, State};
        _NoMatch ->
            {"Access refused", ReqData, State}
    end.

content_types_provided(ReqData, State) ->
        {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, #state{device = Device, jsonp = Jsonp} = State) ->
    {data, Result} = mysql:execute(pool, device_sensor, [Device]),

    Data = [{struct, [{<<"sensor">>, Sensor},
                      {<<"config">>, Config},
                      {<<"port">>,
                          case is_binary(Ports) of
                              true ->
                                  list_to_integer(string:substr(binary_to_list(Ports), 2, 1));
                              false -> undefined
                          end},
                      {<<"type">>, Type},
                      {<<"subtype">>, Subtype},
                      {<<"function">>, Function},
		      {<<"kid">>, Kid},
                      {<<"data_type">>, DataType}]}
              || [Sensor, Config, Ports, Type, Subtype, Function, DataType, Kid]
              <- mysql:get_result_rows(Result)],

    Reply = mochijson2:encode(Data),

    {case Jsonp of
        undefined -> Reply;
        _ -> [Jsonp, "(", Reply, ");"]
     end,
    ReqData, State}.
