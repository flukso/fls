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
%% @doc Flukso API: /sensor/xyz resource specification 

-module(api_sensor).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         content_types_provided/2,
         timeseries_to_csv/2,
         timeseries_to_json/2,
         param_to_json/2,
         update_to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("flukso.hrl").


init([]) -> 
    {ok, undefined}.

% debugging
%init(Config) ->
%   {{trace, "/tmp"}, Config}.

allowed_methods(ReqData, State) ->
    {['POST', 'GET'], ReqData, State}.

malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> malformed_POST(ReqData, State);
        'GET'  -> malformed_GET(ReqData, State)
    end.

malformed_POST(ReqData, _State) ->
    {_Version, ValidVersion} =
        check:version(wrq:get_req_header("X-Version", ReqData)),

    {Sensor, ValidSensor} =
        check:sensor(wrq:path_info(sensor, ReqData)),

    {Digest, ValidDigest} =
        check:digest(wrq:get_req_header("X-Digest", ReqData)),

    State = #state{sensor = Sensor,
                   digest = Digest},

    {case {ValidVersion, ValidSensor, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.

malformed_GET(ReqData, _State) ->
    {_Version, ValidVersion} =
        check:version(wrq:get_req_header("X-Version", ReqData),
                      wrq:get_qs_value("version", ReqData)),

    {Sensor, ValidSensor} =
        check:sensor(wrq:path_info(sensor, ReqData)),

    {Start, End, Resolution, ValidTime} =
        check:times(wrq:get_qs_value("interval", ReqData),
                    wrq:get_qs_value("start", ReqData),
                    wrq:get_qs_value("end", ReqData),
                    wrq:get_qs_value("resolution", ReqData)),

    {Factor, ValidUnit} =
        check:unit(wrq:get_qs_value("unit", ReqData)),

    {Token, ValidToken} =
        check:token(wrq:get_req_header("X-Token", ReqData),
                    wrq:get_qs_value("token", ReqData)),

    {Session, ValidSession} =
        check:session(wrq:get_cookie_value(check:session_name(), ReqData)),

    {Jsonp, ValidJsonp} =
        check:jsonp(wrq:get_qs_value("callback", ReqData)),

    {Param, ValidParam} =
        check:param(wrq:get_qs_value("param", ReqData)),

    {_Update, ValidUpdate} =
        check:update(wrq:get_qs_value("update", ReqData)),

    State = #state{sensor     = Sensor, 
                   start      = Start,
                   'end'      = End,
                   resolution = Resolution,
                   factor     = Factor,
                   token      = Token,
                   session    = Session,
                   jsonp      = Jsonp,
                   param      = Param},

    case {ValidVersion, ValidSensor, ValidToken or ValidSession, ValidTime,
                        ValidUnit, ValidJsonp, ValidParam, ValidUpdate}  of
        {true, true, true,  true,  true, true, false, false} ->  % GET timeseries
            EndState = State#state{return = timeseries},
            {false, ReqData, EndState};
        {true, true, true, false, false,    _,  true, false} ->  % GET param
            EndState = State#state{return = param},
            {false, ReqData, EndState};
        {true, true, true, false, false,    _, false,  true} ->  % 'GET' update
            EndState = State#state{return = update},
            {false, ReqData, EndState};
	    _ ->
            {true, ReqData, State}
    end. 

is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> is_auth_POST(ReqData, State);
        'GET'  -> is_auth_GET(ReqData, State)
    end.

is_auth_POST(ReqData, #state{sensor = Sensor, digest = ClientDigest} = State) ->
    {data, Result} = mysql:execute(pool, sensor_key, [Sensor]),

    case mysql:get_result_rows(Result) of
        [[Key]] ->
            Data = wrq:req_body(ReqData),
            <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, Data),
            ServerDigest = lists:flatten(io_lib:format("~40.16.0b", [X])),

            {case ServerDigest of
                 ClientDigest -> true;
                 _WrongDigest -> "Incorrect digest"
             end,
             ReqData, State};

        _NoKey ->
            {"No proper provisioning for this sensor", ReqData, State}
    end.

is_auth_GET(ReqData, #state{sensor = Sensor, token = Token, session = Session} = State) ->
    {data, Permission} = mysql:execute(pool, permissions, [Sensor, Token]),
    {data, Master} = mysql:execute(pool, master_token, [Sensor, Token]),
    {data, Suid} = mysql:execute(pool, session, [Session]),

    {case { mysql:get_result_rows(Permission),
            mysql:get_result_rows(Master),
            mysql:get_result_rows(Suid) } of
        {[[62]], [], []} -> true;
        {[], [[_Token]], []} -> true;
        {[], [], [[_Uid]]} -> true; 
        {[], [], []} -> "Access refused" 
    end,
    ReqData, State}.

content_types_provided(ReqData, #state{return = Return} = State) -> 
        Csv = {"text/csv",
            case Return of
                timeseries -> timeseries_to_csv;
                _ -> dummy_callback
            end},

        Json = {"application/json",
           case Return of
               timeseries -> timeseries_to_json;
               param -> param_to_json;
               update -> update_to_json;
               _ -> dummy_callback  % for POST
           end},

        {[Json, Csv], ReqData, State}.

timeseries_to_csv(ReqData, #state{sensor     = Sensor,
                                  start      = Start,
                                  'end'      = End,
                                  resolution = Resolution,
                                  factor     = Factor} = State) ->
    case wrq:get_qs_value("interval", ReqData) of
        "night"   -> Rrd = night;
        _Interval -> Rrd = base
    end,

    case rrd:fetch(Rrd, Sensor, Start, End, Resolution, Factor) of
        {ok, Datapoints, Nans} ->
            Datapoints2 = [[X, integer_to_list(Y)] || [X, Y] <- Datapoints],
            Merge = lists:merge(Datapoints2, Nans),
            Reply = [[integer_to_list(X), ",", Y, "\r\n"] || [X, Y] <- Merge],
            {Reply, ReqData, State};

        {error, _Reason} ->
            {{halt, 404}, ReqData, State}
    end.

timeseries_to_json(ReqData, #state{sensor     = Sensor,
                                   start      = Start,
                                   'end'      = End,
                                   resolution = Resolution,
                                   factor     = Factor,
                                   jsonp      = Jsonp} = State) -> 
    case wrq:get_qs_value("interval", ReqData) of
        "night"   -> Rrd = night;
        _Interval -> Rrd = base
    end,

    case rrd:fetch(Rrd, Sensor, Start, End, Resolution, Factor) of
        {ok, Datapoints, Nans} ->
            Reply = mochijson2:encode(lists:merge(Datapoints, Nans)),

            {case Jsonp of
                undefined -> Reply;
                _ -> [Jsonp, "(", Reply, ");"]
             end,
            ReqData, State};

        {error, _Reason} ->
            {{halt, 404}, ReqData, State}
    end.

param_to_json(ReqData, #state{sensor = Sensor} = State) ->
    LastUpdate = rrd:lastupdate(Sensor),

    {data, Result} = mysql:execute(pool, sensor_param, [Sensor]),
    [Params] = mysql:get_result_rows(Result),
    FilteredParams = lists:map(fun undefined_to_null/1, Params),

    FieldInfo = mysql:get_result_field_info(Result),
    Fields = [Field || {_Table, Field, _Length, _Type} <- FieldInfo],
    Objects = lists:append(lists:zip(Fields, FilteredParams),
                           [{<<"lastupdate">>, LastUpdate}]),

    JsonResponse = mochijson2:encode({struct, Objects}),
    {JsonResponse, ReqData, State}.

update_to_json(ReqData, #state{sensor = Sensor} = State) ->
    event:sensor_update(Sensor),
    {"\"sensor update event triggered\"", ReqData, State}.

process_post(ReqData, State) ->
    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),

    Payload = {proplists:get_value(<<"measurements">>, JsonData),
               proplists:get_value(<<"config">>, JsonData)},

    case Payload of
        {undefined, undefined} ->
            {false, ReqData, State};
        {Measurements, undefined} ->
            process_measurements(Measurements, ReqData, State);
        {undefined, Config} ->
            process_config(Config, ReqData, State);
        {_Measurements, _Config} ->
            {false, ReqData, State}
    end.

% JSON: {"config":{"type":"electricity","enable":0,"class":"analog","current":50,"voltage":230}}
% Mochijson2: {struct,[{<<"config">>, {struct,[{<<"type">>,<<"electricity">>}, {<<"enable">>,0}, ... ]} }]}
process_config({struct, Params}, ReqData, #state{sensor = Sensor} = State) ->
    Args = [proplists:get_value(<<"class">>,    Params),
            proplists:get_value(<<"type">>,     Params),
            proplists:get_value(<<"function">>, Params),
            proplists:get_value(<<"voltage">>,  Params),
            proplists:get_value(<<"current">>,  Params),
            proplists:get_value(<<"phase">>,    Params),
            proplists:get_value(<<"constant">>, Params),
            proplists:get_value(<<"enable">>,   Params),
            Sensor],

    {updated, _Result} = mysql:execute(pool, sensor_config, Args),
    rrd_provision(Sensor, proplists:get_value(<<"enable">>, Params)),

    {true, ReqData, State}.

rrd_provision(_Sensor, ?DISABLE) ->
    {ok, noop};
rrd_provision(Sensor, ?ENABLE) ->
    rrd:create(Sensor).

% JSON: {"measurements":[[<TS1>,<VALUE1>],...,[<TSn>,<VALUEn>]]}
% Mochijson2: {struct,[{<<"measurements">>,[[<TS1>,<VALUE1>],...,[<TSn>,<VALUEn>]]}]}
process_measurements(Measurements, ReqData, #state{sensor = Sensor} = State) ->
    Data = [[integer_to_list(Time), ":", integer_to_list(Counter), " "]
        || [Time, Counter] <- Measurements, is_integer(Counter)],

    [LastTimestamp, LastValue] = lists:last(Measurements),

    {data, Result} = mysql:execute(pool, sensor_props, [Sensor]),
    [[Uid, _Device, Midnight]] = mysql:get_result_rows(Result),

    case rrd:update(Sensor, Data) of
        {ok, _Response} ->
            Response = "ok",

            NewMidnight =
                update_night(Sensor, Uid, Midnight, LastTimestamp, ReqData),

            mysql:execute(pool, sensor_update,
                [check:unix(), NewMidnight, LastValue, Sensor]),

            event:sensor_update(Sensor);

        {error, Response} ->
            flog:drupal(Uid, <<"rrdupdate.base">>, list_to_binary(Response), ?ERROR, ReqData)
    end,

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(Response)}]}),
    {true , wrq:set_resp_body(JsonResponse, ReqData), State}.

update_night(Sensor, Uid, Midnight, LastTimestamp, ReqData)
                  when LastTimestamp > Midnight + 6 * ?HOUR ->

    LastMidnight = calculate_midnight(check:unix(), Uid),
    Start = integer_to_list(LastMidnight + 2 * ?HOUR),
    End = integer_to_list(LastMidnight + 5 * ?HOUR),
    Resolution = integer_to_list(?QUARTER),

    case rrd:fetch(Sensor, Start, End, Resolution, raw) of
        {ok, Datapoints, _Nans} ->
            Values = [Y || [_X, Y] <- Datapoints],

            NightAverage =
                lists:foldl(fun(X, Sum) -> X / 12 + Sum end, 0.0, Values),

            Data =
                [integer_to_list(LastMidnight + 5 * ?HOUR), ":", float_to_list(NightAverage)],

            case rrd:update(night, Sensor, Data) of
                {ok, _Response} ->
                     flog:drupal(Uid, <<"rrdupdate.night">>, <<"Successful update of night rrd.">>, ?INFO, ReqData);

                {error, Reason} ->
                     flog:drupal(Uid, <<"rrdupdate.night">>, list_to_binary(Reason), ?ERROR, ReqData)
            end;

        {error, Reason} ->
            flog:drupal(Uid, <<"rrdupdate.night">>, list_to_binary(Reason), ?ERROR, ReqData)
    end,

    LastMidnight + ?DAY;
update_night(_Sensor, _Uid, Midnight, _LastTimestamp, _ReqData) ->
    Midnight.

calculate_midnight(Timestamp, Uid) ->
    {data, Result} = mysql:execute(pool, timezone, [Uid]),

    case mysql:get_result_rows(Result) of
       [[undefined]] ->
           Timezone = 0;
       [[TimezoneChar8]] ->
           Timezone = list_to_integer(binary_to_list(TimezoneChar8))
    end,

    closest_midnight(trunc(Timestamp/?DAY + 1) * ?DAY - Timezone, Timestamp).

closest_midnight(ProposedMidnight, Timestamp) when ProposedMidnight > Timestamp ->
    closest_midnight(ProposedMidnight - ?DAY, Timestamp);
closest_midnight(ProposedMidnight, _Timestamp) ->
    ProposedMidnight.

undefined_to_null(undefined) ->
    null;
undefined_to_null(Field) ->
    Field.
