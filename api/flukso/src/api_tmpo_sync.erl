%% @author Bart Van Der Meerssche <bart@flukso.net>
%% @copyright (C) 2014 Bart Van Der Meerssche
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
%% @doc Flukso API: /sensor/<sid>/tmpo/sync resource specification 

-module(api_tmpo_sync).
-author('Bart Van Der Meerssche <bart@flukso.net>').

-export([
	init/1,
	malformed_request/2,
	is_authorized/2,
	content_types_provided/2,
	to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(TMPO_BLOCK8_SPAN, 256). % = 2^8
-define(TMPO_BLOCK12_SPAN, 4096). % = 2^12
-define(TMPO_BLOCK16_SPAN, 65536). % = 2^16
-define(TMPO_BLOCK20_SPAN, 1048576). % = 2^20

-record(state, {
	sensor,
	rid,
	lvl,
	bid,
	token,
	jsonp,
	session,
	iter = 0}).

init([]) ->
	{ok, undefined}.

malformed_request(ReqData, _State) ->
	{_Version, ValidVersion} = check:version(
		wrq:get_req_header("X-Version", ReqData),
		wrq:get_qs_value("version", ReqData)),
	{Sensor, ValidSensor} =	check:sensor(
		wrq:path_info(sensor, ReqData)),
	{Rid, Lvl, Bid, ValidTmpo} = check:tmpo(
		wrq:get_qs_value("rid", ReqData),
		wrq:get_qs_value("lvl", ReqData),
		wrq:get_qs_value("bid", ReqData)),
	{Token, ValidToken} = check:token(
		wrq:get_req_header("X-Token", ReqData),
		wrq:get_qs_value("token", ReqData)),
	{Session, ValidSession} = check:session(
		wrq:get_cookie_value(check:session_name(), ReqData)),
	{Jsonp, ValidJsonp} =
        check:jsonp(wrq:get_qs_value("callback", ReqData)),

	State = #state{
		sensor = Sensor,
		rid = Rid,
		lvl = Lvl,
		bid = Bid,
		token = Token,
		session = Session,
		jsonp = Jsonp},

	case {ValidVersion, ValidSensor, ValidTmpo, ValidToken or ValidSession, ValidJsonp} of
		{true, true, true, true, true} -> {false, ReqData, State};
		_ -> {true, ReqData, State}
	end.

is_authorized(ReqData, State = #state{sensor = Sensor, token = Token, session = Session}) ->
	{data, Permission} = mysql:execute(pool, permissions, [Sensor, Token]),
	{data, Master} = mysql:execute(pool, master_token, [Sensor, Token]),
	{data, Suid} = mysql:execute(pool, session, [Session]),

	{case {mysql:get_result_rows(Permission), mysql:get_result_rows(Master),
		mysql:get_result_rows(Suid)} of
		{[[62]], [], []} -> true;
		{[], [[_Token]], []} -> true;
		{[], [], [[_Uid]]} -> true; 
		{[], [], []} -> "Access refused" 
	end, ReqData, State}.

content_types_provided(ReqData, State) ->
	Ctypes = [{"application/json", to_json}],
	{Ctypes, ReqData, State}.

to_json(ReqData, State = #state{iter = 4}) ->
	{{halt, 500}, ReqData, State};
to_json(ReqData, State = #state{
	sensor = Sensor, rid = Rid, lvl = Lvl, bid = Bid, jsonp = Jsonp, iter = Iter}) ->
	Bid20 = Bid - (Bid rem ?TMPO_BLOCK20_SPAN),
	{data, Result} = mysql:execute(pool, tmpo_sync, [Sensor, Rid, Bid20]),
	Blocks = mysql:get_result_rows(Result),
	case monotonous(Blocks) of
		false ->
			timer:sleep(100), %ms
			to_json(ReqData, State#state{iter = Iter + 1});
		true ->
			Tail = tail(Lvl, Bid),
			FilteredBlocks = [{struct, [
				{<<"rid">>, RidB},
				{<<"lvl">>, LvlB},
				{<<"bid">>, BidB},
				{<<"ext">>, ExtB}]} || [RidB, LvlB, BidB, ExtB]
				<- Blocks, BidB + trunc(math:pow(2, LvlB)) - 1 > Tail],
			Reply = mochijson2:encode(FilteredBlocks),
			{case Jsonp of
				undefined -> Reply;
				_ -> [Jsonp, "(", Reply, ");"]
			end, ReqData, State}
	end.

monotonous(Blocks) ->
	monotonous(Blocks, 0, 0).

monotonous([], _Srid, _Stail) ->
	true;
monotonous([[Rid, Lvl, Bid, _Ext] | Blocks], Srid, Stail) when Rid > Srid ->
    Btail = tail(Lvl, Bid),
    monotonous(Blocks, Rid, Btail);
monotonous([[Rid, Lvl, Bid, _Ext] | Blocks], Srid, Stail) when Rid =:= Srid ->
	Btail = tail(Lvl, Bid),
	case Btail > Stail of
		true -> monotonous(Blocks, Srid, Btail);
		false -> false
	end.

tail(Lvl, Bid) ->
	Bid + trunc(math:pow(2, Lvl)) - 1.
