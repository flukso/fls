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
%% @doc Flukso API: /sensor/<sid>/tmpo/<rid>/<lvl>/<bid> resource specification 

-module(api_tmpo_block).
-author('Bart Van Der Meerssche <bart@flukso.net>').

-export([
	init/1,
	malformed_request/2,
	is_authorized/2,
	content_types_provided/2,
	to_json/2,
	to_gzip/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {
	sensor,
	rid,
	lvl,
	bid,
	token,
	session}).

init([]) ->
	{ok, undefined}.

malformed_request(ReqData, _State) ->
	{_Version, ValidVersion} = check:version(
		wrq:get_req_header("X-Version", ReqData),
		wrq:get_qs_value("version", ReqData)),
	{Sensor, ValidSensor} =	check:sensor(
		wrq:path_info(sensor, ReqData)),
	{Rid, Lvl, Bid, ValidTmpo} = check:tmpo(
		wrq:path_info(rid, ReqData),
		wrq:path_info(lvl, ReqData),
		wrq:path_info(bid, ReqData)),
	{Token, ValidToken} = check:token(
		wrq:get_req_header("X-Token", ReqData),
		wrq:get_qs_value("token", ReqData)),
	{Session, ValidSession} = check:session(
		wrq:get_cookie_value(check:session_name(), ReqData)),

	State = #state{
		sensor = Sensor,
		rid = Rid,
		lvl = Lvl,
		bid = Bid,
		token = Token,
		session = Session},

	case {ValidVersion, ValidSensor, ValidTmpo, ValidToken or ValidSession} of
		{true, true, true, true} -> {false, ReqData, State};
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

encodings_provided(ReqData, State) ->
	Encodings = [
		{"identity", fun(X) -> X end},
		{"gzip", fun(X) -> X end}],
	{Encodings, ReqData, State}.

content_types_provided(ReqData, State) ->
	Ctypes = [
		{"application/gzip", to_gzip},
		{"application/json", to_json}], % should always be used with gzip encoding!
	{Ctypes, ReqData, State}.

to_json(ReqData, State) ->
	ReqData1 = wrq:set_resp_header("Content-Encoding", "gzip", ReqData),
	to_gzip(ReqData1, State).

to_gzip(ReqData, State = #state{sensor = Sensor, rid = Rid, lvl = Lvl, bid = Bid}) ->
	{data, Result} = mysql:execute(pool, tmpo_block, [Sensor, Rid, Lvl, Bid, "gz"]),
	Block = mysql:get_result_rows(Result),

	case Block of
		[[Data]] ->
			{Data, ReqData, State};
		_ ->
			ReqData1 = wrq:remove_resp_header("Content-Encoding", ReqData),
			{{halt, 404}, ReqData1, State}
	end.

