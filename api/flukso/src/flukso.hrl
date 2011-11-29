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
%% @doc Common definitions


% fs paths
-define(SYSLOG_PATH,   "var/data/syslog/").
-define(RRD_BASE_PATH,   "var/data/base/").
-define(RRD_NIGHT_PATH, "var/data/night/").

% webmachine state records
-record(state,
    {'sensor',
     'start',
     'end',
     'resolution',
     'factor',
     'token',
     'device',
     'digest',
     'jsonp',
     'param',
     'return',
     'uid',
     'session'}).

% interval definitions
-define(MINUTE,     60).
-define(QUARTER,   900).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(NIGHT,  -86400).
-define(WEEK,   604800).
-define(MONTH, 2419200).
-define(YEAR, 31536000).

% unit conversion factors from rrd to ...
-define(FACTOR_WATT,         3600).     % Wh/s -> Watt
-define(FACTOR_LPERDAY, 24 * 3600).     % L/s -> L/day

% alarm/logging severity levels
-define(EMERGENCY, 0).
-define(ALERT,     1).
-define(CRITICAL,  2).
-define(ERROR,     3).
-define(WARNING,   4).
-define(NOTICE,    5).
-define(INFO,      6).
-define(DEBUG,     7).

% logging severity threshold
-define(LOGLEVEL, 7).
