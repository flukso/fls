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
%% @doc SQL prepared statements used in the REST API

-module(sql).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([prepare/0]).

-define(SQL_WATCHDOG,
    <<"INSERT
       INTO watchdog
           (uid,
            type,
            message,
            variables,
            severity,
            location,
            hostname,
            timestamp
           )
       VALUES
           (?, ?, ?, ?, ?, ?, ?, ?)">>).

-define(SQL_PERMISSIONS,
    <<"SELECT 
           permissions
       FROM
           logger_tokens
       WHERE
           meter = ? AND token = ?">>).

-define(SQL_MASTER_TOKEN,
    <<"SELECT
           token
       FROM
           (logger_meters lm INNER JOIN logger_users lu ON lm.uid = lu.uid)
       WHERE
           meter = ? AND token = ?">>).

-define(SQL_SENSOR_KEY,
    <<"SELECT
           sha
       FROM
           (logger_devices ld INNER JOIN logger_meters lm ON ld.device = lm.device)
       WHERE
           lm.meter = ?">>).

-define(SQL_SENSOR_PROPS,
    <<"SELECT
           uid,
           device,
           night
       FROM
           logger_meters
       WHERE
           meter = ?">>).

-define(SQL_SENSOR_PARAM,
    <<"SELECT
           access,
           type,
           function,
           class,
           voltage,
           current,
           phase,
           constant,
           enabled
       FROM
           logger_meters
       WHERE
           meter = ?">>).

-define(SQL_SENSOR_UPDATE,
    <<"UPDATE
           logger_meters
       SET
           access = ?,
           night = ?,
           value = ?
       WHERE
           meter = ?">>).

-define(SQL_SENSOR_CONFIG,
    <<"UPDATE
           logger_meters
       SET
           class = ?,
           type = ?,
           function = ?,
           voltage = ?,
           current = ?,
           phase = ?,
           constant = ?,
           enabled = ?
       WHERE
           meter = ?">>).

-define(SQL_TIMEZONE,
    <<"SELECT
           timezone
       FROM
           users
       WHERE
           uid = ?">>).

-define(SQL_DEVICE_KEY,
    <<"SELECT
           sha
       FROM
           logger_devices
       WHERE
           device = ?">>).

-define(SQL_DEVICE_PROPS,
    <<"SELECT
           sha,
           upgrade,
           resets
       FROM
           logger_devices
       WHERE
           device = ?">>).

-define(SQL_DEVICE_UPDATE,
    <<"UPDATE
           logger_devices
       SET
           access = ?,
           version = ?,
           upgrade = ?,
           resets = ?,
           uptime = ?,
           memtotal = ?,
           memfree = ?,
           memcached = ?,
           membuffers = ?
       WHERE
           device = ?">>).

-define(SQL_ALARM_SENSOR_LOAD,
    <<"SELECT
           sensor,
           type,
           resolution,
           threshold,
           state,
           timestamp
       FROM
           alarm_sensor
       WHERE
           sensor = ?">>).

                   
% TODO: use a binary:replace to clean up \n's in the query strings
prepare() ->
    mysql:prepare(watchdog, ?SQL_WATCHDOG),
    mysql:prepare(permissions, ?SQL_PERMISSIONS),
    mysql:prepare(master_token, ?SQL_MASTER_TOKEN),
    mysql:prepare(sensor_key, ?SQL_SENSOR_KEY),
    mysql:prepare(sensor_props, ?SQL_SENSOR_PROPS),
    mysql:prepare(sensor_param, ?SQL_SENSOR_PARAM),
    mysql:prepare(sensor_update, ?SQL_SENSOR_UPDATE),
    mysql:prepare(sensor_config, ?SQL_SENSOR_CONFIG),
    mysql:prepare(timezone, ?SQL_TIMEZONE),
    mysql:prepare(device_key, ?SQL_DEVICE_KEY),
    mysql:prepare(device_props, ?SQL_DEVICE_PROPS),
    mysql:prepare(device_update, ?SQL_DEVICE_UPDATE),
    mysql:prepare(alarm_sensor_load, ?SQL_ALARM_SENSOR_LOAD).
