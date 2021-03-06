<?php

//
// xmlrpc.inc : data and management plane xmlrpc methods for logging metering data
//              alpha version API
//
// Copyright (c) 2008-2009 jokamajo.org
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
//
// $Id$
//

/**
 * Implementation of hook_xmlrpc().
 * Mapping external XML-RPC methods to callback functions.
 * API versioning; logger.flukso.net/xmlrpc.php maps to xmlrpc.inc
 */
function logger_xmlrpc() {
  return array(
    array(
      'logger.heartbeat', // External method name.
      '_logger_heartbeat', // Drupal callback function to run.
      array('int', 'string', 'int', 'int', 'int'), // Return value's type, then any parameter types (upgrade, device, version, resets, uptime)
      'Send a heartbeat to the logger.' // Description.
      ),
    array(
      'logger.measurementAdd', // External method name.
      '_logger_measurement_add', // Drupal callback function to run.
      array('string', 'array'), // Return value's type, then any parameter types (return, measurements)
      'Submit measurements to the logger.' // Description.
      ),
  );
}

/**
 * Callback functions registered in the logger_xmlrpc section
 */
function _logger_heartbeat($device, $version, $resets, $uptime) {
  $dev = db_fetch_object(db_query("SELECT upgrade, resets FROM {logger_devices} WHERE device = '%s'", $device));
  $dev->resets += $resets;
  db_query("UPDATE {logger_devices} SET access = %d, version = %d, upgrade = %d, resets = %d, uptime = %d WHERE device = '%s'", time(), $version, 0, $dev->resets, $uptime, $device);
  return $dev->upgrade;
}

function _logger_measurement_add($logs) {
  $info = 'added 5min interval measurements to the log';
  $path = new stdClass();
  $path->root = XMLRPC_PATH . '/' . XMLRPC_MODULE; // need to hardcode drupal_get_path('module', 'logger');
  $path->base = $path->root .'/data/base/';
  $path->night = $path->root .'/data/night/';
  foreach ($logs as $meter => $measurements) {
    //load the normalisation factor, relative to 1pulse = 1Wh
    $meterdata = db_fetch_object(db_query("SELECT uid, night, factor FROM {logger_meters} WHERE meter = '%s'", $meter));
    if ($meterdata->uid < 5) { // only alpha users are allowed to call this API
      $command = $path->root .'/rrdtool update '. $path->base . $meter .'.rrd ';
      ksort($measurements); // sort the key-value pairs in the associative array by key, i.e. the timestamp
      foreach ($measurements as $timestamp => $value) {
        if (is_numeric($timestamp) and is_numeric($value)) {
          $command .= $timestamp .':'. $value*$meterdata->factor .' ';
        }
        else {
          watchdog_xmlrpc('logger.measurementAdd', 'corrupted input data for %meter : %timestamp : %value', array('%meter' => $meter, '%timestamp' => $timestamp, '%value' => $value), WATCHDOG_ERROR);
        }
      }
      system($command, $return);
      if ($return == 0) {
        // update the night rrd every day at 6AM local time
        if (time() > $meterdata->night) {
          $midnight = _logger_midnight(time(), $auth['device']);
          $start = $midnight + 7200; // 2AM local time
          $end = $start + 10800; // 3h time interval
          $command = $path->root ."/rrdtool fetch ". $path->base . $meter .".rrd AVERAGE -r 900 -s ". $start ." -e ". $end ." | tail -n 12 | awk -F': ' '{SUM += $2} END {print SUM/12}'";
          $night = (float)shell_exec($command); //test shell_exec iso system
          $command = $path->root .'/rrdtool update '. $path->night . $meter .'.rrd '. $end .':'. $night;
          system($command, $return);
          if ($return == 0) {
            watchdog_xmlrpc('logger.measurementAdd', 'successful update for night rrd: %command | midnight = %midnight', array('%command' => $command, '%midnight' => $midnight), WATCHDOG_NOTICE); //debugging
          }
          else {
            watchdog_xmlrpc('logger.measurementAdd', 'error updating night rrd: %command', array('%command' => $command), WATCHDOG_ERROR); //debugging
          }
          $meterdata->night = $midnight + 108000; //add an offset of 30h = 6AM local time
        }
        // {logger_meters} is updated with the true metervalue $value, NOT $value*$meterdata->factor since we're not normalising this entry!
        db_query("UPDATE {logger_meters} SET access = %d, night = %d, value = %d WHERE meter = '%s'", time(), $meterdata->night, $value, $meter);

        _logger_sensor_update($meter);
      }
      else {
        watchdog_xmlrpc('logger.measurementAdd', 'shell command execution failed: %return %command', array('%command' => $command, '%return' => $return), WATCHDOG_ERROR);
      }
    }
  }
  return $command; //using $command for testing purposes, replace by $info afterwards
}

/**
 * Calculate the most recent midnight for this device based on the user's timezone
 */
function _logger_midnight($timestamp, $device) {
  $timezone = db_result(db_query("SELECT u.timezone FROM {logger_devices} ld INNER JOIN {users} u ON ld.uid = u.uid WHERE ld.device = '%s'", $device));
  // add one day to make sure $midnight > $timestamp
  $midnight = floor($timestamp/86400 + 1)*86400 - $timezone;
  // search for first $midnight < $timestamp
  while ($midnight > $timestamp) $midnight -= 86400;
  return $midnight;
}

/**
 * Trigger a sensor update event in webmachine
 */
function _logger_sensor_update($sensor) {
  $host = "localhost:9999";
  $token = db_result(db_query("SELECT token FROM {logger_tokens} WHERE meter = '%s'", $sensor));

  $ch = curl_init("http://$host/sensor/$sensor?version=1.0&token=$token&update=true");
  curl_exec($ch);
}
