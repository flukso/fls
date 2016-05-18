<?php
//
// logger.drush.inc : drush support for the logger module
// Copyright (c) 2008-2009 jokamajo.org
//               2010 flukso.net
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
 * Implementation of hook_drush_command().
 */
function logger_drush_command() {
  $items = array();

  $items['logger-load-nodes'] = array(
    'callback'    => '_logger_load_nodes',
    'description' => 'Load nodes from a csv file into the database.',
    'arguments'   => array(
      'file'      => 'Path of the csv file.',
    ),   
  );

  $items['logger create node'] = array(
    'callback'    => '_logger_create_node',
    'description' => 'Create a new sensor node entry.',
    'arguments'   => array(
      'serial'      => 'Sensor node serial number.',
      'country'     => 'Destination country.',
      'uid'         => 'User ID.',
    ),  
    'options'     => array(
      '--pipe' => 'Returns a space delimited list of created entries.',
    ),
  );

  $items['logger-assign-node'] = array(
    'callback'    => '_logger_assign_node',
    'description' => 'Assign a sensor node to a user.',
    'arguments'   => array(
      'serial'      => 'Sensor node serial number.',
      'uid'         => 'User ID.',
    ),
  );

  $items['logger config meter'] = array(
    'callback'    => '_logger_config_meter',
    'description' => 'Configure a specific meter.',
    'arguments'   => array(
      'meter'       => 'Meter ID.',
      'type'        => 'Meter type.',
      'function'    => 'Meter function.',
      'phase'       => 'Meter phase. Only applicable in case of type = electricity.',
      'constant'    => 'Meter constant. Only applicable in case of type = electricity.',
      'unit'        => 'Meter unit.',
    ),
    'options'     => array(
      '--pipe' => 'Returns ok.',
    ),
  );

  $items['logger token'] = array(
    'callback'    => '_logger_token',
    'description' => 'Create tokens for meter/sensorIDs.',
    'arguments'   => array(
      'meter'       => 'Create a token for one specific meterID.',
      'permissions' => 'Set non-default permissions for this token.',
    ),
  );

  $items['logger-master-tokens'] = array(
    'callback'    => '_logger_master_token',
    'description' => 'Create master tokens for NULL entries in logger_users',
  );

  return $items;
}

/**
 * Drush command callback for logger-load-nodes.
 *
 * @param $file      The absolute path of the to-be-loaded csv file containing following columns:
 *                   serial;device;key;sensor_hw;sensor1;sensor2;sensor3;sensor4;sensor5;sensor6
 * @see logger_drush_command
 */
function _logger_load_nodes($file) {
  $fh = fopen($file, "r");

  /* discard the first line containing column names */
  fgets($fh);

  while ($line = fgets($fh)) {
    $row = explode(";", $line);
    $row = str_replace("\n", "", $row);

    $created = time();

    _logger_load_device($row, $created);
    _logger_load_sensors($row, $created);
  }

  if (!drush_get_error()) {
    $message = 'Successfully created all device/sensor/token/rrd entries.';
    drush_log(dt($message, array(), 'ok'));
  }
}

/**
 * Loads a device entry into the {logger_devices} database table.
 *
 * @param $row       An array containing a single row from the csv file.
 * @param $created   The entry ceation time.
 * @see _logger_load_nodes
 */
function _logger_load_device($row, $created) {
  $serial    = (int)substr($row[0], 2, 9);  /* FL01234567 */
  $device    = $row[1];
  $key       = $row[2];
  $sensor_hw = (int)$row[3];

  $sql = "INSERT INTO {logger_devices}
            (serial, device, sha, created, sensor)
          VALUES
            (%d, '%s', '%s', %d, %d)";

  $result = db_query($sql, $serial, $device, $key, $created, $sensor_hw); 

  if (!$result) {
    $error = 'An error occured when creating a device entry for @device.';
    drush_set_error('LOGGER_INSERT_DEVICE', dt($error, array('@device' => $device)));
  }
}

/**
 * Loads all sensors of a single device into the {logger_meters} database table and
 * loads a token for each sensor in {logger_tokens}.
 *
 * @param $row       An array containing a single row from the csv file.
 * @param $created   The entry ceation time.
 * @see _logger_load_nodes
 */
function _logger_load_sensors($row, $created) {
  $permissions = 62;
  $device = $row[1];
  $sensor_hw = (int)$row[3];

  if ($sensor_hw == 21) {
    $max_i = 6;
  }
  elseif ($sensor_hw == 22) {
    $max_i = 64;
  }
  elseif ($sensor_hw == 32 || $sensor_hw = 35) {
    $max_i = 128;
  }

  for ($i = 1; $i <= $max_i ; $i++) {
    $sensor = $row[$i + 3];

    _logger_load_sensor($i, $sensor, $device, $created);

    $token  = md5(uniqid(rand(), TRUE));
    _logger_load_token($token, $sensor, $permissions);
  }
}

/**
 * Loads a single sensor entry into the {logger_meters} database table.
 *
 * @param $serial    The device serial number.
 * @param $sensor    The 32-hex sensor id.
 * @param $device    the 32-hex device id.
 * @param $created   The entry ceation time.
 * @see _logger_load_sensors
 */
function _logger_load_sensor($serial, $sensor, $device, $created) {
  $sql = "INSERT INTO {logger_meters}
            (serial, meter, device, created)
          VALUES
            (%d, '%s', '%s', %d)";

  $result = db_query($sql, $serial, $sensor, $device, $created);

  if (!$result) {
    $error = 'An error occured when creating an entry for sensor @sensor.';
    drush_set_error('LOGGER_INSERT_SENSOR', dt($error, array('@sensor' => $sensor)));
  }
}

/**
 * Loads a single token entry into the {logger_tokens} database table.
 *
 * @param $token       The 32-hex token.
 * @param $sensor      The 32-hex sensor id.
 * @param $permissions A permission bitmap, defaulting to 62.
 * @see _logger_load_sensors
 */
function _logger_load_token($token, $sensor, $permissions) {
  $sql = "INSERT INTO {logger_tokens}
            (token, meter, permissions)
          VALUES
            ('%s', '%s', %d)";

  $token  = md5(uniqid(rand(), TRUE));

  $result = db_query($sql, $token, $sensor, $permissions);

  if (!$result) {
    $error = 'An error occured when creating a token entry for sensor @sensor.';
    drush_set_error('LOGGER_INSERT_TOKEN', dt($error, array('@sensor' => $sensor)));
  }
}

function _logger_create_node($serial, $country ="", $uid = 0) {
  // guard against duplicating entries for the same S/N
  $count = db_result(db_query("SELECT COUNT(device) FROM {logger_devices} WHERE serial = %d", $serial));
  if ($count > 0) {
    drush_set_error('LOGGER_CREATE_SERIAL_DUPLICATE', dt('The S/N: @serial already exists.', array('@serial' => $serial)));
  }
  else { 
    if ($uid > 0) {
      $result = db_query("INSERT INTO {logger_users} (uid, private) VALUES (%d, %d)", $uid, 0);
      if (!$result) drush_set_error('LOGGER_CREATE_USERS_ENTRY', dt('Error creating a user entry for @uid.', array('@uid' => $uid)));
    }

    // create an entry in the {logger_devices} table
    $device  = md5(uniqid(rand(), TRUE));
    $sha     = md5(uniqid(rand(), TRUE));
    $created = time();

    $result = db_query("INSERT INTO {logger_devices} (device, serial, uid, sha, created, country) VALUES ('%s', %d, %d, '%s', %d, '%s')", $device, $serial, $uid, $sha, $created, $country);
    if (!$result) drush_set_error('LOGGER_CREATE_DEVICE_ENTRY', dt('Error creating a device entry for @device.', array('@device' => $device)));

    if (!drush_get_error()) {
      drush_log(dt('Successfully created the device/key: @device / @key for S/N: @serial', array('@device' => $device, '@key' => $sha, '@serial' => $serial)), 'ok');
      $pipe[] .= 'DEVICE='.$device;
      $pipe[] .= 'KEY='.$sha;
    }

    // create an entry in the {logger_meters} table
    for ($i = 0; $i < 4; $i++) {
      $permissions = 62;
      $meter = md5(uniqid(rand(), TRUE));
      $token = md5(uniqid(rand(), TRUE));

      $path = new stdClass();
        $path->root = DRUPAL_ROOT .'/'. drupal_get_path('module', 'logger');
        $path->base = $path->root .'/data/base/';
        $path->night = $path->root .'/data/night/';

      $result = db_query("INSERT INTO {logger_meters} (meter, uid, device, created) VALUES ('%s', %d, '%s', %d)", $meter, $uid, $device, $created); 
      $insert = db_query("INSERT INTO {logger_tokens} (token, meter, permissions) VALUES ('%s', '%s', %d)", $token, $meter, $permissions);

      if (!($result && $insert)) drush_set_error('LOGGER_CREATE_METER_ENTRY', dt('Error creating meter entry for @meter.', array('@meter' => $meter)));

      // rrd's of sensors (a.k.a. meters) are now generated on-the-fly when enabling the sensor

      if (!drush_get_error()) {
	drush_log(dt('Successfully created the meter: @meter with token: @token', array('@meter' => $meter, '@token' => $token)), 'ok');
        $pipe[] .= 'SENSOR'.$i.'='.$meter;
      }
    }

    // Space delimited list for use by other scripts. Set the --pipe option.
    drush_print_pipe(implode(' ', $pipe));

  }
}

function _logger_assign_node($serial, $uid) {
  // check the existence of S/N
  $device = db_result(db_query("SELECT device FROM {logger_devices} WHERE serial = %d", $serial));
  if ($device == '') {
    drush_set_error('LOGGER_ASSIGN_SERIAL_NON_EXISTENT', dt('The S/N: @serial does not exist.', array('@serial' => $serial)));
  }
  else {
    db_query("UPDATE {logger_devices} SET uid = %d WHERE serial = %d", $uid, $serial);   
    db_query("UPDATE {logger_meters} SET uid = %d WHERE device = '%s'", $uid, $device);
    
    $result = db_query("INSERT INTO {logger_users} (uid, private) VALUES (%d, %d)", $uid, 0);
    if (!$result) drush_log(dt('uid: @uid already exists in the {logger_users} table', array('@uid' => $uid)), 'notice');
  }

  if (!drush_get_error()) drush_log(dt('Successfully assigned uid: @uid to S/N: @serial', array('@uid' => $uid, '@serial' => $serial)), 'ok');

}

function _logger_config_meter($meter, $type, $function, $phase, $constant, $unit) {
  $result = db_query("UPDATE {logger_meters} SET type = '%s', function = '%s', phase = %d, constant = %d, unit = '%s' WHERE meter = '%s'", $type, $function, $phase, $constant, $unit, $meter);
  drush_log(dt('Successfully updated meter: @meter', array('@meter' => $result)), 'ok');  
  drush_print_pipe('ok');
}

function _logger_tokens($meter = "", $permissions = 62) {
  if ($meter == "") {
    $result = db_query("SELECT meter FROM {logger_meters}");
    while ($meter = db_fetch_object($result)) {
      $count = db_result(db_query("SELECT COUNT(meter) FROM {logger_tokens} WHERE meter = '%s'", $meter->meter));
      if ($count == 0) {
        $token = md5(uniqid(rand(), TRUE));
        $insert = db_query("INSERT INTO {logger_tokens} (token, meter, permissions) VALUES ('%s', '%s', %d)", $token, $meter->meter, $permissions);

        if (!$insert) {
          drush_set_error('LOGGER_CREATE_TOKEN_ENTRY', dt('Error creating token entry for @meter.', array('@meter' => $meter->meter)));
        }
        else {
          drush_log(dt('Created an entry in {logger_tokens} with token: @token and meter: @meter', array('@token' => $token , '@meter' => $meter->meter)), 'ok');
        }
      }
    }
  }
}

/**
 * Populates a master token for each entry in the {logger_users} table
 * for which no token is available yet.
 *
 * @see logger_drush_command
 */
function _logger_master_token() {
  $result = db_query("SELECT uid FROM {logger_users} WHERE token IS NULL");

  while ($usr = db_fetch_object($result)) {
    $token = md5(uniqid(rand(), TRUE));

    $insert = db_query("UPDATE {logger_users} SET token = '%s' WHERE uid = %d", $token, $usr->uid);

    if (!$insert) {
      $error = 'Error creating master token for uid @uid';
      drush_set_error('LOGGER_CREATE_MASTER_TOKEN_ENTRY', dt($error, array('@uid' => $usr->uid)));
    }
    else {
      $msg = 'Created master token @token for uid @uid in {logger_users}';
      drush_log(dt($msg, array('@token' => $token, '@uid' => $usr->uid)), 'ok'); 
    }
  }
}
