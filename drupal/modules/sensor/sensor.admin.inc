<?php

/**
 * sensor.admin.inc : callbacks for sensor admin pages
 * Copyright (c) 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


/**
 * Callback function for the user/x/sensors page
 *
 * @param $type
 *   String defining the sensor type
 */
function _sensor_account() {
  global $user;
  $rows = array();

  $sql =
    "SELECT lm.meter, lm.type, lm.function, lt.token, lt.permissions 
     FROM {logger_meters} lm
     INNER JOIN {logger_tokens} lt
       ON lm.meter = lt.meter
     WHERE lm.uid = %d
     AND (lm.type = '%s' OR lm.type = '%s' OR lm.type = '%s')
     AND lm.enabled = %d
     ORDER BY lm.function, lm.type";

  $result = db_query($sql, $user->uid, 'electricity', 'gas', 'water', 1);

  while ($sensor = db_fetch_object($result)) {
    $row = array();
    $row[] = $sensor->meter;
    $row[] = $sensor->token;
    $row[] = $sensor->type;
    $row[] = $sensor->function;

    $rows[] = $row;
  }

  return theme('sensor_account_list', $rows);
}

/**
 * Theme function for displaying the user's sensors
 *
 * @param $items
 *   An array of table rows.
 */
function theme_sensor_account_list($items) {
  if (count($items) > 0) {
    $headers = array(t('Sensor'), t('Token'), t('Type'), t('Function'));
    $output  = theme('table', $headers, $items);
  }
  else {
    $output = t('No sensors available.');
  }

  return $output;
}
