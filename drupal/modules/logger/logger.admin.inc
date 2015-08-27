<?php

//
// logger.admin.inc : callbacks for logger account and admin pages
// Copyright (c) 2010 flukso.net
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

define(FLM02_16M_UPGRADE_TARGET, 216);
define(FLM02_32M_UPGRADE_TARGET, 247);

/**
 * Callback function for the user/x/devices page's drupal_get_form
 * 
 * @ingroup forms
 * @see _logger_account_devices_validate
 * @see _logger_account_devices_submit
 * @see theme_logger_account_devices
 */
function _logger_account_devices() {
  $form['serial'] = array(
    '#type' => 'textfield',
    '#size' => 10,
    '#maxlength' => 10,
    '#required' => TRUE,
  );
  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Add device'),
  );
  $form['#validate'][] = '_logger_account_devices_validate';
  $form['#submit'][] = '_logger_account_devices_submit';
  $form['#theme'] = 'logger_account_devices';

  return $form;
}

function _logger_account_devices_validate($form, &$form_state) {
  $serial = $form_state['values']['serial'];
  if (strlen($serial) == 10 && is_numeric(substr($serial, 2, 9))) {
    if (db_result(db_query("SELECT COUNT(*) FROM {logger_devices} WHERE serial = %d AND uid = %d", substr($serial, 2, 9), 0)) == 0) {
      form_set_error('serial', t('Please specify a valid serial number.'));
    }
  }
  else {
    form_set_error('serial', t('Please specify a proper 10-character serial number.'));
  }
}

function _logger_account_devices_submit($form, &$form_state) {
  global $user;
  $serial = substr($form_state['values']['serial'], 2, 9);
  $device = db_result(db_query("SELECT device FROM {logger_devices} WHERE serial = %d", $serial));

  db_query("UPDATE {logger_devices} SET uid = %d WHERE serial = %d", $user->uid, $serial); 
  db_query("UPDATE {logger_meters} SET uid = %d WHERE device = '%s'", $user->uid, $device);

  // check whether the user is already a true fluksonian
  // if not, generate the proper entries in {users_roles} and {logger_users}
  if (!user_access('logger')) {
    $rid = db_result(db_query("SELECT rid FROM {role} WHERE name = '%s'", 'fluksonian'));
    db_query("INSERT INTO {users_roles} (uid, rid) VALUES (%d, %d)", $user->uid, $rid);

    $master_token = md5(uniqid(rand(), TRUE));
    db_query("INSERT INTO {logger_users} (uid, token, private) VALUES (%d, '%s', %d)", $user->uid, $master_token, 0);
  }

  drupal_set_message(sprintf("The FLM with serial FL%08s is now associated with your account.", $serial));
}

/**
 * Callback function for the user/x/device/y/remove path
 * 
 * @param $did
 *   ID of the device that needs to be removed from the user account
 */
function _logger_account_device_remove($did) {
  global $user;

  // built-in check so that user is only able to remove own devices
  db_query("UPDATE {logger_devices} SET uid = 0 WHERE device = '%s' AND uid = %d", $did, $user->uid);   
  db_query("UPDATE {logger_meters} SET uid = 0 WHERE device = '%s' AND uid = %d", $did, $user->uid);

  $serial = db_result(db_query("SELECT serial FROM {logger_devices} WHERE device = '%s'", $did));
  drupal_set_message(sprintf("The FLM with serial FL%08s has been removed from your account.", $serial));

  // return to the user/x/devices page
  $destination = sprintf("/user/%d/devices", $user->uid);
  drupal_goto($destination);
}

/**
 * Callback function for the user/x/device/y/upgrade path
 * 
 * @param $did
 *   ID of the device that needs upgrading
 */
function _logger_account_device_upgrade($did) {
  global $user;

  // built-in check so that user is only able to upgrade own devices
  $device = db_fetch_object(db_query("SELECT serial, version, upgrade
                                      FROM {logger_devices}
                                      WHERE device = '%s' AND uid = %d", $did, $user->uid));

  if ($device->version >= 200 && $device->serial >= 2e6) {
    $upgrade_target = FLM02_32M_UPGRADE_TARGET;
  }
  elseif ($device->version >= 200 && $device->serial >= 1e6) {
    $upgrade_target = FLM02_16M_UPGRADE_TARGET;
  }

  if ($device->version >= 200
      && $device->version < $upgrade_target
      && $device->upgrade == 0) {
    db_query("UPDATE {logger_devices}
              SET upgrade = %d
              WHERE device = '%s'", $upgrade_target, $did);

    $msg = "The FLM with serial FL%08s will be upgraded to firmware version %d at the next FLM heartbeat.";
    drupal_set_message(sprintf($msg, $device->serial, $upgrade_target));
  };

  // return to the user/x/devices page
  $destination = sprintf("/user/%d/devices", $user->uid);
  drupal_goto($destination);
}

/**
 * Theme function for displaying the devices form
 *
 * @ingroup themeable
 *
 * @param $form
 *   The device form array.
 */
function theme_logger_account_devices($form) {
  global $user;
  $rows = array();

  $result = db_query("SELECT device, serial, access, version, upgrade, resets, uptime
                      FROM {logger_devices}
                      WHERE uid = %d
                      ORDER BY serial", $user->uid);

  function unix_to_userlocaltime($unix) {
    global $user;
    $unix_local = gmdate('r', $unix + $user->timezone);
    return substr($unix_local, 4, strlen($unix_local) - 10);
  }

  function seconds_to_dayshours($seconds) {
    $days    = intval($seconds / 86400);
    $hours   = intval(($seconds % 86400) / 3600);
    return sprintf('%dd %dh', $days, $hours);
  }

  while ($device = db_fetch_object($result)) {
    $row = array();
    $row[] = 'FL' . sprintf('%08s', $device->serial);
    $row[] = $device->upgrade == 0 ? $device->version : $device->upgrade . '**'; 
    $row[] = $device->resets;
    $row[] = unix_to_userlocaltime($device->access);
    $row[] = seconds_to_dayshours($device->uptime);

    $remove = sprintf('user/%d/device/%s/remove', $user->uid, $device->device);

    if ($device->version >= 200 && $device->serial >= 2e6) {
      $upgrade_target = FLM02_32M_UPGRADE_TARGET;
    }
    elseif ($device->version >= 200 && $device->serial >= 1e6) {
      $upgrade_target = FLM02_16M_UPGRADE_TARGET;
    }

    $button_remove = array('class' => 'btn btn-warning', 'role' => 'button');
    if ($device->version >= 200
        && $device->version < $upgrade_target
        && $device->upgrade == 0) {
      $upgrade = sprintf('user/%d/device/%s/upgrade', $user->uid, $device->device);
      //$attributes = array('title' => 'Upgrade the firmware of this FLM02 to version ' . $upgrade_target);
      $button_upgrade = array(
        'class' => 'btn btn-success',
        'role' => 'button',
        'title' => 'Upgrade the firmware of this FLM02 to version ' . $upgrade_target);
      $row[] = l(t('remove'), $remove, array('attributes' => $button_remove)) . ' ' .
               l(t('upgrade'), $upgrade, array('attributes' => $button_upgrade));
    }
    else {
      $row[] = l(t('remove'), $remove, array('attributes' => $button_remove));
    }

    $rows[] = $row;
  }

  $rows[] = array(drupal_render($form['serial']), array('data' => drupal_render($form['submit']), 'colspan' => 5));

  $output = drupal_render($form);
  $output .= theme('logger_account_devices_list', $rows);

  return $output;
}

/**
 * Theme function for displaying the list of devices
 *
 * @ingroup themeable
 *
 * @param $items
 *   An array of table rows.
 */
function theme_logger_account_devices_list($items) {
  if (count($items) > 0) {
    $headers = array(t('Serial'), t('Version'), t('Resets'), t('Last heartbeat'), t('Uptime'), t('Operations'));
    $output  = theme('table', $headers, $items);
  }
  else {
    $output = t('No devices available.');
  }

  return $output;
}

/**
 * Callback function for the user/x/privacy page
 */
function _logger_account_privacy() {
  $output .= drupal_get_form('_logger_account_privacy_form');
  return $output;
}

/**
 * Generates the privacy form.
 */
function _logger_account_privacy_form() {
  global $user;

  $form['privacy'] = array(
    '#title' => t('Set your preferred privacy mode'),
    '#type' => 'radios',
    '#options' => array(
      t('Shared within Flukso'),
      t('Private'),
    ),
    '#description' => t("When selecting Private mode, your data stream will not be drawn on other people's graph and all statistics will be marked as 'prv'. Conversely, nobody else's data stream will be drawn on your chart."),
    '#default_value' => db_result(db_query("SELECT private
                                            FROM {logger_users}
                                            WHERE uid = %d", $user->uid)),
  );
  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Submit'),
  );
  return $form;
}

/**
 * Process settings form submissions.
 */
function _logger_account_privacy_form_submit($form, &$form_state) {
  global $user;

  // the built-in validation function already checks elements with options
  $private = $form_state['values']['privacy'];
  db_query("UPDATE {logger_users}
            SET private = %d
            WHERE uid = %d", $private, $user->uid);

  // page redirection to the dashboard after form submission
  $form_state['redirect'] = 'logger';
}

/**
 * Define the administration settings form for the logger module
 */
function _logger_admin_settings() {
//TODO
}

