--
-- Database: `flukso`
--

-- --------------------------------------------------------

--
-- Table structure for table `logger_devices`
--

CREATE TABLE IF NOT EXISTS `logger_devices` (
  `device` varchar(32) NOT NULL,
  `serial` bigint(10) unsigned NOT NULL,
  `uid` int(10) unsigned NOT NULL default '0',
  `sha` varchar(32) default NULL,
  `created` int(10) unsigned NOT NULL default '0',
  `access` int(10) unsigned NOT NULL default '0',
  `version` smallint(5) unsigned NOT NULL default '0',
  `upgrade` smallint(5) unsigned NOT NULL default '0',
  `resets` smallint(5) unsigned NOT NULL default '0',
  `uptime` int(10) unsigned NOT NULL default '0',
  `memtotal` smallint(5) unsigned NOT NULL default '0',
  `memfree` smallint(5) unsigned NOT NULL default '0',
  `memcached` smallint(5) unsigned NOT NULL default '0',
  `membuffers` smallint(5) unsigned NOT NULL default '0',
  `uart_oe` smallint(5) unsigned NOT NULL default '0',
  `sensor` smallint(5) unsigned NOT NULL default '12',
  `country` varchar(2) default NULL,
  `comment` varchar(50) default NULL,
  PRIMARY KEY  (`device`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `logger_meters`
--

CREATE TABLE IF NOT EXISTS `logger_meters` (
  `serial` smallint(5) unsigned default NULL,
  `meter` varchar(32) NOT NULL,
  `uid` int(10) unsigned NOT NULL default '0',
  `device` varchar(32) NOT NULL default '0',
  `created` int(10) unsigned NOT NULL default '0',
  `access` int(10) unsigned NOT NULL default '0',
  `night` int(10) unsigned NOT NULL default '0',
  `type` varchar(16) default NULL,
  `function` varchar(16) default NULL,
  `class` varchar(16) default NULL,
  `voltage` int(10) unsigned default NULL,
  `current` int(10) unsigned default NULL,
  `phase` int(10) unsigned default NULL,
  `constant` decimal(10,3) unsigned default NULL,
  `value` int(10) unsigned NOT NULL default '0',
  `factor` int(10) unsigned NOT NULL default '1',
  `unit` varchar(16) default NULL,
  `enabled` tinyint(3) unsigned default NULL,
  `chart` tinyint(3) unsigned NOT NULL default '0',
  PRIMARY KEY  (`meter`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `logger_tokens`
--

CREATE TABLE IF NOT EXISTS `logger_tokens` (
  `token` varchar(32) NOT NULL default '',
  `meter` varchar(32) NOT NULL,
  `permissions` tinyint(3) unsigned NOT NULL default '62',
  PRIMARY KEY  (`token`),
  KEY `meter` (`meter`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Table structure for table `logger_users`
--

CREATE TABLE IF NOT EXISTS `logger_users` (
  `uid` int(10) unsigned NOT NULL,
  `token` varchar(32) default NULL,
  `private` tinyint(3) unsigned NOT NULL default '0',
  `electricity_unit` varchar(16) NOT NULL default 'watt',
  `water_unit` varchar(16) NOT NULL default 'lpday',
  `gas_unit` varchar(16) NOT NULL default 'lpday',
  PRIMARY KEY  (`uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
