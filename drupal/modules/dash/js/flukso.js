/**
 * flukso.js: dashboard js code 
 * 
 * Copyright (c) 2012 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
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

/* ES5 directive to switch to strict mode */
"use strict";

/* set AJAX call defaults */
$.ajaxSetup({
	timeout: 4000
});

/* Highcharts general options tweaking */
Highcharts.setOptions({
	global: {
		useUTC: false,
		canvasToolsURL: "sites/all/modules/dash/js/canvas-tools.js"
	}
});

/* Create a namespace and define constants for the Flukso app */
window.Flukso = {
	time: {
		SECOND:		1000, /* ms */
		MINUTE:		 60 * 1000,
		QUARTER:	 15 *  60 * 1000,
		HOUR:		 60 *  60 * 1000,
		DAY:		 24 *  60 * 60 * 1000,
		WEEK:		  7 *  24 * 60 * 60 * 1000,
		MONTH:		 30 *  24 * 60 * 60 * 1000,
		YEAR:		365 *  24 * 60 * 60 * 1000,
		DECADE:		 10 * 365 * 24 * 60 * 60 * 1000
	}
};

Flukso.timeParams = {
	minute : { interval : "minute" , resolution : "second", resSec : Flukso.time.SECOND , range : Flukso.time.MINUTE },
	hour   : { interval : "day"	   , resolution : "minute", resSec : Flukso.time.MINUTE , range : Flukso.time.HOUR   },
	day    : { interval : "week"   , resolution : "15min" , resSec : Flukso.time.QUARTER, range : Flukso.time.DAY    },
	month  : { interval : "year"   , resolution : "day"	  , resSec : Flukso.time.DAY    , range : Flukso.time.MONTH  },
	year   : { interval : "decade" , resolution : "week"  , resSec : Flukso.time.WEEK   , range : Flukso.time.YEAR   },
	night  : { interval : "night"  , resolution : "day"	  , resSec : Flukso.time.DAY    , range : Flukso.time.MONTH  }
};

/* default unit params used in API calls */
Flukso.unitParams = {
	electricity : "kwhperyear",
	gas         : "lperday",
	water       : "lperday"
};

Flukso.unitRtParams = {
	electricity : "watt",
	gas         : "lperday",
	water       : "lperday"
};

Flukso.unitPowerFactor = {
	electricity: { watt: 1.142e-1, kwhperyear: 1, eurperyear: 999 },
	gas: { lpermin: 6.944e-4, lperday: 1, m3peryear: 0.365, eurperyear: 999 },
	water: { lpermin: 6.944e-4, lperday: 1, m3peryear: 0.365, eurperyear: 999 }
};

Flukso.unitEnergyFactor = {
	electricity: {
		wh: {
			minute: 1000 * Flukso.time.SECOND / Flukso.time.YEAR,
			hour:   1000 * Flukso.time.MINUTE / Flukso.time.YEAR,
			day:    1000 * Flukso.time.QUARTER / Flukso.time.YEAR,
			month:  1000 * Flukso.time.DAY / Flukso.time.YEAR,
			year:   1000 * Flukso.time.WEEK / Flukso.time.YEAR,
			night:  1000 * Flukso.time.DAY / Flukso.time.YEAR
		}
	},

	gas: {
		liter: {
			minute: Flukso.time.SECOND / Flukso.time.DAY,
			hour:   Flukso.time.MINUTE / Flukso.time.DAY,
			day:    Flukso.time.QUARTER / Flukso.time.DAY,
			month:  Flukso.time.DAY / Flukso.time.DAY,
			year:   Flukso.time.WEEK / Flukso.time.DAY,
			night:  Flukso.time.DAY / Flukso.time.DAY
		}
	},

	water: {
		liter: {
			minute: Flukso.time.SECOND / Flukso.time.DAY,
			hour:   Flukso.time.MINUTE / Flukso.time.DAY,
			day:    Flukso.time.QUARTER / Flukso.time.DAY,
			month:  Flukso.time.DAY / Flukso.time.DAY,
			year:   Flukso.time.WEEK / Flukso.time.DAY,
			night:  Flukso.time.DAY / Flukso.time.DAY
		}
	}
};

Flukso.fontFamily = '"Droid Sans", "Helvetica Neue", Helvetica, Arial, sans-serif';

Flukso.chartDefaults = {
	chart: {
		renderTo: 'chart',
		backgroundColor: '#f5f5f5',
		animation: {
			duration : 400,
			easing : 'swing'
		},

		events: {
		}
	},

	colors: [
		'#E41A1C',	// red
		'#377EB8',	// blue
		'#4DAF4A',	// green
		'#FF7F00',	// orange
		'#984EA3',	// purple
		'#999999',	// grey
		'#A65628',	// brown
		'#F781BF'	// pink
	],

/* light color palette 
	colors: [
		'#FB8072',	// red
		'#80B1D3',	// blue
		'#B3DE69',	// green
		'#FDB462',	// orange
		'#BEBADA',	// purple
		'#8DD3C7',	// ?
		'#FCCDE5'	// pink
		'#D9D9D9',	// grey
	],
*/
	credits: {
		enabled: false
	},

	plotOptions: {
		line: {
			animation: {
				duration : 400,
				easing : 'swing'
			},

			connectNulls: false,
			lineWidth: 2
		}
	},

	rangeSelector: {
		enabled: false
	},

	xAxis: {
		gridLineWidth: 1,
		gridLineDashStyle: 'ShortDot',
		lineColor: '#000',
		tickColor: '#000',
		labels: {
			style: {
				color: '#000',
				fontSize: '13px',
				fontFamily: Flukso.fontFamily
			}
		},

		title: {
			style: {
				color: '#333',
				fontWeight: 'normal',
				fontSize: '15px',
				fontFamily: Flukso.fontFamily
			}
		}
	},

	yAxis: {
		min: 0,
		minorTickInterval: 'auto',
		gridLineWidth: 1,
		gridLineDashStyle: 'ShortDot',
		minorGridLineDashStyle: 'ShortDot',
		lineColor: '#000',
		lineWidth: 1,
		tickWidth: 1,
		tickColor: '#000',
		labels: {
			style: {
				color: '#000',
				fontSize: '13px',
				fontFamily: Flukso.fontFamily
			}
		},

		title: {
			style: {
				color: '#333',
				fontWeight: 'normal',
				fontSize: '15px',
				fontFamily: Flukso.fontFamily
			}
		}
	},

	series: []
};

Flukso.ChartState = Backbone.NestedModel.extend({
	defaults: {
		init: true,
		reloadChart: true,
		refreshData: false, /* toggle to refresh */
		type: 'electricity',
		interval: 'day',

		unit: {
			electricity: 'watt',
			gas: 'lperday',
			water: 'lperday'
		},

		unitRt: {
			electricity: 'watt',
			gas: 'lperday',
			water: 'lperday'
		},

		cumul: false,

		count: {
			electricity: 0,
			gas: 0,
			water: 0 
		}
	}
});

Flukso.User = Backbone.Model.extend({
	defaults: {
		uid: null,
		name: null,
		avatar: null,
		show: true
	},

	initialize: function() {
		Flukso.chartState.set({reloadChart: true});
		Flukso.sensorCollect.GET(this.get('uid'), this.get('name'));

		this.bind('remove', function() {
			Flukso.sensorCollect.removeByUid(this.get('uid'));
		});
	}
});

Flukso.UserCollect = Backbone.Collection.extend({
	model: Flukso.User,

	initialize: function() {
	},

	getByUid: function(uid) {
		return this.find(function(user) {
			return user.get('uid') == uid;
		});
	},

	getUids: function() {
		return this.map(function(user) {
			return user.get('uid');
		});
	},

	syncUids: function() {
		if (!Flukso.chartState.get('init')) {
			/* skip the first uid entry (= /me) */
			var subs = _.rest(this.getUids());
			/* should be a PUT method but not all browsers seem to support it */
			$.ajax({
				type: 'POST',
				url: '/dash/subscriptions',
				contentType: 'application/json',
				data: JSON.stringify(subs)
			});
		} 
	}
});

Flukso.UserView = Backbone.View.extend({
	el: '#fluksonians',
	template: _.template($('#avatar-add').html()),

	initialize: function() {
		_.bindAll(this, 'action', 'add', 'remove', 'show');

		/* bind this view to the add and remove events of the collection */
		this.collection.bind('add', this.add);

		/* first populate user entry for /me */
		this.collection.add(Drupal.settings.me);

		/* ... then populate with stored subscriptions */
		_.each(Drupal.settings.subscriptions, function(sub) {
			sub.show = false;
			this.collection.add(sub);
		}, this);
	},

	events: {
		'click .avatar': 'action'
	},

	action: function(e) {
		if ($('#fluksonian-show').hasClass('active'))
			this.show(e)
		else
			this.remove(e);
	},

	add: function(user) {
		$(this.el).append(this.template(user.attributes));

		if (user.get('show') == false) {
			var sel = '.avatar [uid=' + user.get('uid') + ']';
			$(sel).parent().addClass('grey-out');
		}

		this.collection.syncUids();
	},

	remove: function(e) {
		var sel = e.target;
		var uid = Number($(sel).attr('uid'));
		var user = this.collection.getByUid(uid);

		/* /me cannot be removed from the chart */
		if (Drupal.settings.me.uid == uid) {
			return
		};

		$(sel).parent().remove();
		/* triggers a chain of actions that removes the
		   entries in userCollect and sensorCollect */
		this.collection.remove(user);
		this.collection.syncUids();
		
		Flukso.chartState.set({
			reloadChart: true,
			refreshData: !Flukso.chartState.get('refreshData')
		});
	},

	show: function(e) {
		var sel = e.target;
		var uid = Number($(sel).attr('uid'));
		var user = this.collection.getByUid(uid);

		user.set({show: !user.get('show')});
		$(sel).parent().toggleClass('grey-out');

		Flukso.chartState.set({reloadChart: true});
	}
});

Flukso.UserCtrlView = Backbone.View.extend({
	el: '#fluksonian-ctrl',

	initialize: function() {
		if (Drupal.settings.me.uid == 0) {
			$('#fluksonians').parent().remove();
			$(this.el).parent().remove();
			return;
		}

		$(this.el).show();
		_.bindAll(this, 'add');

		/* show is the default ctrl action */
		$('#fluksonian-show').addClass('active');
		/* init typeahead */
		this.add();
	},

	add: function() {
		var that = this;

		$('#fluksonian-add').typeahead({
			source: function(typeahead, query) {
				return $.getJSON('/dash/user/autocomplete/' + query)
					.success(function(data) {
						typeahead.process(data);
					});
			},

			property: 'name',

			onselect: function (fluksonian) {
				/*  reject duplicate entries */
				if (!that.collection.getByUid(fluksonian.uid)) {
					that.collection.add(fluksonian);
				}

				/* clear the typeahead's text input */
				$('#fluksonian-add').val('');
			}
		});
	}
});

Flukso.Sensor = Backbone.Model.extend({
	defaults: {
//		sensor: null,
		id: null, /* sensor id doubling as backbone model id */
		uid: null,
		userName: null,
		type: null,
		'function': null,
		interval: null,
//		resolution: null,
		unit: null,
		unitRt: null,
		data: null,

		baseUrl: 'https://www.flukso.net/api/sensor/',
		localUrl: null,
		callback: '?callback=?',
		version: '1.0',
	
		fetching: null,
		timeoutId: null
	},

	initialize: function() {
		this.set({
			unit: Flukso.unitParams[this.get('type')],
			unitRt: Flukso.unitRtParams[this.get('type')]
		});

		this.GET = _.bind(this.GET, this);
		Flukso.chartState.bind('change:refreshData', this.GET);
		Flukso.chartState.bind('change:type', this.GET);
		Flukso.chartState.bind('change:interval', this.GET);
		Flukso.chartState.bind('change:unit', this.GET);
		Flukso.chartState.bind('change:cumul', this.GET);
		Flukso.userCollect.bind('change:show', this.GET);

		this.GET();
	},

	decouple: function() {
		Flukso.chartState.unbind('change:refreshData', this.GET);
		Flukso.chartState.unbind('change:type', this.GET);
		Flukso.chartState.unbind('change:interval', this.GET);
		Flukso.chartState.unbind('change:unit', this.GET);
		Flukso.chartState.unbind('change:cumul', this.GET);
		Flukso.userCollect.unbind('change:show', this.GET);
	},

	GET: function() {
		if (Flukso.chartState.get('type') != this.get('type')
			|| !Flukso.userCollect.getByUid(this.get('uid')).get('show'))
		{
			return;
		};

		this.set({
			fetching: true,
			interval: Flukso.chartState.get('interval')},
		{silent: true});

		function process(data) {
			var shift = Flukso.timeParams[this.get('interval')].resSec;

			function shiftStamp(point) {
				/* convert to ms timestamps and shift left one resolution interval */
				point[0] = point[0] * 1000 - shift;
				return point;
			};

			this.set({
				data: _.map(data, shiftStamp),
				fetching: false
			});
		};
	
		process = _.bind(process, this); 

		function errorRt(data, stat, req) {
			clearTimeout(this.get('timeoutId'));

			if (stat == 'timeout') {
				Flukso.alertView.timeoutRt(this.get('function'));
			}
		};

		errorRt = _.bind(errorRt, this);

		var queryParams = {
			version: this.get('version'),
			interval: Flukso.timeParams[this.get('interval')].interval,	/* we fetch a bigger interval than requested */
			resolution: Flukso.timeParams[this.get('interval')].resolution,
			unit: this.get('interval') == 'minute' ? this.get('unitRt') : this.get('unit')
		};

		if (this.get('interval') == 'minute') {
			if (this.get('localUrl') == null) {
				Flukso.alertView.noRealtime();
				return;
			}

			$.getJSON(this.get('localUrl') + this.get('id') + this.get('callback'), queryParams)
				.success(process)
				.error(errorRt);
			this.set({timeoutId: setTimeout(this.GET, 1000)});
		} else {
			$.getJSON(this.get('baseUrl') + this.get('id') + this.get('callback'), queryParams)
				.success(process);
		}
	}
});

Flukso.SensorCollect = Backbone.Collection.extend({
	model: Flukso.Sensor,

	initialize: function() {
		/* set the attributes here, in the absence of a defaults entry for a collection */
		this.attributes = {
			baseUrl: 'https://www.flukso.net/api/user/',
			callback: '?callback=?',
			version: '1.0'
		};
	},

	comparator: function(sensor) {
		var user = Flukso.userCollect.getByUid(sensor.get('uid'));
		/* truncate leading c and pad with zeros */
		var userPosition = 1000 + Number(user.cid.substring(1));
		/* primary sort on user insertion order, secondary on sensor name */
		var sensorPosition = userPosition + sensor.get('function').toLowerCase();
		return sensorPosition;
	},

	getByUid: function(uid) {
		return this.filter(function(sensor) {
			return sensor.get('uid') == uid;
		});
	},

	removeByUid: function(uid) {
		var that = this;

		_.each(this.getByUid(uid), function(sensor) {
			that.remove(sensor);
			sensor.decouple();
		});
	},

	GET: function(uid, name) {
		function process(sensors) {
			/* We have to fetch each counter separately, not as an object.
			 * If not, the count:change will not trigger properly
			 */
			var count = {
				electricity: Flukso.chartState.get('count.electricity'),
				gas: Flukso.chartState.get('count.gas'),
				water: Flukso.chartState.get('count.water')
			};

			for (var i in sensors) {
				/* add sensor entries to the collection */
				this.add({
					id: sensors[i].sensor,
					uid: Number(uid),
					userName: name,
					type: sensors[i].type,
					'function': sensors[i]['function'],
					localUrl: sensors[i].ip == 'undefined' ?
						null : 'http://' + sensors[i].ip + ':' + sensors[i].port + '/sensor/'
				});

				count[sensors[i].type]++;
			};

			/* We've got to set the nested attributes directly for the change events to fire
			 * see: http://afeld.github.com/backbone-nested
			 */ 
			Flukso.chartState.set({count: {
				electricity: count.electricity,
				gas: count.gas,
				water: count.water
			}});

			/* do not refresh all sensors when users & sensors are still being populated */
			if (!Flukso.chartState.get('init'))
				Flukso.chartState.set({refreshData: !Flukso.chartState.get('refreshData')});
		};

		/* Bind the function to the object. So whenever the function is called, the value
		 * of this will be the object it is bound to.
		 */
		process = _.bind(process, this);

		var queryParams = {
			version: this.attributes.version
		};

		/* GET /user/<uid>/sensor?version=1.0&callback=? */
		$.getJSON(this.attributes.baseUrl + uid + '/sensor' + this.attributes.callback, queryParams, process);
	}
});

Flukso.TypeView = Backbone.View.extend({
	el: '[menu=type]',

	initialize: function() {
		_.bindAll(this, 'render', 'clickButton');
		this.model.bind('change:type', this.render);

		/* use JQuery to register the event handler since
		   the backbone implementation only attaches to the
		   first element */
		$('[menu=type]').click(this.clickButton);

		this.render();
	},

	render: function() {
		/* no radio-style behaviour in drop-downs*/
		$("[menu=type] li a").removeClass("active");
		/* activate button based on chart model
		   IE doesn't like 'type' as an attribute */
		var sel = '[typ=' + this.model.get('type') + ']';
		$(sel).button("toggle");

		return this;
	},

	events: {
//		"click": "clickButton"
	},

	clickButton: function(e) {
		Flukso.alertView.clear();

		/* What isn't instantly obvious is that under the bonnet, Backbone
		 * uses jQuery's .delegate() to provide instant support for event
		 * delegation but goes a little further, extending it so that this
		 * always refers to the current view object. [1]
		 *
		 * So we cannot use $(this) to alter the target's properties.
		 *
		 * [1] https://github.com/addyosmani/backbone-fundamentals#views
		 */
		var sel = e.target;
		this.model.set({type: $(sel).attr('typ')});
		this.model.set({cumul: false}); /* making sure we don't trigger cumul on a power unit */
		this.model.set({reloadChart: true});
	}
});

Flukso.IntervalView = Backbone.View.extend({
	el: '[menu=interval]',

	initialize: function() {
		/* needed when render is called as a callback to the change event */
		_.bindAll(this, 'render', 'clickButton');
		this.model.bind('change:interval', this.render);

		$('[menu=interval]').click(this.clickButton);

		this.render();
	},

	render: function() {
		/* no radio-style behaviour in drop-downs*/
		$("[menu=interval] li a").removeClass("active");
		/* activate tabs based on chart model */
		var sel = '[interval=' + this.model.get('interval') + ']';
		$(sel).button("toggle");

		return this;
	},

	events: {
//		"click": "clickButton"
	},

	clickButton: function(e) {
		Flukso.alertView.clear();

		var sel = e.target;
		this.model.set({interval: $(sel).attr('interval')});
		this.model.set({reloadChart: true});
	}
});

Flukso.UnitView = Backbone.View.extend({
	el: '[menu=unit]',

	initialize: function() {
		/* needed when render is called as a callback to the change event */
		_.bindAll(this, 'render', 'clickDropdown');
		this.model.bind('change:type', this.render);
		this.model.bind('change:interval', this.render);

        $('[menu=unit]').click(this.clickDropdown);

		this.render();
	},

	render: function() {
		$("[menu=unit].dropdown-menu a").hide();

		/* show only relevant units in dropdown */
		var cls = '[menu=unit].dropdown-menu a.' + this.model.get('type');

		if (this.model.get('interval') == 'minute') {
			cls += '.rt';
		};

		$(cls).show();

		return this;
	},

	events: {
//		"click": "clickDropdown"
	},

	clickDropdown: function(e) {
		Flukso.alertView.clear();

		var unit = {
			electricity: this.model.get('unit.electricity'),
			gas: this.model.get('unit.gas'),
			water: this.model.get('unit.water')
		};

		var sel = e.target;
		unit[this.model.get('type')] = $(sel).attr('unit');

		this.model.set({unit: {
			electricity: unit.electricity,
			gas: unit.gas,
			water: unit.water
		}});

        this.model.set({cumul: $(sel).hasClass('cumul')});
		this.model.set({reloadChart: true});
	}
});

Flukso.AlertView = Backbone.View.extend({
	el: '#alert',

	initialize: function() {
		_.bindAll(this, 'verifyNumSensors');
		_.bindAll(this, 'noRealtime');
		_.bindAll(this, 'timeoutRt');
		_.bindAll(this, 'clear');
		this.model.bind('change:type', this.verifyNumSensors);
	},

	verifyNumSensors: function() {
		var type = this.model.get('type');

		if (this.model.get('count.' + type) == 0) {
			var tpl = _.template($('#alert-no-sensor').html());
			$(this.el).html(tpl({type: type}));
		}

		return this;
	},

	noRealtime: function() {
		var tpl = _.template($('#alert-no-realtime').html());
		$(this.el).html(tpl());
	},

	timeoutRt: function(func) {
		var tpl = _.template($('#alert-timeout-rt').html());
		$(this.el).html(tpl({func: func}));
	},

	clear: function() {
		$(this.el).empty();
	}
});

Flukso.ChartView = Backbone.View.extend({
//	el: $('#chart'),

	initialize: function() {
		this.collection.bind('change:fetching', this.render);
	},

	render: function() {
		/* 'this' points to the collection! */
		var fetching = this.map(function(sensor) {
			return sensor.get('fetching') == true ? 1 : 0;
		});

		/* mapreduce to a single ready variable */
		var notReady = _.reduce(fetching, function(sum, value) {
			return sum + value;
        }, 0);

		if (notReady) { 
			return this;
		};

		/* init has finished when the chart renders for the first time */
		Flukso.chartState.set({init: false});

		var type = Flukso.chartState.get('type');
		var interval = Flukso.chartState.get('interval');

		if (interval == 'minute') {
			var unit = Flukso.chartState.get('unitRt.' + type);
			var cumul = false;
			var factor = 1;
		} else {
			var unit = Flukso.chartState.get('unit.' + type);
			var cumul = Flukso.chartState.get('cumul');
			var factor = Flukso.unitPowerFactor[type][unit] ?
				Flukso.unitPowerFactor[type][unit] : Flukso.unitEnergyFactor[type][unit][interval];
		};

		/* filter out the sensors we wish to display */
		var sensors = this.filter(function(sensor) {
			return sensor.get('type') == type
				&& sensor.get('interval') == interval
				&& Flukso.userCollect.getByUid(sensor.get('uid')).get('show')
				&& _.find(sensor.get('data'), function(point) {
					return point[1] != 'nan'});
		});

		var series = _.map(sensors, function(sensor) {
			var start = _.last(sensor.get('data'))[0] - Flukso.timeParams[interval].range;

			function formatPoint(point, idx, list) {
				if (point[1] == 'nan') {
					point[1] = null;
				} else {
					point[1] = point[1] * factor;

					if (cumul && idx != 0) {
						point[1] = point[1] + list[idx - 1][1];
					};
				};

				return point;
			};

			function truncatePoint(point) {
				return point[0] < start ? false : true;
			};

			var entry = {
				name: sensor.get('userName') + '.' + sensor.get('function'),
				data: _.map(cumul ? _.filter(sensor.get('data'), truncatePoint) : sensor.get('data'), formatPoint),
				step: true,
				tooltip: {
					yDecimals: 0
				}
			};

			return entry;
		});

		/* real-time chart should not reload the second time */
		if (Flukso.chartState.get('reloadChart')) {
			Flukso.chartState.set({reloadChart: false});

			/* deep config object copy */
			var config = $.extend(true, {}, Flukso.chartDefaults);

			config.xAxis.range = Flukso.timeParams[interval].range;
			config.yAxis.title.text = unit;
			config.series = series;

			Flukso.chart = new Highcharts.StockChart(config);
		} else {
			_.each(series, function(entry, i) {
				Flukso.chart.series[i].setData(entry.data);
			});
		};

		return this;
	}
});

Flukso.ExportView = Backbone.View.extend({
	el: '#export-ctrl',

	initialize: function() {
    },

	render: function() {
	},

	events: {
		'click': 'clickButton'
	},

	clickButton: function(e) {
		var type = Flukso.chartState.get('type');
		var interval = Flukso.chartState.get('interval');

		if (interval == 'minute') {
			var unit = Flukso.chartState.get('unitRt.' + type);
			var factor = 1;
		} else {
			var unit = Flukso.chartState.get('unit.' + type);
			var factor = Flukso.unitPowerFactor[type][unit] ?
				Flukso.unitPowerFactor[type][unit] : Flukso.unitEnergyFactor[type][unit][interval];
		};

		/* we only export our own sensors */
		var sensors = Flukso.sensorCollect.filter(function(sensor) {
			return sensor.get('type') == type
				&& sensor.get('interval') == interval
				&& sensor.get('uid') == Drupal.settings.me.uid
		});

		function stamp(point) {
			var time = new Date(point[0]);
			return [time.toISOString()];
		};

		var json = {
			'fields': [''],
			'data': _.map(sensors[0].get('data'), stamp)
		}

		_.each(sensors, function(sensor) {
			json.fields.push(sensor.get('function'));
			_.each(sensor.get('data'), function(point, idx, list) {
				json.data[idx].push(point[1] == 'nan' ? '' : point[1]);
			});
		});

		var csv = Papa.unparse(json);

		var now = new Date();
		var dateTime = now.getFullYear() + '-' + now.getMonth()+1 + '-' +
				now.getDate() + '-' + now.getHours() +
				now.getMinutes() + now.getSeconds();
		var fileName = 'flukso-' + dateTime + '.csv';
		var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
		if (navigator.msSaveBlob) { // IE 10+
			navigator.msSaveBlob(blob, fileName);
		} else {
			var link = document.createElement('a');
			if (link.download !== undefined) { // feature detection
				// Browsers that support HTML5 download attribute
				var url = URL.createObjectURL(blob);
				link.setAttribute('href', url);
				link.setAttribute('download', fileName);
				link.style = 'visibility:hidden';
				document.body.appendChild(link);
				link.click();
				document.body.removeChild(link);
			}
		}
	}
});

Flukso.Router = Backbone.Router.extend({
	routes: {
		":type/:interval" : "gotoChart"
	},

	initialize: function() {
		_.bindAll(this, 'updateRoute');
		Flukso.chartState.bind('change', this.updateRoute);
	},

	updateRoute: function() {
		var type = Flukso.chartState.get('type');
		var interval = Flukso.chartState.get('interval');

		this.navigate(type + "/" + interval, true);
	},

	gotoChart: function(type, interval) {
		Flukso.chartState.set({
			type: type,
			interval: interval,
			reloadChart: true
		});
	}
});

/* setup & glue code */
$(function() {
	Flukso.chartState = new Flukso.ChartState();
	Flukso.sensorCollect = new Flukso.SensorCollect();
	Flukso.userCollect = new Flukso.UserCollect();

	Flukso.typeView = new Flukso.TypeView({model: Flukso.chartState});
	Flukso.intervalView = new Flukso.IntervalView({model: Flukso.chartState});
	Flukso.unitView = new Flukso.UnitView({model: Flukso.chartState});
	Flukso.alertView = new Flukso.AlertView({model: Flukso.chartState});
	Flukso.chartView = new Flukso.ChartView({collection: Flukso.sensorCollect});
	Flukso.userView = new Flukso.UserView({collection: Flukso.userCollect});
	Flukso.userCtrlView = new Flukso.UserCtrlView({collection: Flukso.userCollect});
	Flukso.exportView = new Flukso.ExportView({collection: Flukso.sensorCollect});

	Flukso.router = new Flukso.Router();
	Backbone.history.start({root: "/dash"});
	Flukso.router.updateRoute();
});
