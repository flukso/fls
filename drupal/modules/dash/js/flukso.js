/* ES5 directive to switch to strict mode */
"use strict";

/* Highcharts general options tweaking */
Highcharts.setOptions({
	global: {
		useUTC: false
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
				font: '13px "Helvetica Neue", Helvetica, Arial, sans-serif'
			}
		},

		title: {
			style: {
				color: '#333',
				fontWeight: 'bold',
				fontSize: '14px',
				fontFamily: '"Helvetica Neue", Helvetica, Arial, sans-serif'
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
				font: '13px "Helvetica Neue", Helvetica, Arial, sans-serif'
			}
		},

		title: {
			style: {
				color: '#333',
				fontWeight: 'bold',
				fontSize: '14px',
				fontFamily: '"Helvetica Neue", Helvetica, Arial, sans-serif'
			}
		}
	},

	series: []
};

Flukso.ChartState = Backbone.NestedModel.extend({
	defaults: {
		reload: true,
		type: 'electricity',
		interval: 'day',

		unit: {
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

Flukso.Sensor = Backbone.Model.extend({
	defaults: {
//		sensor: null,
		id: null, /* sensor id doubling as backbone model id */
		uid: null,
		type: null,
		'function': null,
		interval: null,
//		resolution: null,
		unit: null,
		data: null,

		baseUrl: 'https://www.flukso.net/api/sensor/',
		localUrl: null,
		callback: '?callback=?',
		version: '1.0',
	
		fetching: null
	},

	initialize: function() {
		this.set({
			unit: Flukso.unitParams[this.get('type')]
		});

		this.GET = _.bind(this.GET, this);
		Flukso.chartState.bind('change:type', this.GET);
		Flukso.chartState.bind('change:interval', this.GET);
		Flukso.chartState.bind('change:unit', this.GET);
		Flukso.chartState.bind('change:cumul', this.GET);

		this.GET();	
	},

	GET: function() {
		if (Flukso.chartState.get('type') != this.get('type')) {
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

		var queryParams = {
			version: this.get('version'),
			interval: Flukso.timeParams[this.get('interval')].interval,	/* we fetch a bigger interval than requested */
			resolution: Flukso.timeParams[this.get('interval')].resolution,
			unit: this.get('unit')
		};

		if (this.get('interval') == 'minute') {
			$.getJSON(this.get('localUrl') + this.get('id') + this.get('callback'), queryParams, process);
			setTimeout(this.GET, 1000);
		} else {
			$.getJSON(this.get('baseUrl') + this.get('id') + this.get('callback'), queryParams, process);
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

		/* fetch the user's own sensors */
		this.GET(Drupal.settings.uid);
	},

	/* sort by function (=name) */
	comparator: function(sensor) {
		return sensor.get('function');
	},

	GET: function(uid) {
		function process(sensors) {
			/* We have to fetch each counter separately, not as an object.
			 * If not, the count:change will not trigger properly
			 */
			var count = {
				electricity: Flukso.chartState.get('count.electricity'),
				gas: Flukso.chartState.get('count.gas'),
				water: Flukso.chartState.get('count.water')
			}

			for (var i in sensors) {
				/* add sensor entries to the collection */
				this.add({
					id: sensors[i].sensor,
					uid: Number(uid),
					type: sensors[i].type,
					'function': sensors[i]['function'],
					localUrl: 'http://' + sensors[i].ip + ':' + sensors[i].port + '/sensor/'
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
	el: '#type',

	initialize: function() {
		_.bindAll(this, 'render');
		this.model.bind('change:type', this.render);
		this.render();
	},

	render: function() {
		/* activate button based on chart model */
		var id = '#' + this.model.get('type');
		$(id).button("toggle");

		return this;
	},

	events: {
		"click": "clickButton"
	},

	clickButton: function(e) {
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
		this.model.set({type: $(sel).attr('id')});
		this.model.set({cumul: false}); /* making sure we don't trigger cumul on a power unit */
		this.model.set({reload: true});
	}
});

Flukso.IntervalView = Backbone.View.extend({
	el: '#interval',

	initialize: function() {
		/* needed when render is called as a callback to the change event */
		_.bindAll(this, 'render');
		this.model.bind('change:interval', this.render);
		this.render();
	},

	render: function() {
		/* activate tabs based on chart model */
		var id = '#' + this.model.get('interval');
		$(id).button("toggle");

		return this;
	},

	events: {
		"click": "clickButton"
	},

	clickButton: function(e) {
		var sel = e.target;
		this.model.set({interval: $(sel).attr('id')});
		this.model.set({reload: true});
	}
});

Flukso.UnitView = Backbone.View.extend({
	el: '#unit',

	initialize: function() {
		/* needed when render is called as a callback to the change event */
		_.bindAll(this, 'render');
		this.model.bind('change:type', this.render);
		this.render();
	},

	render: function() {
		$("#unit.dropdown-menu a").hide();

		/* show only relevant units in dropdown */
		var cls = '#unit.dropdown-menu a.' + this.model.get('type');
		$(cls).show();

		return this;
	},

	events: {
		"click": "clickDropdown"
	},

	clickDropdown: function(e) {
		var unit = {
			electricity: this.model.get('unit.electricity'),
			gas: this.model.get('unit.gas'),
			water: this.model.get('unit.water')
		};

		var sel = e.target;
		unit[this.model.get('type')] = $(sel).attr('id');

		this.model.set({unit: {
			electricity: unit.electricity,
			gas: unit.gas,
			water: unit.water
		}});

        this.model.set({cumul: $(sel).hasClass('cumul')});
		this.model.set({reload: true});
	}
});

Flukso.AlertView = Backbone.View.extend({
	el: '#alert',

	initialize: function() {
		_.bindAll(this, 'verifyNumSensors');
		this.model.bind('change:type', this.verifyNumSensors);
	},

	verifyNumSensors: function() {
		var type = this.model.get('type');

		if (this.model.get('count.' + type) == 0) {
			var tpl = _.template($('#alert-no-sensor').html());
			$(this.el).html(tpl({type: type}));

			Flukso.chartView.render();
		}

		return this;
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

		var type = Flukso.chartState.get('type');
		var unit = Flukso.chartState.get('unit.' + type);
		var cumul = Flukso.chartState.get('cumul');
		var interval = Flukso.chartState.get('interval');

		var factor = Flukso.unitPowerFactor[type][unit] ?
			Flukso.unitPowerFactor[type][unit] : Flukso.unitEnergyFactor[type][unit][interval];

		/* filter out the sensors we wish to display */
		var sensors = this.filter(function(sensor) {
			return sensor.get('type') == type && sensor.get('interval') == interval;
		});

		var series = _.map(sensors, function(sensor) {
			var start = _.last(sensor.get('data'))[0] - Flukso.timeParams[interval].range / 1000;

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
				name: sensor.get('function'),
				data: _.map(cumul ? _.filter(sensor.get('data'), truncatePoint) : sensor.get('data'), formatPoint),
				step: true,
				tooltip: {
					yDecimals: 0
				}
			};

			return entry;
		});

		if (Flukso.chartState.get('reload')) {
			Flukso.chartState.set({reload: false});

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
			reload: true
		});
	}
})

/* setup & glue code */
$(function() {
	Flukso.chartState = new Flukso.ChartState();
	Flukso.sensorCollect = new Flukso.SensorCollect();
	
	Flukso.typeView = new Flukso.TypeView({model: Flukso.chartState});
	Flukso.intervalView = new Flukso.IntervalView({model: Flukso.chartState});
	Flukso.unitView = new Flukso.UnitView({model: Flukso.chartState});
	Flukso.alertView = new Flukso.AlertView({model: Flukso.chartState});
	Flukso.chartView = new Flukso.ChartView({collection: Flukso.sensorCollect});

	Flukso.router = new Flukso.Router();
	Backbone.history.start({root: "/chart"});
	Flukso.router.updateRoute();
});
