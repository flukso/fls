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
	hour  : { interval : "day"	  , resolution : "minute", range : Flukso.time.HOUR  },
	day   : { interval : "week"   , resolution : "15min" , range : Flukso.time.DAY   },
	month : { interval : "year"   , resolution : "day"	 , range : Flukso.time.MONTH },
	year  : { interval : "decade" , resolution : "week"  , range : Flukso.time.YEAR  },
	night : { interval : "night"  , resolution : "day"	 , range : Flukso.time.MONTH }
};

Flukso.unitParams = {
	electricity : "watt",
	water		: "lperday",
	gas			: "lperday"
};

Flukso.chartConfig = {
	chart: {
		renderTo: 'chart',
		backgroundColor: '#f3f3f3',
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
				font: '11px Trebuchet MS, Verdana, sans-serif'
			}
		},

		title: {
			style: {
				color: '#333',
				fontWeight: 'bold',
				fontSize: '12px',
				fontFamily: 'Trebuchet MS, Verdana, sans-serif'
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
				font: '11px Trebuchet MS, Verdana, sans-serif'
			}
		},

		title: {
			style: {
				color: '#333',
				fontWeight: 'bold',
				fontSize: '12px',
				fontFamily: 'Trebuchet MS, Verdana, sans-serif'
			}
		}
	},

	series: []
};

Flukso.Chart = Backbone.Model.extend({
	defaults: {
		type: 'electricity',
		interval: 'hour',
		unit: 'watt'
	}
});

Flukso.Sensor = Backbone.Model.extend({
	defaults: {
//		sensor: null,
		id: null, /* sensor id doubling as backbone model id */
		uid: null,
		type: null,
		function: null,
		interval: null,
//		resolution: null,
		unit: null,
		data: null,

		baseUrl: 'https://www.flukso.net/api/sensor/',
		callback: '?callback=?',
		version: '1.0',
	
		fetching: false
	},

	initialize: function() {
		this.set({unit: Flukso.unitParams[this.get('type')]});

		/* TODO do we filter on type here? */
		this.GET();	
	},

	GET: function() {
		this.set({fetching: true});

		function process(data) {
			this.set({
				data: data,
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

		$.getJSON(this.get('baseUrl') + this.get('id') + this.get('callback'), queryParams, process);
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

	GET: function(uid) {
		function process(sensors) {
			for (var i in sensors) {
				this.add({
					id: sensors[i].sensor,
					uid: Number(uid),
					type: sensors[i].type,
					function: sensors[i].function,
					interval: 'day'
				});
			};
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
	},
});

Flukso.TypeView = Backbone.View.extend({
	el: 'ul.tabs.primary',

	initialize: function() {
		/* needed when render is called as a callback to the change event */
		_.bindAll(this, 'render');
		this.model.bind('change:type', this.render);
		this.render();
	},

	render: function() {
		var sel = $(this.el).children();

		/* clear active tabs */
		sel.removeClass('active');	
		sel.children().removeClass('active');

		/* activate tabs based on chart model */
		var id = '#' + this.model.get('type');

		sel.filter(id).addClass('active');
		sel.filter(id).children().addClass('active');

		return this;
	},

	events: {
		"click": "clickTab"
	},

	clickTab: function(e) {
		/* What isn't instantly obvious is that under the bonnet, Backbone
		 * uses jQuery's .delegate() to provide instant support for event
		 * delegation but goes a little further, extending it so that this
		 * always refers to the current view object. [1]
		 *
		 * So we cannot use $(this) to alter the target's properties.
		 *
		 * [1] https://github.com/addyosmani/backbone-fundamentals#views
		 */
		var sel = e.target.parentElement;
		this.model.set({type: $(sel).attr('id')});

		/* this click event should not bubble up to the default handler */
		e.preventDefault();
		e.stopPropagation();

		/* insert some magic here */
		//Flukso.router.navigate(type + "/" + interval, true);
	}
});

Flukso.IntervalView = Backbone.View.extend({
	el: 'ul.tabs.secondary',

	initialize: function() {
		/* needed when render is called as a callback to the change event */
		_.bindAll(this, 'render');
		this.model.bind('change:interval', this.render);
		this.render();
	},

	render: function() {
		console.log('rendering');

		var sel = $(this.el).children();

		/* clear active tabs */
		sel.removeClass('active');	
		sel.children().removeClass('active');

		/* activate tabs based on chart model */
		var id = '#' + this.model.get('interval');

		sel.filter(id).addClass('active');
		sel.filter(id).children().addClass('active');

		return this;
	},

	events: {
		"click": "clickTab"
	},

	clickTab: function(e) {
		var sel = e.target.parentElement;
		this.model.set({interval: $(sel).attr('id')});

		/* this click event should not bubble up to the default handler */
		e.preventDefault();
		e.stopPropagation();

		/* insert some magic here */
		//Flukso.router.navigate(type + "/" + interval, true);
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
			return sum + value; }, 0);

		if (!notReady) { 
			/* deep config object copy */
			var config = $.extend(true, {}, Flukso.chartConfig);

			/* TODO remove hardcoded params */
			config.xAxis.range = Flukso.timeParams['day'].range;
			config.yAxis.title.text = Flukso.unitParams['electricity'];

			/* filter out the sensors we wish to display */
			var sensors = this.filter(function(sensor) {
				/* TODO remove hardcoded params */
				return sensor.get('type') == 'electricity' && sensor.get('interval') == 'day';
			});

			var series = _.map(sensors, function(sensor) {
				function formatPoint(point) {
					/* convert to ms timestamps */
					point[0] = point[0] * 1000;

					if (point[1] == 'nan') {
						point[1] = null;
					};

					return point;
				
				};

				var entry = {
					name: sensor.get('function'),
					data: _.map(sensor.get('data'), formatPoint),
					step: true,
					tooltip: {
						yDecimals: 0
					}
				};

				return entry;
			});

			config.series = series;
			new Highcharts.StockChart(config);
		};

		return this;
	}
});

Flukso.Router = Backbone.Router.extend({
	routes: {
		":type/:interval" : "switchChart"
	},

	switchChart: function(type, interval) {
	}
})

/* setup & glue code */
$(function() {
	Flukso.chart = new Flukso.Chart;
	Flukso.sensorCollect = new Flukso.SensorCollect();

	Flukso.typeView = new Flukso.TypeView({model: Flukso.chart});
	Flukso.intervalView = new Flukso.IntervalView({model: Flukso.chart});
	Flukso.chartView = new Flukso.ChartView({collection: Flukso.sensorCollect});

	Flukso.router = new Flukso.Router();

	Backbone.history.start({root: "/chart"});
});
