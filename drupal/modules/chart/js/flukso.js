/* ES5 directive to switch to strict mode */
"use strict";

/* provide a map method to the Array object for older, non-ES5 implementations */
if (!Array.prototype.map) {
	Array.prototype.map = function(fun /*, thisp*/) {
		var len = this.length;

		if (typeof fun != "function") {
			throw new TypeError();
		};

		var res = new Array(len);
		var thisp = arguments[1];

		for (var i = 0; i < len; i++) {
			if (i in this) {
				res[i] = fun.call(thisp, this[i], i, this);
			};
		};

		return res;
	};
};

Highcharts.setOptions({
	global: {
		useUTC: false
	}
});

window.chart = null;

window.chartConfig = {
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

$(function () {
	var processSensors = function(uidSensorsObject) {
		window.flukso = new Array();
		flukso[uid] = { "sensors" : uidSensorsObject };

		/* render the default chart */
		getSensorData("electricity", "day");
	};

	var uid = Drupal.settings.uid;

	var baseUrl = 'https://www.flukso.net/api/user/' + uid + '/sensor';
	var callback = '?callback=?';
	var queryParams = {
		version: '1.0'
	};

	/* GET /user/<uid>/sensor?version=1.0&callback=? */
	$.getJSON(baseUrl + callback, queryParams, processSensors);
});

window.getSensorData = function(type, interval) {
	/* closure returning a unique callback for each getJSON invocation */
	var createCb = function(sensorId) {
		return function(data) {
			var formatPoint = function(point) {
				/* convert to ms timestamps */
				point[0] = point[0] * 1000;

				if (point[1] == 'nan') {
					point[1] = null
				};

				return point;
			};

			var series = {
				name: sensorId,
				data: data.map(formatPoint),
				step: true,
				tooltip: {
					yDecimals: 0
				}
			};
 
			chartConfig.series.push(series);

			/* AJAX callback synchronization through the semaphore
			 * the last request to return will take care of chart rendering
			 */
			chartSemaphore--;

			if (chartSemaphore == 0) {
				window.chart = new Highcharts.StockChart(chartConfig);
			}
		};
	};

	var time = {
		SECOND:		1000, /* ms */
		MINUTE:		 60 * 1000,
		QUARTER:	 15 *  60 * 1000,
		HOUR:		 60 *  60 * 1000,
		DAY:		 24 *  60 * 60 * 1000,
		WEEK:		  7 *  24 * 60 * 60 * 1000,
		MONTH:		 30 *  24 * 60 * 60 * 1000,
		YEAR:		365 *  24 * 60 * 60 * 1000,
		DECADE:		 10 * 365 * 24 * 60 * 60 * 1000
	};

	var timeParams = {
		hour  : { interval : "day"    , resolution : "minute", range : time.HOUR  },
		day   : { interval : "week"   , resolution : "15min" , range : time.DAY   },
		month : { interval : "year"   , resolution : "day"   , range : time.MONTH },
		year  : { interval : "decade" , resolution : "week"  , range : time.YEAR  },
		night : { interval : "night"  , resolution : "day"   , range : time.MONTH }
    };

	var unitParams = {
		electricity : "watt",
		water       : "lperday",
		gas         : "lperday"
	};

	var baseUrl = 'https://www.flukso.net/api/sensor/';
	var callback = '?callback=?';
	var queryParams = {
		version: '1.0',
		interval: timeParams[interval].interval,	/* we fetch a bigger interval than requested */
		resolution: timeParams[interval].resolution,
		unit: unitParams[type]
	};

	chartConfig.series = [];

	/* For some weird reason, the rendering of the chart changes the
	 * original chartConfig object. So we're catching this case here.
	 */
	if (chart == null) {
		chartConfig.xAxis.range = timeParams[interval].range;
		chartConfig.yAxis.title.text = unitParams[type];
	}
	else {
		chartConfig.xAxis[0].range = timeParams[interval].range;
		chartConfig.yAxis[0].title.text = unitParams[type];
	};

	window.chartSemaphore = 0;

	var uid = Drupal.settings.uid;

	for (var i in flukso[uid].sensors) {
		var sensorObj = flukso[uid].sensors[i];

        /* debugging
		console.log(sensorObj); */

		if (sensorObj.type == type) {
			chartSemaphore++;
			$.getJSON(baseUrl + sensorObj.sensor + callback, queryParams, createCb(sensorObj['function']));
		};
	};

	/* TODO return to previous tab when no data for this type is present */
	if (chartSemaphore == 0) {
		location.reload();
	};
};


$('ul.tabs li').click(function() {
    /* remove active class from former tab */
	$(this).siblings().removeClass('active');
	$(this).siblings().children().removeClass('active');

    /* activate newly clicked tab */
	$(this).addClass('active');
	$(this).children().addClass('active');

    /* determine the type and interval */
    var type = $('ul.tabs.primary li.active').attr('id');
    var interval = $('ul.tabs.secondary li.active').attr('id');

    /* insert some magic here */
    getSensorData(type, interval);

	/* preventDefault and stopPropagation */
	return false;
});
