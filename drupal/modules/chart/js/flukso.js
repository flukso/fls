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
        },
	},

	colors: [
		'#f1572f', 
		'#44c3D3', 
		'#7aab5a', 
		'#fbdf0d', 
		'#a052a0',
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
			lineWidth: 1
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

	const SECOND  = 1000; /* ms */
	const MINUTE  =  60 * SECOND;
	const QUARTER =  15 * MINUTE;
	const HOUR    =  60 * MINUTE;
	const DAY     =  24 * HOUR;
	const WEEK    =   7 * DAY;
	const MONTH   =  30 * DAY;
	const YEAR    = 365 * DAY;

	var timeParams = {
		hour  : { interval : "day"  , resolution : "minute", range : HOUR  },
		day   : { interval : "week" , resolution : "15min" , range : DAY   },
		month : { interval : "year" , resolution : "day"   , range : MONTH },
		year  : { interval : "year" , resolution : "week"  , range : YEAR  },
		night : { interval : "night", resolution : "day"   , range : MONTH }
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

        /* debugging */
		console.log(sensorObj);

		if (sensorObj.type == type) {
			chartSemaphore++;
			$.getJSON(baseUrl + sensorObj.sensor + callback, queryParams, createCb(sensorObj.function));
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
