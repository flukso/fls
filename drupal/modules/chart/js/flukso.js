Highcharts.setOptions({
	global: {
		useUTC: false
	}
});

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
		range: 24 * 3600 * 1000, /* = day */
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
		/* hardcode the uid for the time being */
		var Uid = 1;

		window.flukso = new Array();
		flukso[Uid] = { "sensors" : uidSensorsObject };

		getSensorData("electricity", "day");
	};

	var baseUrl = 'https://www.flukso.net/api/user/1/sensor';
	var callback = '?callback=?';
	var queryParams = {
		version: '1.0'
	};

	/* GET /user/<uid>/sensor?version=1.0&callback=? */
	$.getJSON(baseUrl + callback, queryParams, processSensors);
});

window.getSensorData = function(type, interval) {
	/* closure returning a unique callback for each getJSON invocation */
	function createCb(sensorId) {
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
				console.log('charting now!');
				window.chart = new Highcharts.StockChart(chartConfig);
			}
		};
	};

	var baseUrl = 'https://www.flukso.net/api/sensor/';
	var sensorId = 'c1411c6b4f9910bbbab09f145f8533b9';
	var callback = '?callback=?';
	var queryParams = {
		version: '1.0',
		token: '9b2b5b4cd37f3741fc7cc2b7d69bdab2',
		interval: 'week',
		resolution: '15min',
		unit: 'watt'
	};

    chartConfig.series = [];
	window.chartSemaphore = 0;

	/* hardcoded Uid */
	var Uid = 1;

	for (var i in flukso[Uid].sensors) {
		var sensorObj = flukso[Uid].sensors[i];

        /* debugging */
		console.log(sensorObj);

		if (sensorObj.type == type) {
			chartSemaphore++;
			$.getJSON(baseUrl + sensorObj.sensor + callback, queryParams, createCb(sensorObj.function));
		};
	};

	/* TODO return to previous tab when no data for this type is present */
	if (chartSemaphore == 0) {
		window.chart = new Highcharts.StockChart(chartConfig);
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

	return false; /* preventDefault and stopPropagation */
});
