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

$($.getJSON('https://www.flukso.net/api/sensor/c1411c6b4f9910bbbab09f145f8533b9?version=1.0&token=d8a8ab8893ea73f768b66b45234b5c3a&interval=week&resolution=15min&unit=watt&callback=?',
	function(data) {
		var formatPoint = function(point) {
			/* convert to ms timestamps */
			point[0] = point[0] * 1000;

			if (point[1] == 'nan') {
				point[1] = null
			};

			return point;
		};

		var series = {
			name: 'flukso sensor',
			data: data.map(formatPoint),
			step: true,
			tooltip: {
				yDecimals: 0
			}
		};
 
		chartConfig.series.push(series);

		window.chart = new Highcharts.StockChart(chartConfig);
	})
);

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

	return false; /* preventDefault and stopPropagation */
});