$(function() {
	$.getJSON('https://www.flukso.net/api/sensor/c1411c6b4f9910bbbab09f145f8533b9?version=1.0&token=d8a8ab8893ea73f768b66b45234b5c3a&interval=day&resolution=15min&unit=watt&callback=?', function(data) {

		var formatPoint = function(point) {
			point[0] = point[0] * 1000;

			if (point[1] == 'nan') {
				point[1] = null
			};

			return point;
		};

		Highcharts.setOptions({
			global: {
				useUTC: false
			}
		});

		// Create the chart
		window.chart = new Highcharts.StockChart({
			chart: {
				renderTo: 'chart',
				backgroundColor: '#f3f3f3',
				animation: {
					duration : 250,
					easing : 'swing'
				}
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
					lineWidth: 1
				}
			},

			rangeSelector: {
				enabled: false
				//selected: 1
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

			series: [{
				name: 'flukso sensor',
				data: data.map(formatPoint),
				step: true,
				tooltip: {
					yDecimals: 0
				}}]
		});
	});
});
