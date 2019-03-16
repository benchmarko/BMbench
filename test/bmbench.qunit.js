// bmbench.qunit.js - ...
//
/* globals QUnit */

"use strict";

var startBench;

if (typeof require !== "undefined") {
	global.bmBenchNoAutoStart = true;
	startBench = require("../bmbench.js"); // eslint-disable-line global-require
}

QUnit.module("BMbench", function (hooks) {
	hooks.beforeEach(function (/* assert */) {
		// var that = this; // eslint-disable-line no-invalid-this
	});

	QUnit.test("single benchmark run", function (assert) {
		var done = assert.async(),
			options = {
				bench1: 1,
				bench2: 1,
				caliMs: 201,
				fnDone: function () {
					var fRes = this.benchRes[1];

					assert.equal(fRes > 0, true, "benchmark 1: result > 0");
					done();
				}
			};

		startBench(options);
	});

	QUnit.test("benchmark runs", function (assert) {
		var done = assert.async(),
			options = {
				bench1: 0,
				bench2: 5,
				caliMs: 201,
				fnDone: function () {
					var i, fRes;

					for (i = 0; i <= 5; i++) {
						fRes = this.benchRes[i];
						assert.equal(fRes > 0, true, "benchmark " + i + ": result > 0");
					}
					done();
				}
			};

		startBench(options);
	});
});
// end
