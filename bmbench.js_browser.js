//
// bmbench.js_browser.js
//

/* globals gState, startBench */

"use strict";


function fnLog(s) {
	gState.outputArea.value += s + "\n";
	if (typeof console !== "undefined") { // special care for IE
		console.log(s); // eslint-disable-line no-console
	}
}

function fnDone() {
	if (typeof console !== "undefined") { // special care for IE
		console.log("DEBUG: benchmark done"); // eslint-disable-line no-console
	}
}


function onStartButtonClick(frm) { // eslint-disable-line no-unused-vars
	var options = {
		bench1: Number(frm.bench1.value),
		bench2: Number(frm.bench2.value),
		n: Number(frm.n.value),
		outputArea: frm.outputArea,
		bWantStop: false,
		fnLog: fnLog,
		fnDone: fnDone
	};

	//frm.outputArea.value = "";
	return startBench(options);
}

function onStopButtonClick(frm) { // eslint-disable-line no-unused-vars
	gState.bWantStop = true;
}

function onClearButtonClick(frm) { // eslint-disable-line no-unused-vars
	frm.outputArea.value = "";
}


function onLoad() {
	if (typeof console !== "undefined") { // special care for IE
		console.log("DEBUG: onLoad"); // eslint-disable-line no-console
	}
}

window.onload = onLoad;

// end
