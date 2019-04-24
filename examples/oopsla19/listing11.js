// generator of correct traces for listing11test.rml: non-exclusive access to multiple resources

'use strict';

var trace_size = process.argv[2] || 100;
var max_resources = 100;
var resources = [];
var resId = void 0;

function getRandom(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function acquire(resId) {
    console.log('acquire(' + resId + ')');
    resources.push(resId);
    return resId;
}

function use(resId) {
    console.log('use(' + resId + ')');
}

function release(resId) {
    console.log('release(' + resId + ')');
    var removeIndex=resources.findIndex(function (id) {
        return id==resId;
    });
    resources.splice(removeIndex,1);
}

for (var event_counter = 1; event_counter <= trace_size; event_counter++) {
    var acq=resources.length==0?1:getRandom(2); // decide if next op is acquire
    resId = acq?getRandom(max_resources):resources[getRandom(resources.length)];
    if(acq)
	acquire(resId)
    else {
	var callUse=getRandom(3)<=1;
	callUse?use(resId):release(resId);
    }
}

while(resources.length)
    release(resources[resources.length-1]);   

