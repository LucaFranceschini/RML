// generator of correct traces for listing12.rml: exclusive access to multiple resources

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
    resId = getRandom(max_resources);
    if (resources.includes(resId)) 
        getRandom(2)?use(resId):release(resId);
    else
	acquire(resId);
}
while(resources.length)
    release(resources[resources.length-1]);   
