// generator of correct traces for listing12.rml: exclusive access to multiple resources

'use strict';

var trace_size = process.argv[2] || 100;
var max_size = 100;
var max_elem = 1000;
var queue = [];

function getRandom(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function dequeue() {
    var el=queue.pop();
    console.log('dequeue(' + el + ')');
    return el;
}

function enqueue(el) {
    console.log('enqueue(' + el + ')');
    queue.unshift(el);
}

for (var event_counter = 1; event_counter <= trace_size; event_counter++) {
    if (queue.length==0)
	enqueue(getRandom(max_elem));
    else if (queue.length==max_size)
	dequeue();
    else
        getRandom(max_size)>=queue.length?enqueue(getRandom(max_elem)):dequeue();
}
while(queue.length)
    dequeue();
