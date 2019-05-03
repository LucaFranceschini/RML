// generator of correct traces for listing18.rml: FIFO properties, single queue with enqueue, dequeue and size

'use strict';

var trace_size = process.argv[2] || 100;
var max_size = Math.floor(trace_size/10);
var max_elem = 1000;
var queue = [];

function getRandom(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function size(){
    var s=queue.length;
    console.log('size returns '+s);
    return s;
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
    size();
}
while(queue.length){
    dequeue();
    size();
}
