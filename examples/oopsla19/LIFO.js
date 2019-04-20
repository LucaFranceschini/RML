// generator of correct traces for listing12.rml: exclusive access to multiple resources

'use strict';

var trace_size = process.argv[2] || 100;
var max_size = 100;
var max_elem = 1000;
var stack = [];

function getRandom(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function mypop() {
    var el=stack.pop();
    console.log('pop(' + el + ')');
    return el;
}

function mypush(el) {
    console.log('push(' + el + ')');
    stack.push(el);
}

for (var event_counter = 1; event_counter <= trace_size; event_counter++) {
    if (stack.length==0)
	mypush(getRandom(max_elem));
    else if (stack.length==max_size)
	mypop();
    else
        getRandom(max_size)>=stack.length?mypush(getRandom(max_elem)):mypop();
}
while(stack.length)
    mypop();
