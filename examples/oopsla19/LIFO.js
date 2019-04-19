// generator of correct traces for listing12.rml: exclusive access to multiple resources

'use strict';

var trace_size = process.argv[2] || 100;
var max_size = 100;
var max_elem = 1000;
var stack = [];

function getRandom(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function pop() {
    var el=stack.pop();
    console.log('pop(' + el + ')');
    return el;
}

function push(el) {
    console.log('push(' + el + ')');
    stack.push(el);
}

for (var event_counter = 1; event_counter <= trace_size; event_counter++) {
    resId = getRandom(max_size);
    if (stack.length==0)
	push(getRandom(max_elem));
    else if (stack.length==max_size)
	pop();
    else
        getRandom(2)?use(resId):release(resId);
    else
	acquire(resId);
}
while(stack.length)
    release(stack[stack.length-1]);   
