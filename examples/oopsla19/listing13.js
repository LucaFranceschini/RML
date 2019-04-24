// generator of correct traces for listing13.rml: quantitative monitoring with state variables

'use strict';

var trace_size = process.argv[2] || 100;
var total_res = 1000000;
var curr_res = total_res;
var used = Math.floor(total_res/trace_size);

function available() {
    console.log('available('+total_res+')');
    return total_res;
}

function use(total) {
    console.log('use('+total+','+used+')');
    curr_res -= used;
    return used;
}

available();
for (var event_counter = 1; event_counter <= trace_size; event_counter++) {
    use(curr_res,used);
}
