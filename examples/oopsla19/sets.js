// generator of correct traces for setstest.rml: sets as extension of exclusive access to multiple resources

'use strict';

var trace_size = process.argv[2] || 100;
var max_set_size = 100;
var max_elem = 100;
var set=new Set();
		 
function getRandom(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function insert(el) {
    var res=!set.has(el);
    console.log('insert(' + el + ',' + res + ')');
    set.add(el);
    return res;
}

function remove(el) {
    var res=set.delete(el);
    console.log('remove(' + el + ',' + res + ')');
    return res;
}

for (var event_counter = 1; event_counter <= trace_size; event_counter++) {
    var elem=getRandom(max_elem);
    getRandom(max_set_size)>=set.size?insert(elem):remove(elem);
}
var it=set.values();
while(true){
    var n=it.next();
    if(n.done)
	break;
    remove(n.value);
}



