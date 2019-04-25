// generator of correct traces for listing16.rml: LIFO properties, multiple stacks with push, pop and size

'use strict';

var trace_size = process.argv[2] || 100;
var max_size = Math.floor(Math.sqrt(trace_size));
var max_stacks = max_size;
var max_elem = 1000;
var stacks = [];

// Stack objects
function Stack(){Array.apply(this,arguments)}
Object.setPrototypeOf(Stack.prototype,Array.prototype);
Stack.prototype.size=function(){return this.length}

function getRandom(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function create(id){
    if(id===undefined)
	id=stacks.length;
    mynew(id);
    mysize(id);
    return id;
}

function myfree(id){
    console.log('free('+id+')');
}

function mynew(id){
    stacks[id]=new Stack();
    console.log('new('+id+')');
    return id;
}

function dealloc(id){
    if(!(id in stacks))
	return;
    while(stacks[id].length)
	popsize(id);
    delete stacks[id];
    myfree(id);
}

function mypop(id) {
    var el=stacks[id].pop();
    console.log('pop(' + id + ',' + el + ')');
    return el;
}

function mypush(id,el) {
    console.log('push(' + id + ',' + el + ')');
    stacks[id].push(el);
}

function popsize(id) {
    mypop(id);
    mysize(id);
}

function pushsize(id,el) {
    mypush(id,el);
    mysize(id);
}

function mysize(id){
    var s=stacks[id].size();
    console.log('size(' + id + ',' + s +')');
    return s;
}

for (var event_counter = 1; event_counter <= trace_size; event_counter++) {
    if (stacks.length==0)
	create();
    else {
	var id=getRandom(max_stacks);
	if(stacks[id]===undefined)
	    create(id);
	else // push or pop or free
	{
	    var k=getRandom(max_size);
	    if(stacks[id][k]===undefined)
		pushsize(id,getRandom(max_elem));
	    else if(k<max_size-1)
		popsize(id);
	    else
		dealloc(id);
	}
    }
}
for(var i=0; i<stacks.length; i++)
    dealloc(i);

