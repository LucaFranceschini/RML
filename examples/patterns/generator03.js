// generator of correct traces for spec03.pl: exclusive access to multiple resources

'use strict';

const trace_size = process.argv[2] || 100;
const max_resources = 100; // to keep low, although the benchmark worked with max_resources==trace_size==10000
let resources=[];
let resId;

function getRandom(max) {
  return Math.floor(Math.random() * Math.floor(max));
}

function acquire(resId){
    console.log(`acquire(${resId})`);
    resources.push(resId);
    return resId;
}

function use(resId){
    console.log(`use(${resId})`);
}

function release(resId){
    console.log(`release(${resId})`);
    resources=resources.filter(id => id !== resId);
}

    
for(let event_counter=1;event_counter<=trace_size;event_counter++){
    resId=getRandom(max_resources);
    if(resources.includes(resId)){
	if(getRandom(2))
	    use(resId);
	else
	    release(resId);
    }
    else
	acquire(resId);
}

while(resources.length)
    release(resources.pop());


