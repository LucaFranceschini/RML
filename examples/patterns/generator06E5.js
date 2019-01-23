"use strict";

var max = process.argv[2] || 10;

var a = [];

function enq(elem) {
    a.push(elem);
    return elem;
}

function deq() {
    return a.shift();
}

for (var i = 1; i < max; i++) {
    if (a.length > 0 && Math.random() < .5) console.log("deq " + deq());else console.log("enq " + enq(Math.random()));
}

while(a.length>0)
    console.log(`deq ${deq()}`);

