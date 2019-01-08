var fs = require('fs')
var MAX = 10000

function error(err) {
	if (err) {
		error.log(err.message)
		process.exit(1)
	}
}

fs.open('out.txt', 'w', function (err, fd) {
	error(err)
	// BUG! should wait for the callback to be executed before writing again
	for (var i = 1; i <= MAX; ++i)
		fs.write(fd, String(i), error)
	fs.close(fd, error)
})
