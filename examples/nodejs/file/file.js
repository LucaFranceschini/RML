var fs = require('fs')
var MAX = 10000

function error(err) {
	if (err) {
		error.log(err.message)
		process.exit(1)
	}
}

// this correctly waits for the callback to be executed before writing again
function write(fd, num) {
	if (num <= MAX)
		fs.write(fd, String(num), function (err) {
			error(err)
			write(fd, num+1)
		})
	else
		fs.close(fd, error)
} 

fs.open('out.txt', 'w', function (err, fd) {
	error(err)
	write(fd, 1)
})
