// this program write numbers from 0 to "max" to a file

const fs = require('fs');
const os = require('os');

const path = os.tmpdir() + '/oneFileTest';
const mode = 'w'; // fails if path exists
const max = 10;

fs.open(path, mode, function opencb(err, fd) {
	if (err) console.error(err);
	else myWrite(0, fd);
});

// write given number to file descriptor
function myWrite(num, fd) {
	if (num < max)
		fs.write(fd, num, function writecb(err) {
			if (err)
				console.error(err);
			
			myWrite(num + 1, fd);
		});
}
