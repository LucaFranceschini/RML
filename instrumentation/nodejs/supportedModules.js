const modules = require('module');

module.exports = modules.builtinModules.filter(m =>
	// exclude v8 and node-inspect modules
	// not very useful for specification and many are deprecated
	!m.startsWith('v8') &&
	!m.startsWith('node-inspect') &&
	// sys is deprecated
	m !== 'sys' &&
	// crypto has deprecated members, discard it for now
	m !== 'crypto'
);
