// DO NOT INSTRUMENT
(function (sandbox) {
	function FunctionCallAnalysis() {
		const fs = require('fs');
		const util = require('util');
		
		const { FunctionCallData, PostFunctionCallData } = require('./metadata.js');
		const stringify = require('./stringify.js');
		
		// skip the first two function calls observed by functionEnter/Exit
		// https://github.com/Haiyang-Sun/nodeprof.js/issues/16
		// double it to easily handle both functionEnter and functionExit
		let toSkip = 2 * 2;

		const logFileFd = fs.openSync(J$.initParams.logFile, 'w');
		
		// not all information available on functionEnter are passed to functionExit
		// always push on functionEnter and pop on functionExit
		const callStack = [];
		
		// avoid loops between callbacks here and bultinEnter/Exit
		let avoidBuiltins = false;
		
		function logCall(callData) {
			fs.writeSync(logFileFd, stringify(callData) + '\n');
		}
		
		this.functionEnter = function (iid, f, dis, args) {
			if (toSkip > 0) {
				--toSkip;
				return;
			}
			
			const callData = new FunctionCallData(f.name, args, dis);
			callStack.push(callData);
			logCall(callData);
		}

		this.functionExit = function (iid, returnVal, wrappedExceptionVal) {
			if (toSkip > 0) {
				--toSkip;
				return;
			}
			
			const preCallData = callStack.pop();
			const callData = new PostFunctionCallData(preCallData, returnVal);
			logCall(callData);
		}
		
		this.builtinEnter = function (name, f, dis, args) {
			if (avoidBuiltins) return;
			
			avoidBuiltins = true;
			const callData = new FunctionCallData(name, args, dis);
			logCall(callData);
			avoidBuiltins = false;
		};

		this.builtinExit = function (name, f, dis, args, returnVal, exception) {
			if (avoidBuiltins) return;
			
			avoidBuiltins = true;
			const preCallData = new FunctionCallData(name, args, dis);
			const callData = new PostFunctionCallData(preCallData, returnVal);
			logCall(callData);
			avoidBuiltins = false;
		};
		
		this.endExecution = function () {
			// don't log builtin calls in this callback
			avoidBuiltins = true;
			
			fs.closeSync(logFileFd);
		}
	}

	sandbox.addAnalysis(new FunctionCallAnalysis());
})(J$)
