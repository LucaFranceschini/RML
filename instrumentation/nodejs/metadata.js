class FunctionCallData {
	// avoid "this" keyword and "arguments" object
	// it's ok to use them as property keys
	constructor(functionName, args, dis) {
		if (typeof functionName !== 'string')
			throw new TypeError('function name is not a string');
		
		if (!Array.isArray(args))
			throw new TypeError('function call arguments is not an array');
		
		// "this" can be anything
		
		this.functionName = functionName;
		this.arguments = args;
		this.this = dis;
		this.event = 'func_pre';
	}
}

class PostFunctionCallData extends FunctionCallData {
	// pre-call data will already be available when constructing this
	// exceptions not supported, set result to undefined if any
	constructor(preCallData, result) {
		super(preCallData.functionName, preCallData.arguments, preCallData.this);
		this.result = result;
		this.event = 'func_post';
	}
}

exports.FunctionCallData = FunctionCallData;
exports.PostFunctionCallData = PostFunctionCallData;
