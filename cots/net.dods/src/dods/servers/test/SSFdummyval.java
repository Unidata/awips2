package dods.servers.test;

import java.util.*;
import dods.dap.*;
import dods.dap.Server.*;

public class SSFdummyval
    implements BTFunction {
    
    public String getName() {
	return "dummyval";
    }
    
    public void checkArgs(List args)
	throws InvalidParameterException {

	if (args.size() < 1) {
	    throw new InvalidParameterException("must have at least 1 param.");
	}
    }

    public BaseType getReturnType(List args) {
	return ((SubClause)args.get(0)).getValue();
    }

    public BaseType evaluate(List args) 
	throws SDODSException {

	return ((SubClause)args.get(0)).evaluate();
    }
}
