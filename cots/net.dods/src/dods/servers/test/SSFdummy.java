package dods.servers.test;

import java.util.*;
import dods.dap.Server.*;

public class SSFdummy 
    implements BoolFunction {
    
    public String getName() {
	return "dummy";
    }

    public void checkArgs(List args) {
    }

    public boolean evaluate(List args) {
	return args.size() > 2;
    }
}
