package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

import java.io.DataInput;
import java.io.IOException;

/**
 * Class=0, Element=23
 * @version $Id:  $ 
 * @author  xphc
 * @since Oct 12, 2010
 */
public class EndApplicationStructure extends Command {

	//ORIGINAL// EndApplicationStructure(int ec, int eid, int l, DataInput in)
	public EndApplicationStructure(int ec, int eid, int l, DataInput in)
	throws IOException {
		super(ec, eid, l, in);
		// no arguments

		// make sure all the arguments were read
		assert (this.currentArg == this.args.length);
	}

	@Override
	public String toString() {
		return "EndApplicationStructure";
	}

}
