/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenDeleteObj
 * 
 * 20 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Implements PGEN "Delete Obj" function. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09			79		B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenDeleteObj extends AbstractPgenTool {
	
    public PgenDeleteObj(){
    	
    	super();
    	
    }

	@Override
	public IInputHandler getMouseHandler() {
		return null;             // no interaction
	}
    
    
}