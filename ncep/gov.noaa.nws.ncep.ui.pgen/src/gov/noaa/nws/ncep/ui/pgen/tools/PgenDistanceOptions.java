
/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenDistanceOption
 * 
 * FEBRUARY 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Implements a modal map tool for PGEN line Interpolation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/11			#318	S. Gilbert 	Created from PgenInterpTool
 * </pre>
 * 
 * @author	S. Gilbert
 */
public class PgenDistanceOptions extends AbstractPgenDrawingTool {
	
	
    /**
     * Constructor.  Sets current status to START and creates a symbol to use for verified elements
     */ 
    public PgenDistanceOptions(){
    	super();
    }
       
    /**
     * Creates and returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	

    	if ( this.mouseHandler == null ) {
    	
    		//  Use null handler - no map interaction needed.
    	    this.mouseHandler = new InputHandlerDefaultImpl();
    	
        }

        return this.mouseHandler;
        
   }


}


