/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenDeleteElement
 * 
 * 23 March 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * Implements a modal map tool for the PGEN selecting element function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/09					B. Yin   	Initial Creation.
 * 04/09            72      S. Gilbert  Modified to use PgenSession and PgenCommands
 * 04/09			103		B. Yin		Extends from AbstractPgenTool
 * 06/09			106		B. Yin		Use AbstractDrawableComponent
 * 03/10			n/a		M.Laryukhin	bug fix, null pointer exception when 
 * 										trying to delete a non-existing elememnt
 * 07/12			748		B. Yin		Delete Outlook line its label together
 * 03/13			927		B. Yin 		Moved out the mouse handler.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenDeleteElement extends AbstractPgenTool {
	
    /**
     * Input handler for mouse events.
     */ 
    protected PgenDeleteElementHandler deleteHandler = null;

    
    public PgenDeleteElement(){
    	
    	super();
    	
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
    	if ( this.deleteHandler == null || this.mapEditor != ((PgenDeleteElementHandler)deleteHandler).getMapEditor() 
    			|| this.drawingLayer != ((PgenDeleteElementHandler)deleteHandler).getPgenrsc() ) {     	        	
    		this.deleteHandler = new PgenDeleteElementHandler(this);

    	}
        
        return this.deleteHandler;
        
    }

}
