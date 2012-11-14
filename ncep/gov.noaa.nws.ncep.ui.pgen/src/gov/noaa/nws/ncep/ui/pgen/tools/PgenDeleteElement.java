/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenDeleteElement
 * 
 * 23 March 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.filter.AcceptFilter;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

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
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenDeleteElement extends AbstractPgenTool {
	
    /**
     * Input handler for mouse events.
     */ 
    protected IInputHandler deleteHandler = null;

    
    public PgenDeleteElement(){
    	
    	super();
    	
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.deleteHandler == null ) {
        	
        	this.deleteHandler = new PgenDeleteHandler();
        	
        }
        
        return this.deleteHandler;
        
    }
        
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
    public class PgenDeleteHandler extends InputHandlerDefaultImpl {
    	
    	private boolean preempt;
    	
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	   	
        public boolean handleMouseDown(int anX, int aY, int button) {
        	if ( !isResourceEditable() ) return false;
        	
        	preempt = false;
        	
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	
        	if ( button == 1 ) {

        		if ( drawingLayer.getSelectedComp() != null ) {
        			// Remove the select element from PGEN Resource
        			drawingLayer.removeElement(drawingLayer.getSelectedComp());
        			// de-select element
                	drawingLayer.removeSelected();
                	preempt = false;
        		}
        		else {        	
        			// Get the nearest element and set it as the selected element.
        			AbstractDrawableComponent elSelected = drawingLayer.getNearestComponent( loc, new AcceptFilter(), true );
        			
        			//Delete watch status line
        			if ( elSelected instanceof DECollection && elSelected.getName().equalsIgnoreCase("Watch")
        					&& drawingLayer.getNearestElement(loc).getPgenType().equalsIgnoreCase("POINTED_ARROW")){
        				elSelected =drawingLayer.getNearestElement(loc);
        			}	
        			else if ( elSelected instanceof Outlook && ((Outlook)elSelected).getDEs() > 1){
        				AbstractDrawableComponent adc = drawingLayer.getNearestElement(loc);
        				elSelected = adc.getParent(); 
        			}
        			
        			if (elSelected != null) {
        				drawingLayer.setSelected( elSelected );
        				preempt = true;
        			}
        		}
                
     	        mapEditor.refresh();  
                return preempt;
                
            }
        	else if ( button == 2 ){
        		
        		return true;
        		
        	}
            else if ( button == 3 ) {
            	
            	if (  drawingLayer.getSelectedComp() != null ){
            		// de-select element
            		drawingLayer.removeSelected();
            		mapEditor.refresh();
            	}
            	else {
            		// set selecting mode
            		PgenUtil.setSelectingMode();
            	}
            	
            	return true;
            	
            }
            else{
            	
               	return true;
               	
            }
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        	if (  !isResourceEditable() || shiftDown ) return false;
        	else return true;
		}
        

    }

}
