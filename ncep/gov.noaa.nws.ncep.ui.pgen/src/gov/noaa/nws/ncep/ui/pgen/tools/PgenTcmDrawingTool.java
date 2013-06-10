/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenTCMDrawingTool
 * 
 * 6 September 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TcmAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.Tcm;

import java.util.ArrayList;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a modal map tool for PGEN TCM drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/11		?			B. Yin   	Initial Creation for TCM
 * 12/11		565			B. Yin		change return values for mouse handlers 
 *										in order for panning to work correctly
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenTcmDrawingTool extends AbstractPgenDrawingTool {
	
    public PgenTcmDrawingTool(){
    	
    	super();
    	
    }

    /*
     * Invoked by the CommandService when starting this tool
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {
    	
    	super.activateTool();
    	
    	DrawableElement elem = null;
    	if ( event.getTrigger() instanceof Tcm ) elem = (Tcm)event.getTrigger(); 
    		
    	if ( attrDlg instanceof TcmAttrDlg ) {
    		if ( elem != null ) attrDlg.setAttrForDlg( elem );
    		((TcmAttrDlg)attrDlg).setTcm((Tcm)elem);
    	}
    	
    	return;
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenTCMDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
   
    
    /**
     * Implements input handler for mouse events.
     * @author jun
     *
     */
          
    public class PgenTCMDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * Points of the new element.
    	 */
        private ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        
       	/**
    	 * Current element.
    	 */     
        private AbstractDrawableComponent elem;
        
       	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	private DrawableElementFactory def = new DrawableElementFactory();

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	
        public boolean handleMouseDown(int anX, int aY, int button) {
        	if ( !isResourceEditable() ) return false;

        	//  Check if mouse is in geographic extent
        	//Coordinate loc = mapEditor.translateClick(anX, aY);
        	
        	if ( button == 1 ) { 
               
                return false;
                
            }
            else if ( button == 3 ) {          
            	
            	drawingLayer.removeGhostLine();   
    	        mapEditor.refresh();
          		          	
            	if ( points.size() == 0 ) {
            		
            		PgenUtil.setSelectingMode();
           		
            	}
            	else {            		
                    
            		points.clear();
       	        
            	}
            	
            	return true;
            	
            }
        	else if ( button == 2 ){
        		
        		return false;
        		
        	}
            else{
            	
               	return false;
               	
            }
        	
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseMove(int,
         *      int)
         */
        @Override
        public boolean handleMouseMove(int x, int y) {
       	
           	return false;
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			return false;
		}

    }

}
