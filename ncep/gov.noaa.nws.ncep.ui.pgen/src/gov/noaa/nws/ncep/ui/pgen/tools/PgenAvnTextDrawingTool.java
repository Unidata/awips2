/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSinglePointDrawingTool
 * 
 * 2 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;

/**
 * Implements a modal map tool for PGEN single point drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/09					B. Yin   	Initial Creation.
 * 04/09            72      S. Gilbert  Modified to use PgenSession and PgenCommands
 * 04/09            88      J. Wu  		Added Text drawing
 * 04/24            99      G. Hull     Use NmapUiUtils
 * 04/09		   103		B. Yin		Extends from AbstractPgenTool
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory
 * 05/09			79		B. Yin		Extends from AbstractPgenDrawingTool
 * 07/09         !104       S. Gilbert  Modified from PgenSinglePointDrawingTool
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenAvnTextDrawingTool extends AbstractPgenDrawingTool {

    public PgenAvnTextDrawingTool(){
    	
    	super();
    	
    }

    /**
     * Returns the current mouse handler.
     * @return
     */
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenAvnTextDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
    
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
        
    public class PgenAvnTextDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	private DrawableElementFactory def = new DrawableElementFactory();
    	
    	/**
    	 * Current element.
    	 */
    	private AbstractDrawableComponent elem = null;
    	
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
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown  ) return false;

        	if ( button == 1 ) {
            	            	
            	// create an element.
        		elem = def.create(getDrawableType(), (IAttribute)attrDlg,
            			        pgenCategory, pgenType, loc, drawingLayer.getActiveLayer() );
          		
            	// add the product to the PGEN resource and repaint.
            	if ( elem != null ) {
            		drawingLayer.addElement( elem );
    	            mapEditor.refresh();
            		AttrSettings.getInstance().setSettings((DrawableElement)elem);
        	    }
            	
                return true;
                
            }
            else if ( button == 3 ) {
            	
            	//  The following methods are called when the tool is deactivated
            	//  and are not needed here
            	//drawingLayer.removeGhostLine();
    	        //mapEditor.refresh();
        		//attrDlg.close();
    	        PgenUtil.setSelectingMode();
    	        
            	return true;
            	
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
        	if ( !isResourceEditable() ) return false;

        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null ) return false;

        	if ( attrDlg != null ) {
        	    
        		AbstractDrawableComponent ghost = null;

        		ghost = def.create( getDrawableType(), (IAttribute)attrDlg,
        				pgenCategory, pgenType, loc, drawingLayer.getActiveLayer() );

        		drawingLayer.setGhostLine( ghost );      
        		mapEditor.refresh();
           
        	}
       	
        	return false;
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if ( shiftDown || !isResourceEditable()) return false;
			else return true;
		}

    }
    
    /**
     * Determine the proper DrawableType from the value of the pgenCategory attribute.
     * @return
     */
    private DrawableType getDrawableType() {
    	
    	if ( pgenType.equals("AVIATION_TEXT") )
    		return DrawableType.AVN_TEXT;
    	else
    		return DrawableType.MID_CLOUD_TEXT;
    }

}
