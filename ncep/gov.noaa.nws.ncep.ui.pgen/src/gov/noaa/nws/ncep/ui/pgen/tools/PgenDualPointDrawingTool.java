/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenDualPointDrawingTool
 * 
 * 29 April 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.ArrayList;


import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.ArcAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.*;

/**
 * Implements a modal map tool for PGEN dual points drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/09		89			J. Wu   	Initial Creation for Arc
 * 04/09		103			B. Yin		Extends from AbstractPgenTool
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory
 * 05/09		79			B. Yin		Extends from AbstractPgenDrawingTool 
 * 02/12		?			J. Wu   	Ensure starting angle is smaller than ending angle
 *
 * </pre>
 * 
 * @author	J. Wu
 */

public class PgenDualPointDrawingTool extends AbstractPgenDrawingTool {
	
    public PgenDualPointDrawingTool(){
    	
    	super();
    	
    }

    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenDualPointDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
   
    
    /**
     * Implements input handler for mouse events.
     * @author jun
     *
     */
          
    public class PgenDualPointDrawingHandler extends InputHandlerDefaultImpl {
    	
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
        	Coordinate loc = mapEditor.translateClick(anX, aY);
			if (loc == null || shiftDown )	return false;

        	if ( button == 1 ) { 
                
        		if ( points.size() == 0 ) {
        			
                    points.add( 0, loc );   
                    
       		    }
        		else {
        			
        			if ( !validArcAngle() ) points.clear();
        			
        			if ( points.size() > 1 ) points.remove( 1 );
        			points.add( 1, loc );

        			// create a new DrawableElement.    
        			elem = def.create( DrawableType.ARC, (IAttribute)attrDlg,
        					pgenCategory, pgenType, points, drawingLayer.getActiveLayer() );

        			// add the product to PGEN resource
        			drawingLayer.addElement( elem );

        			drawingLayer.removeGhostLine();
        			points.clear();

        			mapEditor.refresh();
        			AttrSettings.getInstance().setSettings((DrawableElement)elem);
            		
        		}
      		
                return true;
                
            }
            else if ( button == 3 ) {          
            	
            	drawingLayer.removeGhostLine();   
    	        mapEditor.refresh();
          		          	
            	if ( points.size() == 0 ) {
            		
//            		NmapUiUtils.setPanningMode();
            		PgenUtil.setSelectingMode();
           		
            	}
            	else {            		
                    
            		points.clear();
       	        
            	}
            	
            	return true;
            	
            }
        	else if ( button == 2 ){
        		
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

        	// create the ghost element and put it in the drawing layer
           	AbstractDrawableComponent ghost = null;
           	
           	if ( !validArcAngle() ) points.clear();
           	
            if ( points != null && points.size() >= 1) {
                
            	ghost = def.create( DrawableType.ARC, (IAttribute)attrDlg,
            			            pgenCategory, pgenType, points,
            			            drawingLayer.getActiveLayer());
                          	    
                ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>();
                
                ghostPts.add( 0, points.get(0 ) );
       			ghostPts.add( 1, loc );
               
                Arc arc = (Arc)ghost;
            	arc.setLinePoints( new ArrayList<Coordinate>( ghostPts ) );
                          	
            	drawingLayer.setGhostLine( ghost );
            	
            	mapEditor.refresh();
           	
            }
            
            
        	return false;
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if (  !isResourceEditable() || shiftDown ) return false;
			else return true;
		}
		
		
		/*
		 * Check if the given starting angle is less than the ending angle
		 */
		private boolean validArcAngle() {
			
			boolean isValid = false;
			
			if ( attrDlg != null ) {
           		
           		double sa = ((ArcAttrDlg)attrDlg).getStartAngle();
           		double ea = ((ArcAttrDlg)attrDlg).getEndAngle();
           		
           		if ( sa < ea )  isValid = true;			 
			}
			
		    return isValid;
		}

    }

}
