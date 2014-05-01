/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVectorDrawingTool
 * 
 * May 7th, 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.util.ArrayList;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Vector;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.VectorAttrDlg;

/**
 * Implements a modal map tool for PGEN vector drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09        #111		J. Wu  		Initial creation
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public class PgenVectorDrawingTool extends AbstractPgenDrawingTool {

    public PgenVectorDrawingTool(){
    	
    	super();
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {
    	
    	super.activateTool();
        
       	AbstractDrawableComponent attr = AttrSettings.getInstance().getSettings().get( pgenType );
   	    if ( attr == null ) {
   	    	( (VectorAttrDlg)attrDlg ).adjustAttrForDlg( pgenType );
   	    }      
        
    }

    /**
     * Returns the current mouse handler.
     * @return
     */
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenVectorDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
    
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
        
    public class PgenVectorDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * Points of the new element.
    	 */
        private ArrayList<Coordinate> points = new ArrayList<Coordinate>();
  
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
        	
        	if ( loc == null || shiftDown ) return false;

        	if ( button == 1 ) {
            	            	
            	if ( points.size() == 0 ) {
            		points.add( 0, loc );            	               	
            		
        			elem = (AbstractDrawableComponent) def.create( DrawableType.VECTOR, (IAttribute)attrDlg,
        			                   pgenCategory, pgenType, points.get(0),
        			                   drawingLayer.getActiveLayer());
           	    }
            	else {           		
           			
            		drawingLayer.removeElement( elem );

            		elem = def.create( DrawableType.VECTOR, (IAttribute)attrDlg,
            				           pgenCategory, pgenType, points.get(0),
            				           drawingLayer.getActiveLayer());
           		                		          		    
                   	double dir = ((Vector) elem).vectorDirection( points.get(0), loc );;
            		((Vector) elem).setDirection( dir );          	
                	((VectorAttrDlg)attrDlg).setDirection( ((Vector)elem).getDirection() );            			
           	
            	}
            	
            	// add the product to the PGEN resource and repaint.
            	if ( elem != null ) {
            		drawingLayer.addElement( elem );
    	            mapEditor.refresh();
            		AttrSettings.getInstance().setSettings((DrawableElement)elem);

        	    }
            	
                return true;
                
            }
            else if ( button == 3 ) {

            	drawingLayer.removeGhostLine();
	            mapEditor.refresh();
           	    
            	if ( points.size() > 0 ) {
            		
        		    points.clear();
        		          		
            	}
            	else {
         		    
            		attrDlg.close();
        		
    	            PgenUtil.setSelectingMode();           		          		
            	}
            	return true;
            	
            }
            else {
            	
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
        	Coordinate loc = mapEditor.translateClick( x, y );
        	if ( loc == null ) return false;

        	if ( attrDlg != null && points.size() != 0 ) {
        	    
            	AbstractDrawableComponent ghost = null;
           	
            	ghost = def.create( DrawableType.VECTOR, (IAttribute)attrDlg,
            						pgenCategory, pgenType, points.get(0),
            						drawingLayer.getActiveLayer());
            	
            	double dir = ((Vector)ghost).vectorDirection( points.get(0), loc );;

            	( (Vector)ghost ).setDirection( dir ); 
             	( (VectorAttrDlg)attrDlg ).setDirection( dir ); 
            	
            	drawingLayer.setGhostLine( ghost );      
          	    mapEditor.refresh();
        	}
       	
        	return false;
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if (!isResourceEditable() || shiftDown ) return false;
			else return true;
		}

    }
    
}
