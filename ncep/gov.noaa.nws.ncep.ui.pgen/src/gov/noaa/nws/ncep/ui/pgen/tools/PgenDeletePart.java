/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenDeletePart
 * 
 * 5 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.contours.ContourLine;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.display.CurveFitter;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.display.IMultiPoint;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;

import java.awt.Color;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;

/**
 * Implements a modal map tool for PGEN deleting part function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09		#79			B. Yin   	Initial Creation.
 * 12/09		#167		J. Wu   	Added contour lines.
 * 11/10		#332		B. Yin		Added cleanup() for handler
 * 11/17        #343        S. Gilbert  Delete where selected. not just vertices
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 06/12        TTR102		B. Yin		Make it work for Jet
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenDeletePart extends PgenSelectingTool {
	
    /**
     * Input handler for mouse events.
     */ 
    protected IInputHandler delPartHandler = null;

    
    public PgenDeletePart(){
    	
    	super();
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
    	
    	if ( delPartHandler != null ) ((PgenDelPartHandler) delPartHandler).cleanup();

    	super.deactivateTool();
        
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.delPartHandler == null ) {
        	
        	this.delPartHandler = new PgenDelPartHandler();
        	
        }

        return this.delPartHandler;
        
    }
        
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
    public class PgenDelPartHandler extends PgenSelectingTool.PgenSelectHandler {
   	
    	private Symbol DOT = new Symbol(null, new Color[]{Color.RED}, 1.0f, 7.5, false, null, "Marker", "DOT");
    	OperationFilter delPartFilter = new OperationFilter( Operation.DELETE_PART );
    	
    	/**
    	 * Index of the selected points.
    	 */
    	double TOL = 10.;
     	int	pt1Index = 0;
     	int pt2Index = 0;
     	Coordinate point1 = null;
     	Coordinate point2 = null;
     	LocationIndexedLine lil = null;
     	GeometryFactory gf = new GeometryFactory();
     	
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
        	if ( loc == null ) return false;
        	Coordinate screenPt = new Coordinate(anX, aY);
        	
        	if ( button == 1 ) {

        		if ( drawingLayer.getSelectedDE() == null ){ 
        		        	
        			// Get the nearest element and set it as the selected element.
        			DrawableElement elSelected = drawingLayer.getNearestElement( loc, delPartFilter );
        			if ( elSelected instanceof ILine  &&
        				 !(elSelected instanceof WatchBox)) {
        				
        				drawingLayer.setSelected( elSelected );
        				
        				// create a LocationIndexedLine for the selected line 
        				// in screen coordinates for future queries
        				
        				Coordinate[] coords = preprocessLine(((ILine)elSelected).getSmoothFactor(), 
        						((ILine)elSelected).isClosedLine(), ((IMultiPoint)elSelected).getLinePoints() );
        				LineString	ls = gf.createLineString( coords );
        		 		lil = new LocationIndexedLine(ls);
        			}
        			else { 
        				return false;
        			}
        		}
        		else if ( !ptSelected ) {
        			
        			//select first point
        			
        			MultiPointElement des = (MultiPointElement)drawingLayer.getSelectedDE();
        			if ( des.getParent() instanceof ContourLine ) {
        				pt1Index = getNearestPtIndex((MultiPointElement)drawingLayer.getSelectedDE(), loc); 
        				drawingLayer.addPtSelected( pt1Index );
        			}
        			else {
        				/*
        				 * if clicked close enough to line, save this first point
        				 */
        				LinearLocation linloc1 = lil.project(screenPt);
        				Coordinate screen1 = lil.extractPoint(linloc1);
        				if ( screenPt.distance(screen1) > TOL ) {    // not close enough to line
        					point1 = null;
        					return false;
        				}
        				point1 = mapEditor.translateClick( screen1.x, screen1.y );
        				DOT.setLocation(point1);
        				drawingLayer.setGhostLine(DOT);
        			}
        			ptSelected = true;
        			
        		}
        		else {

        			//select second point
        			Line des = (Line)drawingLayer.getSelectedDE();
            		
            		if ( des.getParent() instanceof ContourLine ) {
            			pt2Index = getNearestPtIndex( des, loc); 
                		drawingLayer.addPtSelected( pt2Index );
                		
                		//remove part between pt1 and pt2
                		int start = Math.min( pt1Index, pt2Index );
                		int end =  Math.max( pt1Index, pt2Index );
            			Contours oldContours = (Contours)des.getParent().getParent();
            			Contours newContours = oldContours.split( (ContourLine)des.getParent(), start, end);
            			            			
            			drawingLayer.replaceElement( oldContours, newContours );
            		}
            		else {
            			/*
            			 * if clicked close enough to line, save this 2nd point, and delete
            			 * proper segment
            			 */
        				LinearLocation linloc2 = lil.project(screenPt);
        				Coordinate screen2 = lil.extractPoint(linloc2);
        				if ( screenPt.distance(screen2) > TOL ) {    // not close enough to line
        					point2 = null;
        					return false;
        				}
        				point2 = mapEditor.translateClick( screen2.x, screen2.y );
               		    drawingLayer.deleteElementPart( des, point1 , point2 );
            		}

            		drawingLayer.removeGhostLine();
            		drawingLayer.removeSelected();
            		lil = null;
            		ptSelected = false;
            			
        		}
        		
     	        mapEditor.refresh();  
                return true;
                
            }
            else if ( button == 3 ) {
            	
            	// reset
        		ptSelected = false;
        		drawingLayer.removeGhostLine();
            	drawingLayer.removeSelected();
            	lil = null;
      	        mapEditor.refresh();
      	        PgenUtil.setSelectingMode();
      	        
            	return true;
            	
            }
            else{
            	
               	return false;
               	
            }
        	
        }
        
		/*
         * overrides the function in selecting tool
         */
        @Override
        public boolean handleMouseDownMove(int anX, int aY, int button){
        	 return false;
        }
        
        /*
         * overrides the function in selecting tool
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button){
        	return false;
        }

        /**
         * creates an array of coordinates representing a line in screen coordinates
         * after closing and smoothing (if necessary) a line given in lat/lon coordinates
         */
        private Coordinate[] preprocessLine(int smoothFactor, boolean isClosed,
				Coordinate[] linePoints) {
        	double[][] newpts;
        	
    		int num = linePoints.length;
    		if ( isClosed ) num++;
    		double[][] dpts = new double[num][3];
    		
    		for (int k=0; k<linePoints.length; k++ ) {
    			dpts[k] = mapEditor.translateInverseClick(linePoints[k]);
    		}

    		if (isClosed) dpts[num-1] = dpts[0];
    		
    		if ( smoothFactor != 0 ) {
    			newpts = CurveFitter.fitParametricCurve(dpts, 10.0f);
    		}
    		else {
    			newpts = dpts;
    		}
    		
    		Coordinate[] coords = new Coordinate[ newpts.length ];
    		
    		for (int k=0; k<newpts.length; k++) {
    			coords[k] = new Coordinate( newpts[k][0], newpts[k][1]);
    		}
    		
			return coords;
		}

        
        private void cleanup(){
        	ptSelected = false;
        	drawingLayer.removeSelected();
        }
        
    }

}
	
