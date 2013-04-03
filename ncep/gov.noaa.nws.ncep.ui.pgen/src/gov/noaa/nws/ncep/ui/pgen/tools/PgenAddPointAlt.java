/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenAddPoint
 * 
 * 1 June 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.ILine;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaReducePoint;

/**
 * Implements a modal map tool for PGEN add point function.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/10			282		S. Gilbert  Initial Creation.
 * 04/11			?		B. Yin		Re-factor IAttribute
 * 05/11			#808	J. Wu		Update Gfa vor text
 * 05/12		    #610	J. Wu   	Add warning when GFA FROM lines > 3
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenAddPointAlt extends AbstractPgenTool {
	
	/**
	 * Possible states to be in when adding a point to an element
	 * @author sgilbert
	 */
	private enum ADD_STATUS { START, SELECTED, MOVING };
	
	
    /**
     * Input handler for mouse events.
     */ 
    protected IInputHandler addPtHandler = null;

    
    public PgenAddPointAlt(){
    	
    	super();
    	
    }
    
    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.addPtHandler == null ) {
        	
        	this.addPtHandler = new PgenAddPtHandler();
        	
        }

        return this.addPtHandler;
        
    }
        
    /**
     * Implements input handler for mouse events.
     * @author sgilbert
     *
     */
    public class PgenAddPtHandler extends InputHandlerDefaultImpl {
   	
    	private boolean preempt;
    	private ADD_STATUS status = ADD_STATUS.START;
    	private DrawableElement newEl;
    	Map<Coordinate, Integer> newPoints = null;
    	private Integer index = null;
    	private DECollection ghost = new DECollection();
    	Color ghostColor = new Color( 255,255,255);
    	private Symbol marker =  new Symbol(null, new Color[] { new Color(255,0,0) }, 2.5f, 1.0, 
    			false, null, "Marker", "BOX" );
    	
    	OperationFilter addPointFilter = new OperationFilter( Operation.ADD_POINT );
    	
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
        	if ( loc == null || shiftDown ) return false;
        	
        	if ( button == 1 ) {

        		switch (status) {
        		
        		case START:
        			/*
        			 * User selects an Element to alter
        			 */
        			//AbstractDrawableComponent elSelected = drawingLayer.getNearestComponent( loc, addPointFilter, true );
        			AbstractDrawableComponent elSelected = drawingLayer.getNearestElement( loc, addPointFilter);

        			if ( elSelected == null ) return false;
        			drawingLayer.setSelected( elSelected );

        			/*
        			 * virtual points are calculated for each segment and displayed as ghosts.
        			 */
        			newPoints = calculateNewPoints(drawingLayer.getSelectedDE());
        			ghost.clear();
        			for ( Coordinate coord : newPoints.keySet() ) {
        				Symbol sym = new Symbol(null, new Color[] { new Color(255,0,0) }, 2.5f, 1.0, 
        		    			false, coord, "Marker", "BOX" );
        				ghost.add(sym);
        			}
        			drawingLayer.setGhostLine(ghost);

        			status = ADD_STATUS.SELECTED;
        			break;

        		case SELECTED:
        			/*
        			 * Check to see if user has selected a virtual point for dragging...
        			 */
        			index = null;
        			if ( newPoints != null ) {
        				for ( Coordinate coord : newPoints.keySet() ) {
        					if ( loc.distance( coord ) < 0.2 ) {
        						index = newPoints.get(coord);
        						status = ADD_STATUS.MOVING;
        						preempt = true;
        						break;
        					}
        				}
        			}
        			break;
        		}
        		
     	        mapEditor.refresh();  
                return preempt;
                
            }
            else if ( button == 3 ) {
            	
        		switch (status) {
        		
        		case SELECTED:
        			/*
        			 * start over
        			 */
        			drawingLayer.removeGhostLine();
        			drawingLayer.removeSelected();
        			status = ADD_STATUS.START;
        			mapEditor.refresh();

        			break;
        		default:
        			PgenUtil.setSelectingMode();
        		}
      	        
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
        	
        	if ( !isResourceEditable()|| button != 1 || shiftDown ) return false;
        	
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	
    		switch (status) {
    		
    		case MOVING:
    			/*
    			 * ghost current marker location
    			 */
    			ghost.clear();
    			marker.setLocation(loc);
    			ghost.add(marker);

    			/*
    			 * ghost line with new point added
    			 */
    			newEl = addPointToElement( loc, index, drawingLayer.getSelectedDE() );
    			newEl.setColors(new Color[]{ghostColor, ghostColor});
    			ghost.add(newEl);
    			drawingLayer.setGhostLine(ghost);
    			break;
    		}
    		
    		mapEditor.refresh();
        	return preempt;
        }  
        
        /*
         * overrides the function in selecting tool
         */
        @Override
    	public boolean handleMouseUp(int anX, int aY, int button) {
        	
        	if (  !isResourceEditable() || button != 1 ) return false;
        	
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	
    		switch (status) {
    		
    		case MOVING:
    			/*
    			 * mouse up.  this is location of new point.  Create new element with new point
    			 * added. and replace the old element in the resource with the new one.
    			 */
    			drawingLayer.removeGhostLine();
    			AbstractDrawableComponent newComp = addPointToElement( loc, index, drawingLayer.getSelectedComp() );
        		drawingLayer.replaceElement(drawingLayer.getSelectedComp(), newComp);
        		
    			if ( newComp instanceof Gfa ) {
    				if( ((Gfa)newComp).getGfaFcstHr().indexOf("-") > -1 ){
    					// snap
    					((Gfa)newComp).snap();
	    				GfaReducePoint.WarningForOverThreeLines( (Gfa)newComp );           		    				
    				}

    				((Gfa)newComp).setGfaVorText( Gfa.buildVorText( (Gfa)newComp ) );
    			}

        		drawingLayer.removeSelected();
    			status = ADD_STATUS.START;
    			refresh();
    			break;
    		}
    		
    		return false;
    	}

    }

    /*
     * Create a new Drawable Element from the given DE with the new point added to the given segment.
     */
	public DrawableElement addPointToElement(Coordinate point, Integer index, DrawableElement selectedDE) {
		
 		DrawableElement newEl = (DrawableElement)selectedDE.copy();
 		
 		ArrayList<Coordinate> pts = new ArrayList<Coordinate>( selectedDE.getPoints() );
 		pts.add(index, point);
 		
		newEl.setPoints( pts );
		return newEl;
		
	}
	
    /*
     * Create a new Drawable Element from the given DE with the new point added to the given segment.
     * Note that Jet does not extend from DrawableElement and must be treated differently
     */
	public AbstractDrawableComponent addPointToElement(Coordinate point, Integer index, AbstractDrawableComponent elem) {
		
		if ( elem instanceof DrawableElement ) {
			DrawableElement de = (DrawableElement)elem;
			return addPointToElement( point, index, de );
		}
		else if ( elem instanceof Jet ) {
			Jet newEl = ((Jet)elem).copy();

			ArrayList<Coordinate> pts = new ArrayList<Coordinate>( elem.getPrimaryDE().getPoints() );
			pts.add(index, point);

			newEl.getPrimaryDE().setPoints( pts );
			return newEl;
		}
		return elem;
		
	}
	
	/*
	 * Returns a map of "virtual" Coordinates along with their segment index
	 * representing the midpoints (and maybe endpoints) for each segment of the given
	 * DrawableElement. 
	 */
	private Map<Coordinate, Integer> calculateNewPoints(DrawableElement elSelected) {
		
		Map<Coordinate, Integer> newLocs = new HashMap<Coordinate, Integer>();
		List<Coordinate> points = new ArrayList<Coordinate>();

		/*
		 * Convert Lat/Lon to pixel coordinates
		 */
		for ( Coordinate c : elSelected.getPoints() ) {
			double[] tmp = mapEditor.translateInverseClick(c);
			points.add( new Coordinate( tmp[0], tmp[1] ) );
		}

		/*
		 * calculate midpoints
		 */
		for (int i=0; i<elSelected.getPoints().size()-1; i++ ) {
			LineSegment ls = new LineSegment( points.get(i), points.get(i+1));
			newLocs.put( toLatLon(ls.midPoint()), i+1 );
		}
		
		if ( ! ((ILine)elSelected).isClosedLine() ) {
			//Coordinate prev = new Coordinate( points.get(0).x - 1*Math.signum(points.get(0).x - points.get(1).x),
			//								  points.get(0).y + 1*Math.signum(points.get(0).y - points.get(1).y) );
			Coordinate prev = points.get(0);
			newLocs.put(toLatLon(prev), 0);

			//Coordinate next = new Coordinate( points.get(points.size()-1).x - 1*Math.signum(points.get(points.size()-1).x - points.get(points.size()-2).x),
			//		  						  points.get(points.size()-1).y + 1*Math.signum(points.get(points.size()-1).y - points.get(points.size()-2).y) );
			Coordinate next = points.get(points.size()-1);
			newLocs.put(toLatLon(next), points.size());
		}
		else {
			LineSegment ls = new LineSegment( points.get(0), points.get( points.size()-1 ) );
			newLocs.put( toLatLon(ls.midPoint()), points.size() );
		}
		
		return newLocs;
	}

	/*
	 * Convert pixel coordinate to Lat/Lon coordinate
	 */
	private Coordinate toLatLon(Coordinate pixel) {
		return mapEditor.translateClick( pixel.x, pixel.y );
	}

}
	
