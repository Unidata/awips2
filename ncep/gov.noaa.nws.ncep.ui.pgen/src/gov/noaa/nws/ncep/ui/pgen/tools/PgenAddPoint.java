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
import java.util.List;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
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
 * 06/10		#282			S. Gilbert  Initial Creation.
 * 05/11		#808		J. Wu		Update Gfa vor text
 * 05/12		#610		J. Wu   	Add warning when GFA FROM lines > 3
 *
 * </pre>
 * 
 * @author	B. Yin
 */

//public class PgenAddPoint extends PgenSelectingTool {
public class PgenAddPoint extends AbstractPgenTool {
	
	/**
	 * Possible states to be in when adding a point to an element
	 * @author sgilbert
	 */
	private enum ADD_STATUS { START, SELECTED };
	
	
    /**
     * Input handler for mouse events.
     */ 
    protected IInputHandler addPtHandler = null;

    
    public PgenAddPoint(){
    	
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
    	private DECollection ghost = new DECollection();
    	Color ghostColor = new Color( 255,255,255);
    	private Symbol newDot =  new Symbol(null, new Color[] { new Color(255,0,0) }, 2.5f, 7.5, 
    			false, null, "Marker", "DOT" );
    	
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
        			//AbstractDrawableComponent elSelected = drawingLayer.getNearestComponent( loc, addPointFilter );
        			//AbstractDrawableComponent elSelected = drawingLayer.getNearestComponent( loc, addPointFilter, true );
        			AbstractDrawableComponent elSelected = drawingLayer.getNearestElement( loc, addPointFilter);

        			if ( elSelected == null ) return false;
        			drawingLayer.setSelected( elSelected );
        			status = ADD_STATUS.SELECTED;
        			preempt = true;
        			break;
        		case SELECTED:
        			/*
        			 * Create new element with new point added.
    			     * Replace the old element in the resource with the new one.
        			 */
        			drawingLayer.removeGhostLine();
        			AbstractDrawableComponent newComp = addPointToElement( loc, drawingLayer.getSelectedComp() );
        			if ( newComp instanceof Gfa ) {
        				if( ((Gfa)newComp).getGfaFcstHr().indexOf("-") > -1 ){
        					// snap
        					((Gfa)newComp).snap();
   	    				    GfaReducePoint.WarningForOverThreeLines( (Gfa)newComp  );           		    				
        				}

        				((Gfa)newComp).setGfaVorText( Gfa.buildVorText( (Gfa)newComp ) );
        			}
        			
        			drawingLayer.replaceElement(drawingLayer.getSelectedComp(), newComp);
        			
        			drawingLayer.removeSelected();
        			status = ADD_STATUS.START;
        			preempt = false;
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
        public boolean handleMouseMove(int anX, int aY){
        	if ( !isResourceEditable() ) return false;
        	
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null ) return false;
        	
    		switch (status) {
    		
    		case SELECTED:
    			/*
    			 * ghost line with new point added
    			 */
    			ghost.clear();
    			newDot.setLocation(loc);
    			//ghost.add(newDot);
    			newEl = addPointToElement( loc, drawingLayer.getSelectedDE() );
    			newEl.setColors(new Color[]{ghostColor, ghostColor});
    			ghost.add(newEl);
    			ghost.add(newDot);
    			drawingLayer.setGhostLine(ghost);
    			refresh();
    			break;
    		}
    		
        	return false;
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if ( shiftDown || !isResourceEditable() ) return false;
			else return preempt;
		}  
        
        
    }
    
    /*
     * Create a new Drawable Element from the given DE with the new point added to the given segment.
     */
	public DrawableElement addPointToElement(Coordinate point, DrawableElement selectedDE) {
		
 		DrawableElement newEl = (DrawableElement)selectedDE.copy();
 		
 		ArrayList<Coordinate> pts = calculateNewPoints( point, selectedDE.getPoints() );
 		
		newEl.setPoints( pts );
		return newEl;
		
	}

    /*
     * Create a new Drawable Element from the given DE with the new point added to the given segment.
     * Note that Jet does not extend from DrawableElement and must be treated differently
     */
	public AbstractDrawableComponent addPointToElement(Coordinate point, AbstractDrawableComponent elem) {
		
		if ( elem instanceof DrawableElement ) {
			DrawableElement de = (DrawableElement)elem;
			return addPointToElement( point, de );
		}
		else if ( elem instanceof Jet ) {
			Jet newEl = ((Jet)elem).copy();

			ArrayList<Coordinate> pts = calculateNewPoints( point, elem.getPrimaryDE().getPoints() );

			newEl.getPrimaryDE().setPoints( pts );
			return newEl;
		}
		return elem;
		
	}

	/*
	 * Adds the given point to the closest segment of the line defined by the list of vertices 
	 */
	private ArrayList<Coordinate> calculateNewPoints( Coordinate point, List<Coordinate> vertices ) {
		
		CoordinateList coords;
 		GeometryFactory gf = new GeometryFactory();

 		/*
 		 * find the location on the line closest to the given point
 		 */
 		LineString ls = gf.createLineString( vertices.toArray(new Coordinate[]{}) );
 		LocationIndexedLine lil = new LocationIndexedLine(ls);
 		LinearLocation loc = lil.project(point);
 		
 		if ( loc.compareTo( lil.getStartIndex() ) == 0 ) {
 			/*
 			 * closest point is the first point.  Add new point to beginning of line
 			 */
 			coords = new CoordinateList( new Coordinate[]{point}, false);
 			coords.add( ls.getCoordinates(), false);
 		}
 		else if ( loc.getSegmentIndex() ==  lil.getEndIndex().getSegmentIndex() ) {
 			/*
 			 * closest point is the last point.  Add new point to end of line
 			 */
 			coords = new CoordinateList( ls.getCoordinates(), false );
 			coords.add(new Coordinate[]{point}, false);
 		}
 		else {
 			/*
 			 * Add point to the proper segment
 			 */
 	 		LinearLocation previous = new LinearLocation( loc.getComponentIndex(), loc.getSegmentIndex(), 0.0);
 	 		LinearLocation next = new LinearLocation( loc.getComponentIndex(), loc.getSegmentIndex()+1, 0.0);
 			Geometry g1 = lil.extractLine(lil.getStartIndex(), previous );
 			Geometry g2 = lil.extractLine(next, lil.getEndIndex() );

 			coords = new CoordinateList( g1.getCoordinates(), false);
 			coords.add(point, false);
 			coords.add( g2.getCoordinates(), false);
 		}
 		
 		ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
 		for ( int i=0; i < coords.size(); i++ ) pts.add( coords.getCoordinate(i) );
 		
		return pts;
	}

}
	
