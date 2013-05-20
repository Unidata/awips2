/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenAddPointHandler
 * 
 * 1 April 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.filter.OperationFilter;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaReducePoint;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;

/**
 * Implements input handler for mouse events for the adding point action.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 04/13		#927		B. Yin   	Moved from the PgenAddPointAlt class
 * 
 * </pre>
 * 
 * @author sgilbert
 */

public class PgenAddPointHandler extends InputHandlerDefaultImpl {
	
	/**
	 * Possible states to be in when adding a point to an element
	 * @author sgilbert
	 */
	private enum ADD_STATUS { START, SELECTED };
	 
	protected AbstractEditor mapEditor;
	protected PgenResource pgenrsc;
	protected AttrDlg attrDlg;
	protected AbstractPgenTool tool;
	
	private boolean preempt;
	private ADD_STATUS status = ADD_STATUS.START;
	private DrawableElement newEl;
	private DECollection ghost = new DECollection();
	Color ghostColor = new Color( 255,255,255);
	private Symbol newDot =  new Symbol(null, new Color[] { new Color(255,0,0) }, 2.5f, 7.5, 
			false, null, "Marker", "DOT" );
	
	OperationFilter addPointFilter = new OperationFilter( Operation.ADD_POINT );
	
	/**
	 * Constructor
	 * @param tool
	 */
	public PgenAddPointHandler( AbstractPgenTool tool ){
	 	this.mapEditor = tool.mapEditor;
    	this.pgenrsc = tool.getDrawingLayer();
    	this.tool = tool;
	}
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
     *      int, int)
     */
    @Override	   	
    public boolean handleMouseDown(int anX, int aY, int button) {
    	
    	if ( !tool.isResourceEditable() ) return false;
    	
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
    			AbstractDrawableComponent elSelected = pgenrsc.getNearestElement( loc, addPointFilter);

    			if ( elSelected == null ) return false;
    			pgenrsc.setSelected( elSelected );
    			status = ADD_STATUS.SELECTED;
    			preempt = true;
    			break;
    		case SELECTED:
    			/*
    			 * Create new element with new point added.
			     * Replace the old element in the resource with the new one.
    			 */
    			pgenrsc.removeGhostLine();
    			AbstractDrawableComponent newComp = addPointToElement( loc, pgenrsc.getSelectedComp() );
    			if ( newComp instanceof Gfa ) {
    				if( ((Gfa)newComp).getGfaFcstHr().indexOf("-") > -1 ){
    					// snap
    					((Gfa)newComp).snap();
	    				    GfaReducePoint.WarningForOverThreeLines( (Gfa)newComp  );           		    				
    				}

    				((Gfa)newComp).setGfaVorText( Gfa.buildVorText( (Gfa)newComp ) );
    			}
    			
    			pgenrsc.replaceElement(pgenrsc.getSelectedComp(), newComp);
    			
    			
    			if ( tool instanceof PgenAddPoint ){
    				pgenrsc.removeSelected();
    				status = ADD_STATUS.START;
    			}
    			else {
    				tool.resetMouseHandler();
    				pgenrsc.removeSelected();
    				pgenrsc.setSelected(pgenrsc.getNearestElement(loc) );
    				tool.setWorkingComponent(pgenrsc.getNearestComponent(loc));
    			}
    			
    			preempt = false;
    			break;
    		}
    		
    		
 	        mapEditor.refresh();  
            return preempt;
            
        }
        else{
        	
           	return false;
           	
        }
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
     *      int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int button) {
		if (  !tool.isResourceEditable() ) return false;
        else if ( button == 3 ) {
        	
    		switch (status) {
    		
    		case SELECTED:
    			if ( tool instanceof PgenAddPoint ){

    				/*
    				 * start over
    				 */
    				pgenrsc.removeGhostLine();
    				pgenrsc.removeSelected();
    				status = ADD_STATUS.START;
    				mapEditor.refresh();
    			}
    			else {
					pgenrsc.removeGhostLine();
					tool.resetMouseHandler();
    			}

    			break;
    		default:
    			tool.resetMouseHandler();
    			//PgenUtil.setSelectingMode();
    		}
        }
		return true;
		
    }
   
    /*
     * overrides the function in selecting tool
     */
    @Override
    public boolean handleMouseMove(int anX, int aY){
    	if ( !tool.isResourceEditable() ) return false;

    	Coordinate loc = mapEditor.translateClick(anX, aY);
    	if ( loc == null ) return false;
    	
    	if ( pgenrsc.getSelectedDE() != null ) status = ADD_STATUS.SELECTED; 
    	
		switch (status) {
		
		case SELECTED:
			/*
			 * ghost line with new point added
			 */
			ghost.clear();
			newDot.setLocation(loc);
			//ghost.add(newDot);
			newEl = addPointToElement( loc, pgenrsc.getSelectedDE() );
			newEl.setColors(new Color[]{ghostColor, ghostColor});
			ghost.add(newEl);
			ghost.add(newDot);
			pgenrsc.setGhostLine(ghost);
			mapEditor.refresh();
			break;
		}
		
    	return false;
    }

	@Override
	public boolean handleMouseDownMove(int x, int y, int mouseButton) {
		if ( shiftDown || !tool.isResourceEditable() ) return false;
		else return preempt;
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

	public AbstractEditor getMapEditor() {
		return mapEditor;
	}

	public PgenResource getPgenrsc() {
		return pgenrsc;
	}
}
