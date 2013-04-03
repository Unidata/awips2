/*
 * gov.noaa.nws.ncep.ui.pgen.tools
 * 
 * 14 June 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import static java.lang.Math.PI;
import static java.lang.Math.abs;
import static java.lang.Math.atan2;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.viz.common.SnapUtil;
import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * A class to hold algorithms for PGEN tools.
 * Some algorithms are left to tools because they need either the mapEditor 
 * or the drawingLayer. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09		#280		B. Yin   	Move methods from action tools
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 02/12                    S. Gurung   Moved isSnapADC() and getNumOfCompassPts() to SigmeInfo. 
 * 11/12		#?			J. Wu   	handle Gfa text box location
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenToolUtils {
	
	/**
	 * Build a new DrawableElement after extrapolation 
     * @param de		element to be extrapolated
     * @param dir	    direction for extrapolation (wind direction)
     * @param dist    	distance for extrapolation
	 *
	 * @return a new extrapolated DrawableElement
	 */
	public static AbstractDrawableComponent extrapElement( AbstractDrawableComponent adc, double dir, double dist ) {
		
		AbstractDrawableComponent newEl = null;    	    
		
		if ( adc != null ) {        		
			
			newEl = adc.copy();				    
			Iterator<DrawableElement> it = newEl.createDEIterator();
			while( it.hasNext() ){
				DrawableElement de = it.next();
				de.setPointsOnly( extrapPoints( de.getPoints(), dir,  dist, adc ) );
			}
			
			/*
			 * Move the Gfa text box as well.
			 */
			if ( adc instanceof Gfa ) {
				ArrayList<Coordinate>  pts = new ArrayList<Coordinate>();
				pts.add( ((Gfa)adc).getGfaTextCoordinate() );
				ArrayList<Coordinate> expPts = extrapPoints( pts, dir,  dist, adc );
				((Gfa)newEl).setGfaTextCoordinate( expPts.get(0) );
			}
			
		}  
		
		return newEl;
	}   	

    
    /**
     * Locate point at a given distance and direction
     * @param points	points to be extrapolated
     * @param direction	direction for extrapolation (wind direction)
     * @param distance	distance for extrapolation
 	 *
 	 * @return extrapolated points
     */
    private static ArrayList<Coordinate> extrapPoints( List<Coordinate> points, 
    		                                    double direction,  double distance, AbstractDrawableComponent adc ) {
    	
    	ArrayList<Coordinate> newPoints = null;
    	    
    	if ( points != null && points.size() > 0 )  {
   	    	
    	    newPoints = new ArrayList<Coordinate>();
    		
     	    GeodeticCalculator gc = new GeodeticCalculator( DefaultEllipsoid.WGS84 );
    		        		
    		for ( Coordinate pt : points ) {
          
    			/*
    			 * Find a point "distance" away at the "direction" of the given point
    			 * Note that the input direction is the wind direction and should be 
    			 * convert to direction in GC by subtracting 180.
    			 */
    			gc.setStartingGeographicPoint( pt.x , pt.y );                   
   			
    		    gc.setDirection( direction - 180.0, distance );
        		    	
    		    java.awt.geom.Point2D newP = gc.getDestinationGeographicPoint();      	            
    		    newPoints.add( new Coordinate( newP.getX(), newP.getY() ) );
   		
    		}
    		
    	}
    	if( SigmetInfo.isSnapADC(adc) ){
  	    	int numOfCompassPts = SigmetInfo.getNumOfCompassPts(adc);
  	    	return SnapUtil.getSnapWithStation(newPoints, SnapUtil.VOR_STATION_LIST, 10, numOfCompassPts);
  	    }
        return newPoints;
    
    }
    
    /*
     * This helper method creates a copy of the input object and then reset the contained  
     * Coordinate points in the reverse order in the copied object.  If DrawableElement is a Front,
     * simply toggle the flipSide flag instead of reversing points.
     * @param selectedDrawableElement, the original DrawableElement object used to create a new
     * DrawableElement object with a reversed order of points
     * @return null if the input DrawableElement is null, otherwise return a copy of the 
     * original DrawableElement object with the Coordinate points in the reverse order 
     */
    public static AbstractDrawableComponent createReversedDrawableElement(AbstractDrawableComponent drawableElement) {
    	if(drawableElement == null)
    		return null; 
		AbstractDrawableComponent drawableElementWithReversedPoints = drawableElement.copy(); 
		if ( drawableElementWithReversedPoints.getPgenCategory().equals("Front") )
			((Line)drawableElementWithReversedPoints).setFlipSide( ! ((Line)drawableElementWithReversedPoints).isFlipSide() );
		else
			reverseElementCoordinatePoints(drawableElementWithReversedPoints.getPrimaryDE()); 
    	return drawableElementWithReversedPoints; 
    }
    
    /*
     * retrieve the original Coordinate points list of the DrawableElement, and then
     * reverse the list and then set back to the DrawableElement. 
     * @param selectedDrawableElement, the selected DrawableElement 
     * @return  
     */
	private static void reverseElementCoordinatePoints(DrawableElement selectedDrawableElement) {
		if(selectedDrawableElement == null)
			return; 
		List<Coordinate> drawableElementPointList = selectedDrawableElement.getPoints(); 
		ArrayList<Coordinate> reverseDrawableElementPointList = reverseDrawableElementPoints(drawableElementPointList); 
		selectedDrawableElement.setPoints(reverseDrawableElementPointList); 
	}

    /*
     * reverse the original Coordinate points list of the DrawableElement
     * @param drawableElementPointList, the original Coordinate points list of the DrawableElement 
     * @return an ArrayList<Coordinate> contains the reversed DrawableElement Coordinate points 
     */
    private static ArrayList<Coordinate> reverseDrawableElementPoints(List<Coordinate> drawableElementPointList) {
    	ArrayList<Coordinate> reversedDrawableElementPointList = null; 
    	if(drawableElementPointList != null) {
    		reversedDrawableElementPointList = new ArrayList<Coordinate>(drawableElementPointList.size()); 
    		Coordinate[] coordinatePointArray = new Coordinate[drawableElementPointList.size()]; 
    		drawableElementPointList.toArray(coordinatePointArray); 
    		
        	for(int i = coordinatePointArray.length -1; i>=0; i--) {
        		reversedDrawableElementPointList.add(coordinatePointArray[i]); 
        	}
    		
    	} else {
    		reversedDrawableElementPointList = new ArrayList<Coordinate>(); 
    	}
        return reversedDrawableElementPointList; 
    }
    

	/**
	 * Calculates the direction of the vector (x1, y1), (x2,y2) in swt coordinates.
	 * 
	 * @param c1
	 *            Coordinate
	 * @param c2
	 *            Coordinate
	 * @return double 
	 */
	public static double calculateAngle(double old, double x1, double y1, double x2, double y2) {
		double delx = x2 - x1;
		double dely = y2 - y1;
		if (abs(delx) < 0.1 && abs(dely) < 0.1) {
			return old;
		}
		double theta = atan2(-delx, dely);
		double dir = theta * 180 / PI;
		return transformToRange0To360(dir);
	}
	
	/**
	 * Transforms the angle into the corresponding angle within (0.0,360.0) degrees range.
	 * 
	 * @param angle 
	 * @return
	 */
	public static double transformToRange0To360(double angle) {
		angle %= 360;
		if (angle < 0) {
			angle += 360;
		}
		return angle;
	}

	/**
	 * Wrap WCC/WCL text for Watch Box
	 * @param wcc - String text
	 * @param lineLength - int number of characters per line 
	 * @return
	 */
	public static String wrapWatchText(String wcc, int lineLength){
		
		if ( wcc.charAt(wcc.length()-1) != '\n') wcc +='\n';
		
		StringBuffer strb = new StringBuffer(wcc);
		
		String oneLine = wcc;
		int len = wcc.length();
		
		while ( len > lineLength ){
			
			int newline = oneLine.indexOf("\n");
			if (  newline <= lineLength ){
				len -= newline+1;
				oneLine = oneLine.substring( newline+1);
			}
			else {
				int blk = oneLine.lastIndexOf(" ", lineLength );
				int dot = oneLine.lastIndexOf("...", lineLength );

				int brk = 0;
				if (blk > dot) brk = blk + 1;
				else brk = dot - 3;

				strb.insert(brk+strb.length()-len, '\n');

				oneLine = strb.toString().substring(brk+strb.length()-len);
				len -= brk;
			}
		}
		
		return strb.toString();
		
	}
}
