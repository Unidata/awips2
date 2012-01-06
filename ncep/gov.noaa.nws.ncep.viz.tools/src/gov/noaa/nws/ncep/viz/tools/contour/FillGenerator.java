/*
 * FillGenerator
 * 
 * Date created 24 MAY 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.contour;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.Map.Entry;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;
import com.vividsolutions.jts.operation.linemerge.LineMerger;

/**
 * Creates polygons for "filled" areas above, below, or between specified contour values.
 * Using classes supply LineStrings and Polygons of contour lines in grid coordinates for various
 * contour values along with a geometry describing the grid edges.  
 * @author sgilbert
 *
 */
public class FillGenerator {

	GeometryFactory gf;
	Geometry grid;
	LocationIndexedLine edges;
	SortedMap<LinearLocation, Geometry> outerEdgeLocations;
	
	HashMap<Float, Geometry> contourMap;
	HashMap<Float, ContourData> openContourData;

	/**
	 * Holds info about the contour lines that intersect with the grid edges
	 * and where they intersect.
	 * @author sgilbert
	 *
	 */
	private class ContourData {
		public List<LineString> openContours;
		
		// TreeMap keeps edge intersections ordered by location
		public TreeMap<LinearLocation, Geometry> contourEdgeIntersections;
	}
	
	/**
	 * Constructor specifying the edges of the grid and missing data areas.
	 * The shell polygon of the edges should be in counter clockwise order (origin in lower left)
	 * @param gridGeom
	 */
	public FillGenerator(Geometry gridGeom) {
		gf = new GeometryFactory();
		
		grid = gridGeom;
        contourMap = new HashMap<Float,Geometry>();
        openContourData = new HashMap<Float, ContourData>();
        
        edges = new LocationIndexedLine( grid );
        outerEdgeLocations = new TreeMap<LinearLocation,Geometry>();

        int n = 0;
        for (Coordinate c : grid.getCoordinates() ) {
    		outerEdgeLocations.put( new LinearLocation(n++, 0), null );
//    		outerEdgeLocations.put( edges.indexOf(c), null );
    	}
    	
	}


	/**
	 * Add a set of LineStrings and Polygons representing the contours for a specified contour value.
	 * It is assumed that the direction of the contours are oriented so that values larger than cval
	 * are to the left of the line as it is traversed.  
	 * 
	 * LineStrings are only considered for the Fill polygons, if it's boundaries (endpoints) intersect
	 * with the grid geometry supplied in the constructor.
	 * 
	 * The fill algorithm requires valid JTS LineStrings and Polygons, so an attempt is made to separate
	 * self-intersecting Geometries into individual non self-intersecting ones. This algorithm will not
	 * catch and process all invalid Geometries, so supplying valid JTS Geometries is recommended.
	 * @param cval value associated with the contours.
	 * @param contours A collection of LineStrings and Polygons  
	 */
	public void addContours(float cval, Geometry contours) {
		
		/*
		 * check for and try to fix invalid JTS Geometries.  Subsequent relations depend on valid Geometries.
		 */
		Geometry adapted = ContourValidator.validateContours(contours);
		contourMap.put( cval, adapted);
		
		ContourData cdata = new ContourData();
		cdata.openContours = new ArrayList<LineString>();
		cdata.contourEdgeIntersections = new TreeMap<LinearLocation, Geometry>();
		
		/*
		 * find edge intersections for all open contours, and save for later.
		 */
    	for ( int i=0; i<adapted.getNumGeometries(); i++ ) {
    		Geometry curr = adapted.getGeometryN(i);
    		if ( curr instanceof LineString ) {
    			curr.setUserData(cval);
    			addContourSegment((LineString)curr, cdata );
    		}
    	}
    	
    	openContourData.put( cval, cdata);
		
	}

	/*
	 * calculate edge intersections --  combining when necessary
	 */
	private void addContourSegment(LineString cntr, ContourData cdata) {
		
		Geometry intx = cntr.getBoundary().intersection( grid );
		
		if ( intx instanceof MultiPoint ) { 
			/*
			 * LineString endpoints intersect w/ grid edge.  Find locations of intersection on edges
			 */
			Coordinate firstPt = intx.getGeometryN(0).getCoordinate();
			LinearLocation firstLoc = edges.indexOf(firstPt);
			Coordinate secondPt = intx.getGeometryN( intx.getNumGeometries()-1 ).getCoordinate();
			LinearLocation secondLoc = edges.indexOf(secondPt);
			
			if ( !cdata.contourEdgeIntersections.containsKey(firstLoc) && !cdata.contourEdgeIntersections.containsKey(secondLoc) ) {
				// new intersection points on edge
				cdata.openContours.add(cntr);
				cdata.contourEdgeIntersections.put( firstLoc, cntr );
				cdata.contourEdgeIntersections.put( secondLoc, cntr );
			}
			else {
				/*
				 * intersection point already exists on edges.  combine this line with the one that
				 * already exists into One LineString.
				 */
				LinearLocation tempLoc;
				if ( cdata.contourEdgeIntersections.containsKey(firstLoc) ) {
					tempLoc = firstLoc;
				}
				else {
					tempLoc = secondLoc;
				}
				LineString old = (LineString)cdata.contourEdgeIntersections.get(tempLoc);
				//cdata.contourEdgeIntersections.put(tempLoc, null);
				List<LinearLocation> removeList = new ArrayList<LinearLocation>();
				for ( Entry<LinearLocation,Geometry> entry : cdata.contourEdgeIntersections.entrySet() ) {
					if ( entry.getValue() == old ) removeList.add(entry.getKey());
				}
				for ( LinearLocation each : removeList ) cdata.contourEdgeIntersections.remove(each);;
				cdata.openContours.remove(old);
				LineMerger lm = new LineMerger();
				lm.add(old);
				lm.add(cntr);
				//System.out.println("MERGEIN1 "+old.getNumPoints()+old.getBoundary());
				//System.out.println("MERGEIN2 "+cntr.getNumPoints()+cntr.getBoundary());
				for ( Object ls : lm.getMergedLineStrings() ) {
					//System.out.println("MERGEOUT "+((LineString)ls).getNumPoints()+((LineString)ls).getBoundary());
					if ( ls instanceof LineString) {
						((LineString)ls).setUserData( cntr.getUserData() );
						addContourSegment((LineString)ls, cdata);
					}
				}
			}
		}
		
	}

	/**
	 * Calculate Polygons that represent the areas that have data values less than the given 
	 * contour value fval.
	 * @param fval Upper contour value
	 * @return A collection of Polygons
	 * @throws FillException
	 */
	public Geometry fillLessThan(float fval) throws FillException {
		
		Geometry contours;
    	List<Geometry> geoms = new ArrayList<Geometry>();
    	
    	if ( contourMap.containsKey(fval) ) contours = contourMap.get(fval);
    	else {
    		throw new FillException("Do not have contours for value: "+fval);
    	}
    	
    	List<Polygon> savedPolygons = new ArrayList<Polygon>();
    	TreeMap<GeometryData, Geometry> possibleHoles = new TreeMap<GeometryData, Geometry>();
    	
    	List<Geometry> linestrings = new ArrayList<Geometry>( openContourData.get(fval).openContours );
    	TreeMap<LinearLocation, Geometry> locIndex = new TreeMap<LinearLocation, Geometry>(outerEdgeLocations);
    	locIndex.putAll( openContourData.get(fval).contourEdgeIntersections );
    	
    	/*
    	 * Loop through each contour geometry
    	 */
    	for ( int i=0; i<contours.getNumGeometries(); i++ ) {
    		Geometry curr = contours.getGeometryN(i);
    		
    		if ( curr instanceof Polygon ) {
    			//System.out.println("Found POLY");
    			if ( CGAlgorithms.isCCW( curr.getCoordinates() ) ) {
    				//  lower values inside polygon
    				savedPolygons.add( (Polygon)curr );
    			}
    			else {
    				// higher values inside polygon
    				possibleHoles.put(new GeometryData(curr), curr);
    				//System.out.println("centroid"+curr.getCentroid());
    			}
    		}
    		else if ( curr instanceof LineString ) {
    			//System.out.println("Found LS "+curr.getBoundary()+"   start:"+curr.getCoordinates()[0]);
    		}
    		else {
        		System.out.println("Unexpected Geometry: "+curr);
    		}

    	}

    	/*
    	 * Start with any contour line.  Add the points of the line to a new polygon.
    	 * The last point intersects the edges.  Since the lower values are on the right
    	 * side of the line, traverse the points along the edge in a clockwise direction,
    	 * adding each edge point to the new polygon until another contour line is
    	 * encountered.  Add the points of that contour line to the new polygon. Now
    	 * continue adding edge points from where the last contour ended, etc... 
    	 * Continue this until we reach the first point of the contour line we started with.
    	 * We now have a polygon with lower values inside. 
    	 * 
    	 * Remove the contour lines used above from the list.
    	 * Continue above until all open contour lines are accounted for.
    	 */
    	while ( linestrings.size() > 0 ) {
    		//System.out.println("Curr size = "+linestrings.size());
    		// start with any contour line
    		Geometry curr = linestrings.remove(0);      
    		CoordinateList clist = new CoordinateList(curr.getCoordinates());

    		Coordinate first = curr.getCoordinates()[0];
    		LinearLocation firstLoc = edges.indexOf(first);
    		Coordinate last = curr.getCoordinates()[ curr.getNumPoints()-1];
    		LinearLocation lastLoc = edges.indexOf(last);
    		//System.out.println("START "+curr.getUserData()+" "+curr.getCoordinates()[0]+curr.getCoordinates()[curr.getNumPoints()-1]);
    		
    		LinearLocation temp = locIndex.lowerKey(lastLoc);
    		if ( temp == null ) temp = locIndex.lastKey();
    		
    		/*
    		 * Add edge points and contour lines until starting contour point is reached.
    		 */
    		boolean problem = false;
    		int iteration = 0;
    		int maxIterations = linestrings.size() + locIndex.size();
    		while ( (temp.compareTo(firstLoc) != 0)  && ! problem) {
    			if ( locIndex.get(temp) ==  null ) {
    				clist.add( edges.extractPoint(temp), true);
    				//System.out.println("COORD= "+edges.extractPoint(temp));
        			temp = locIndex.lowerKey(temp);
    			}
    			else {
    				Geometry g = locIndex.get(temp);
    				clist.add(g.getCoordinates(), true);
    				//System.out.println("COORD= "+g.getUserData()+":"+g.getCoordinates()[0]+g.getCoordinates()[g.getNumPoints()-1]);
    				//System.out.println("REMOVING< "+g.getNumPoints()+" at: "+g.getBoundary());
    				linestrings.remove(g);
    				LinearLocation newest = edges.indexOf( g.getCoordinates()[g.getNumPoints()-1] );
    				if ( newest.compareTo(firstLoc) == 0 ) break;
        			temp = locIndex.lowerKey( newest );
    			}
    			
    			if ( temp == null ) temp = locIndex.lastKey();
    			if ( iteration++ > maxIterations )	problem = true;
    			
    		};
    		
    		if ( problem ) {
    			System.out.println("This contour segment caused problems.  It is being discarded.");
    			continue;
    		}
    		
    		/*
    		 * Create Polygon from accumulated coordinates
    		 */
    		clist.closeRing();
    		LinearRing outer = gf.createLinearRing(clist.toCoordinateArray());
    		Polygon outerPoly = gf.createPolygon(outer, null);
    		
    		/*
    		 * Add any appropriate interior holes to the new Polygon
    		 */
    		Geometry g = addAnyInteriorHoles( outerPoly, possibleHoles );
    		geoms.add(g);
    	}
    	
    	/*
    	 * Check all original Polygons to see if any interior holes should be added
    	 */
    	for ( Polygon p : savedPolygons ) {
    		Geometry g = addAnyInteriorHoles( p, possibleHoles );
   			geoms.add(g);
    	}
        	
    	return gf.createGeometryCollection( geoms.toArray(new Geometry[] {}));

	}


	/**
	 * Calculate Polygons that represent the areas that have data values greater than the given 
	 * contour value fval.
	 * @param fval Upper contour value
	 * @return A collection of Polygons
	 * @throws FillException
	 */
	public Geometry fillGreaterThan(float fval) throws FillException {
		
		Geometry contours;
    	List<Geometry> geoms = new ArrayList<Geometry>();
    	
    	if ( contourMap.containsKey(fval) ) contours = contourMap.get(fval);
    	else {
    		throw new FillException("Do not have contours for value "+fval);
    	}
    	
    	//System.out.println("BOX="+grid);
    	List<Polygon> savedPolygons = new ArrayList<Polygon>();
    	TreeMap<GeometryData, Geometry> possibleHoles = new TreeMap<GeometryData, Geometry>();
    	List<Geometry> linestrings = new ArrayList<Geometry>( openContourData.get(fval).openContours );
    	TreeMap<LinearLocation, Geometry> locIndex = new TreeMap<LinearLocation, Geometry>(outerEdgeLocations);
    	
    	locIndex.putAll( openContourData.get(fval).contourEdgeIntersections );
    	
    	/*
    	 * Loop through each contour geometry
    	 */
    	for ( int i=0; i<contours.getNumGeometries(); i++ ) {
    		Geometry curr = contours.getGeometryN(i);
    		
    		if ( curr instanceof Polygon ) {
    			//System.out.println("Found POLY");
    			if ( ! CGAlgorithms.isCCW( curr.getCoordinates() ) ) {
    				// higher values inside polygon
    				savedPolygons.add( (Polygon)curr );
    			}
    			else {
    				// lower values inside polygon
    				possibleHoles.put(new GeometryData(curr), curr);
    				//System.out.println("centroid"+curr.getCentroid());
    			}
    		}
    		else if ( curr instanceof LineString ) {
    			//System.out.println("Found LS "+curr.getBoundary()+"   start:"+curr.getCoordinates()[0]);
    		}
    		else {
        		System.out.println("Unexpected Geometry: "+curr);
    		}

    	}

    	/*
    	 * Start with any contour line.  Add the points of the line to a new polygon.
    	 * The last point intersects the edges.  Since the higher values are on the left
    	 * side of the line, traverse the points along the edge in a counter-clockwise direction,
    	 * adding each edge point to the new polygon until another contour line is
    	 * encountered.  Add the points of that contour line to the new polygon. Now
    	 * continue adding edge points from where the last contour ended, etc... 
    	 * Continue this until we reach the first point of the contour line we started with.
    	 * We now have a polygon with lower values inside. 
    	 * 
    	 * Remove the contour lines used above from the list.
    	 * Continue above until all open contour lines are accounted for.
    	 */
    	while ( linestrings.size() > 0 ) {
    		//System.out.println("Curr size = "+linestrings.size());
    		// start with any contour line
    		Geometry curr = linestrings.remove(0);
    		CoordinateList clist = new CoordinateList(curr.getCoordinates());

    		Coordinate first = curr.getCoordinates()[0];
    		LinearLocation firstLoc = edges.indexOf(first);
    		Coordinate last = curr.getCoordinates()[ curr.getNumPoints()-1];
    		LinearLocation lastLoc = edges.indexOf(last);
    		
    		LinearLocation temp = locIndex.higherKey(lastLoc);
    		if ( temp == null ) temp = locIndex.firstKey();
    		
    		/*
    		 * Add edge points and contour lines until starting contour point is reached.
    		 */
    		boolean problem = false;
    		int iteration = 0;
    		int maxIterations = linestrings.size() + locIndex.size();
    		while ( (temp.compareTo(firstLoc) != 0) && ! problem ) {
    			if ( locIndex.get(temp) ==  null ) {
    				clist.add( edges.extractPoint(temp), true);
        			temp = locIndex.higherKey(temp);
    			}
    			else {
    				Geometry g = locIndex.get(temp);
    				clist.add(g.getCoordinates(), true);
    				//System.out.println("REMOVING> "+g.getNumPoints()+" at: "+g.getBoundary());
    				linestrings.remove(g);
    				LinearLocation newest = edges.indexOf( g.getCoordinates()[g.getNumPoints()-1] );
        			temp = locIndex.higherKey( newest );
    			}
    			
    			if ( temp == null ) temp = locIndex.firstKey();
    			if ( iteration++ > maxIterations )	problem = true;
    		};
    		
    		if ( problem ) {
    			System.out.println("This contour segment caused problems.  It is being discarded.");
    			continue;
    		}

    		/*
    		 * Create Polygon from accumulated coordinates
    		 */
    		clist.closeRing();
    		LinearRing outer = gf.createLinearRing(clist.toCoordinateArray());
    		Polygon outerPoly = gf.createPolygon(outer, null);

    		/*
    		 * Add any appropriate interior holes to the new Polygon
    		 */
    		Geometry g = addAnyInteriorHoles( outerPoly, possibleHoles );
    		geoms.add(g);
    	}
    	
       	/*
    	 * Check all original Polygons to see if any interior holes should be added
    	 */
    	for ( Polygon p : savedPolygons ) {
    		Geometry g = addAnyInteriorHoles( p, possibleHoles );
   			geoms.add(g);
    	}
        	
    	return gf.createGeometryCollection( geoms.toArray(new Geometry[] {}));

	}

	/**
	 * Calculate Polygons that represent the areas that have data values between the given 
	 * contour values fval1 and fval2.
	 * @param fval Upper contour value
	 * @return A collection of Polygons
	 * @throws FillException
	 */
    public Geometry fillBetween(float fval1, float fval2 ) throws FillException {

		List<Geometry> geoms = new ArrayList<Geometry>();

		for ( float fval : new float[] { fval1, fval2} ) {
			if ( ! contourMap.containsKey(fval) ) 
				throw new FillException("Do not have contours for value "+fval);
		}
    	
		float lowerValue, upperValue;
		Geometry lower, upper;
		if ( fval1 < fval2 ) {
			lowerValue = fval1;
			upperValue = fval2;
			lower = contourMap.get( fval1 );
			upper = contourMap.get( fval2 );
		}
		else {
			lowerValue = fval2;
			upperValue = fval1;
			lower = contourMap.get( fval2 );
			upper = contourMap.get( fval1 );
		}

    	//System.out.println("BOX="+grid+" between "+lowerValue+"  "+upperValue);
    	List<Geometry> possibleOuterRing = new ArrayList<Geometry>();
    	TreeMap<GeometryData, Geometry> possibleHoles = new TreeMap<GeometryData, Geometry>();
    	List<Geometry> linestrings = new ArrayList<Geometry>( openContourData.get(lowerValue).openContours );
    	linestrings.addAll( openContourData.get(upperValue).openContours );
    	
    	TreeMap<LinearLocation, Geometry> locIndex = new TreeMap<LinearLocation, Geometry>(outerEdgeLocations);
    	//System.out.println("StartSize="+locIndex.size());
    	locIndex.putAll( openContourData.get(lowerValue).contourEdgeIntersections );
    	//System.out.println("AddLower"+ openContourData.get(lowerValue).contourEdgeIntersections.size()+"="+locIndex.size());
    	locIndex.putAll( openContourData.get(upperValue).contourEdgeIntersections );
    	//System.out.println("AddUpper"+openContourData.get(upperValue).contourEdgeIntersections.size()+"="+locIndex.size());
    	
    	/*
    	 * Loop through each contour geometry for the lower contour value
    	 */
    	for ( int i=0; i<lower.getNumGeometries(); i++ ) {
    		Geometry curr = lower.getGeometryN(i);
    		
    		if ( curr instanceof Polygon ) {
    			//System.out.println("Found POLY");
    			if ( ! CGAlgorithms.isCCW( curr.getCoordinates() ) ) {
    				//  higher values inside polygon
    				possibleOuterRing.add(curr);
    				//System.out.println("SHELL: "+curr.getNumPoints()+curr.getCentroid()+curr.getEnvelopeInternal());
    				//System.out.println(curr);
    			}
    			else {
    				// lower values inside polygon
    				curr.setUserData(lowerValue);
    				possibleHoles.put(new GeometryData(curr), curr);
    				//System.out.println("HOLE: "+curr.getCentroid()+curr.getEnvelopeInternal());
    				//System.out.println("centroid"+curr.getCentroid());
    			}
    		}
    		else if ( curr instanceof LineString ) {
    			//System.out.println("Found LS "+curr.getBoundary()+"   start:"+curr.getCoordinates()[0]);
    		}
    		else {
        		System.out.println("Unexpected Geometry: "+curr);
    		}

    	}
    	
    	/*
    	 * Loop through each contour geometry for the higher contour value
    	 */
    	for ( int i=0; i<upper.getNumGeometries(); i++ ) {
    		Geometry curr = upper.getGeometryN(i);
    		
    		if ( curr instanceof Polygon ) {
    			//System.out.println("Found POLY");
    			if ( ! CGAlgorithms.isCCW( curr.getCoordinates() ) ) {
    				//  higher values inside polygon
    				curr.setUserData(upperValue);
    				possibleHoles.put(new GeometryData(curr), curr);
    				//System.out.println("HOLE: "+curr.getArea()+curr.getCentroid()+curr.getEnvelopeInternal());
    			}
    			else {
    				//  lower values inside polygon
    				possibleOuterRing.add(curr);
    				//System.out.println("SHELL: "+curr.getCentroid()+curr.getEnvelopeInternal());
    				//System.out.println("centroid"+curr.getCentroid());
    			}
    		}
    		else if ( curr instanceof LineString ) {
    			//System.out.println("Found LS "+curr.getBoundary()+"   start:"+curr.getCoordinates()[0]);
    		}
    		else {
        		System.out.println("Unexpected Geometry: "+curr);
    		}

    	}

    	/*
    	 * Start with any contour line.  If the line is part of the upper contour, 
    	 * reverse the order of the points so that higher values will now be to the right of the line.
    	 * Add the points of the line to a new polygon.  The last point intersects the edges.  
    	 * Traverse the points along the edge in a counter-clockwise direction,
    	 * adding each edge point to the new polygon until another contour line is
    	 * encountered.  Add the points of that contour line (reverse order of points, if necessary) 
    	 * to the new polygon. Now continue adding edge points from where the last contour ended, etc... 
    	 * Continue this until we reach the first point of the contour line we started with.
    	 * We now have a polygon with values between the lower and higher contour values inside. 
    	 * 
    	 * Remove the contour lines used above from the list.
    	 * Continue above until all open contour lines are accounted for.
    	 */
    	while ( linestrings.size() > 0 ) {
    		//System.out.println("Curr size = "+linestrings.size());
    		Geometry curr = linestrings.remove(0);
    		CoordinateList clist;
    		if ( curr.getUserData().equals(upperValue) ) {
    			clist = new CoordinateList( ((LineString)curr).reverse().getCoordinates() );
    		}
    		else {
    			clist = new CoordinateList(curr.getCoordinates());
    		}
    		
    		Coordinate first = clist.getCoordinate(0);
    		LinearLocation firstLoc = edges.indexOf(first);
    		Coordinate last = clist.getCoordinate( clist.size()-1);
    		LinearLocation lastLoc = edges.indexOf(last);
    		//System.out.println("START "+curr.getUserData()+" "+curr.getBoundary());
    		
    		LinearLocation newest;
    		LinearLocation temp = locIndex.higherKey(lastLoc);
    		if ( temp == null ) temp = locIndex.firstKey();
    		
    		boolean problem = false;
    		int iteration = 0;
    		int maxIterations = linestrings.size() + locIndex.size();
    		while ( (temp.compareTo(firstLoc) != 0) && ! problem ) {
    			if ( locIndex.get(temp) ==  null ) {
    				//System.out.println("COORD= "+edges.extractPoint(temp));
    				clist.add( edges.extractPoint(temp), true);
        			temp = locIndex.higherKey(temp);
    			}
    			else {
    				Geometry g = locIndex.get(temp);
    				//System.out.println("COORD= "+g.getUserData()+":"+g.getCoordinates()[0]+g.getCoordinates()[g.getNumPoints()-1]);
    				//System.out.println("REMOVING<> "+g.getNumPoints()+" at: "+g.getBoundary());
    				if ( edges.extractPoint(temp).equals2D( g.getCoordinates()[0]) ) {
        				clist.add(g.getCoordinates(), true);
        				newest = edges.indexOf( g.getCoordinates()[g.getNumPoints()-1] );
    				}
    				else {
    					//System.out.println(g);
    					LineString ls = (LineString)g.reverse();
        				clist.add( ls.getCoordinates(), true);
        				newest = edges.indexOf( g.getCoordinates()[0] );
    				}
    				linestrings.remove(g);
    				if ( newest.compareTo(firstLoc) == 0 ) break; 
    				temp = locIndex.higherKey( newest );
    			}
    			//try {Thread.sleep(300);} catch(Exception e){}
    			if ( temp == null ) temp = locIndex.firstKey();
    			if ( iteration++ > maxIterations )	problem = true;
    		};
    		
    		if ( problem ) {
    			System.out.println("This contour segment caused problems.  It is being discarded.");
    			continue;
    		}
    		
    		/*
    		 * Create Polygon from accumulated coordinates
    		 */
    		clist.closeRing();
    		LinearRing outer = gf.createLinearRing(clist.toCoordinateArray());
    		Polygon outerPoly = gf.createPolygon(outer, null);
    		
    		/*
    		 * Add any appropriate interior holes to the new Polygon
    		 */
    		Geometry g = addAnyInteriorHoles( outerPoly, possibleHoles );
    		geoms.add(g);
    	}
    	
       	/*
    	 * Check all original Polygons to see if any interior holes should be added
    	 */
    	for ( Geometry shell : possibleOuterRing ) {
    		LinearRing outer = gf.createLinearRing(shell.getCoordinates());
    		Polygon outerPoly = gf.createPolygon(outer, null);
    		Geometry g = addAnyInteriorHoles( outerPoly, possibleHoles );
    		geoms.add(g);
    	}
    	
    	return gf.createGeometryCollection( geoms.toArray(new Geometry[] {}));
    	
    }


    /*
     * Adds possible holes (inner rings) to a polygon shell (outer ring).  Assumes possible holes
     * are sorted by size of their area so that larger holes can be added first.  Holes that are
     * contained within an existing inner ring are discarded.
     */
	private Geometry addAnyInteriorHoles(Polygon poly, TreeMap<GeometryData, Geometry> possible) {

		List<LinearRing> holes = new ArrayList<LinearRing>();
		
		GeometryData next = possible.floorKey( new GeometryData(poly) );
		
		while ( next != null ) {
			Geometry g = possible.get(next);
			if ( poly.contains(g) && notInOthers(g, holes) ) {
				LinearRing lr = gf.createLinearRing( g.getCoordinates() );
				holes.add(lr);
			}
			//System.out.println("possibleissue = "+g.getCentroid()+g.getEnvelopeInternal());
			next = possible.lowerKey(next);
		}
		
		if ( holes.isEmpty() ) return poly;
		
		LinearRing outer = gf.createLinearRing(poly.getCoordinates());
		Polygon newPoly = gf.createPolygon(outer, holes.toArray(new LinearRing[] {}));
		return newPoly;
	}

	/*
	 * Returns true if g is not contained within any given holes
	 */
	private boolean notInOthers(Geometry g, List<LinearRing> holes) {
		
		for ( LinearRing lr : holes ) {
			Polygon tmp = gf.createPolygon(lr, null);
			if ( tmp.contains(g) ) return false;
		}
		return true;
	}


}
