/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.FrzlFormatter
 * 
 * May 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.CurveFitter;
import gov.noaa.nws.ncep.viz.common.SnapUtil;
import gov.noaa.nws.ncep.viz.common.graphicUtil.ReducePoints;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.algorithm.SimplePointInRing;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.operation.distance.DistanceOp;

/**
 * Class to format freezing levels.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/2011		#?			B. Yin		Initial Creation.
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 08/12		#?			B. Yin		Added a cll to build vortest for fzlvl
 * 
 * </pre>
 * 
 * @author byin
 */

public class FrzlFormatter {

	//(nm) distance close enough to be considered as on the FA bound 
	private static final double NEAR_FA_BOUND = 20.;
	
	//(nm) distance to extend open FZLVL line 
	private static final double EXT_DIST = 5000.;
	
	//distance tolerance to reduce points 
	private static final double TOLERANCE = 0.15;
	
	//threshold to run reduce points
	private static final int FRZL_MAX_POINTS = 10;
	
	//(nm) maximum length for gaps
	private static final double FZLVL_MAX_GAP = 100.;
	
	//(nm) minimum length for FRZL to create text
	private static final double FZLVL_MIN_LEN = 100.;
	
	//maximum percentage allowed for the increase in size
	// when a point is removed from a GFA polygon
	private static final double REDUCE_INCR_PCT = 3.;
	private static final double MAX_REDUCE_PCT = 25.;

	//maximum distance allowed between a new point and the
	//point to be removed from a GFA polygon.	(nm?)
	private static final double REDUCE_INCR_DST = 100.;

	private static final int MAX_CHARS_PER_LINE = 65;
	private static final int FRZL_INDENT = 7;
	private static final int MAX_LINES = 3;
	
	//Freeing levels to be formatted
	private List<Gfa>frzlList;
	
	private GeometryFactory gf = new GeometryFactory();
	
	/**
	 * Public constructor. 
	 * @param fzlvlList - a list of freezing level GFAs.
	 */
	public FrzlFormatter( List<Gfa> fzlvlList){
		this.frzlList = fzlvlList;
	}
	
	/**
	 * Format(extend, clip, reduce point) freezing level GFAs. 
	 * @return - formatted freezing levels
	 */
	public List<Gfa> format(){
		
		List<Gfa> ret = new ArrayList<Gfa>();
		
		for ( Gfa frzl : frzlList ){
			if (!frzl.getGfaHazard().equalsIgnoreCase("FZLVL") ) continue;
			if ( frzl.isClosedLine() ){
				ret.addAll( formatClosedFrzl( frzl ) );
			}
			else {
				ret.addAll( formatOpenFrzl( frzl ) );
			}
		}
		
		return ret;
	}
	
	/**
	 * Format(extend, clip, reduce point) open freezing level GFAs.
	 * @param openFrzl - a open freezing level GFA
	 * @return - a list of formatted GFAs.
	 */
	private List<Gfa> formatOpenFrzl( Gfa openFrzl ){
		
		//return value
		List<Gfa> ret = new ArrayList<Gfa>();

		//FRZL coordinates
		ArrayList<Coordinate> frzlCoord = new ArrayList<Coordinate>();
		frzlCoord.addAll(openFrzl.getPoints());
		
		//clip against FA areas
		HashMap<String, Geometry> areaBnds = GfaClip.getInstance().getFaAreaBounds();
		for ( String areaName : areaBnds.keySet() ) {
			//Extend frzl if necessary
			
			// extend start point
			if ( needExtend( frzlCoord.get(0), areaBnds.get(areaName) ) ) {		
				frzlCoord.add(0, extendLineSeg( frzlCoord.get(1), frzlCoord.get(0)));
			}
			
			// extend end point
			if ( needExtend( frzlCoord.get( frzlCoord.size() -1 ), areaBnds.get(areaName) ) ) {		
				frzlCoord.add( extendLineSeg( frzlCoord.get(frzlCoord.size() -2 ), frzlCoord.get(frzlCoord.size() -1 )));
			}
			
			//clip
			Geometry clip = areaBnds.get(areaName).intersection( gf.createLineString( frzlCoord.toArray( new Coordinate[ frzlCoord.size()]) ));
			
			//create a LinearRing that is big enough to contains all FA areas
			LinearRing bigBox = gf.createLinearRing(new Coordinate[]{ 
					new Coordinate(-150., 89.), new Coordinate(-150., 0.),
					new Coordinate( 150., 0.),  new Coordinate( 150., 89.),
					new Coordinate(-150., 89.)});

			//get the segments out of the FA area after clipping
			Geometry diff = gf.createPolygon( bigBox, 
					new LinearRing[]{gf.createLinearRing(areaBnds.get(areaName).getCoordinates())}).intersection(
							gf.createLineString( (Coordinate[]) frzlCoord.toArray( new Coordinate[ frzlCoord.size()]) ));

			//add back small gaps
			clip = addBackSmallGaps(clip, diff);
			
			for( int ii = 0; ii < clip.getNumGeometries(); ii++ ) {
				//snap and reduce points
				ArrayList<Coordinate> pts =	reducePtOpenFrzl( clip.getGeometryN(ii).getCoordinates() );
				
				//ignore small segments.
				if ( getLength( pts.toArray( new Coordinate[ pts.size()]) )/PgenUtil.NM2M > FZLVL_MIN_LEN ) {

					Gfa clippedGfa = points2Frzl( pts, openFrzl );
					if ( clippedGfa != null ){
						GfaRules.assignIssueTime(clippedGfa);
						clippedGfa.setGfaArea( areaName );
						clippedGfa.setGfaVorText(Gfa.buildVorText( clippedGfa ));
						ret.add( clippedGfa );
					}
				}
			}
        }
        
		return ret;
	}
	
	/**
	 * Format(extend, clip, etc.) closed freezing level GFAs.
	 * @param closedFrzl - a closed freezing level GFA
	 * @return - a list of formatted GFAs.
	 */	
	private List<Gfa> formatClosedFrzl( Gfa closedFrzl ){
		//return value
		List<Gfa> ret = new ArrayList<Gfa>();

		//FRZL coordinates, make it close. 
		ArrayList<Coordinate> frzlCoord = new ArrayList<Coordinate>();
		frzlCoord.addAll( closedFrzl.getPoints());
		frzlCoord.add( frzlCoord.get(0));
		
		//clip against FA areas
		HashMap<String, Geometry> areaBnds = GfaClip.getInstance().getFaAreaBounds();
		for ( String areaName : areaBnds.keySet() ) {
			//clip
			Geometry clip = intersectRings( closedFrzl.toPolygon(), areaBnds.get(areaName));
			if ( clip == null ) continue;
			
			//create a linearRing big enough to contain all FA areas.
			LinearRing bigBox = gf.createLinearRing(new Coordinate[]{ 
					new Coordinate(-150., 89.), new Coordinate(-150., 0.),
					new Coordinate( 150., 0.),  new Coordinate( 150., 89.),
					new Coordinate(-150., 89.)});

			//get the segments out of the FA area after clipping
			Geometry diff = intersectRings(closedFrzl.toPolygon(), gf.createPolygon( bigBox, 
					new LinearRing[]{gf.createLinearRing(areaBnds.get(areaName).getCoordinates())})); 

			//add back small gaps.
			clip = addBackSmallGaps(clip, diff);
			
			for( int ii = 0; ii < clip.getNumGeometries(); ii++ ) {
				//snap and reduce points
				List<Coordinate> pts;
				
				if ( clip.getGeometryN(ii) instanceof LinearRing ){
					pts =	reducePtClosedFrzl( clip.getGeometryN(ii).getCoordinates() );
				}
				else {
					pts =	reducePtOpenFrzl( clip.getGeometryN(ii).getCoordinates() );
				}

				//ignore small segments.
				if ( getLength( pts.toArray( new Coordinate[ pts.size()]) )/PgenUtil.NM2M > FZLVL_MIN_LEN ) {

					Gfa clippedGfa = points2Frzl( pts, closedFrzl );
					if ( clippedGfa != null ){
						GfaRules.assignIssueTime(clippedGfa);
						clippedGfa.setGfaArea( areaName );
						clippedGfa.setGfaVorText(Gfa.buildVorText( clippedGfa ));
						ret.add( clippedGfa );
					}
				}
			}
        }
        
		return ret;
	}
	
	/**
	 * Convert the input points to a FRZL 
	 * @param pts - input point list
	 * @param model - GFA attributes as of the return FRZL
	 * @return FRZL
	 */
	private Gfa points2Frzl ( List<Coordinate> pts, Gfa model ){
		if ( pts != null && !pts.isEmpty() ){
			Gfa resultGfa = model.copy();
			
			//check if it is closed or open.
			if ( matchCoords(pts.get(0), pts.get(pts.size()-1))) {
				resultGfa.setClosed(true);
				pts.remove(pts.size()-1);
			}
			else {
				resultGfa.setClosed(false);
			}
			
			resultGfa.setPointsOnly( new ArrayList<Coordinate>( pts) );

			return resultGfa;
		}
		else return null;
	}
	
	/**
	 * Check the relative location of the input point with the input polygon and see
	 * if the FRZl the input point belongs to needs to be extended. 
	 * @param endPt - Coordinate of one end point of an open freezing level
	 * @param geo - FA area polygon
	 * @return if or not the freezing level needs to be extended.
	 */
	private boolean needExtend(Coordinate endPt, Geometry geo ){
		//return value
		boolean ret = false;

		Point jtsPt = gf.createPoint( endPt );
		
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

		// if the point is inside of the polygon and the distance from the point
		// to the polygon boundary is greater than the threshold, return true.
		if ( geo.contains( jtsPt )) {
			Geometry bnd =  geo.getBoundary();
			Coordinate[] nearestPts  = DistanceOp.nearestPoints(bnd, jtsPt);

			gc.setStartingGeographicPoint( endPt.x, endPt.y);
			gc.setDestinationGeographicPoint(nearestPts[0].x, nearestPts[0].y );
			double dist = gc.getOrthodromicDistance() / PgenUtil.NM2M;

			if ( dist > NEAR_FA_BOUND ){
				ret = true;
			}
		}

		return ret;
	}

	/**
	 * Extends a segment. p1, p2 are the original end points of the segment.   
	 * @param p1 - Coordinate. One end point of a segment. 
	 * @param p2 - Coordinate. Another end point of the segment.
	 * @return - Coordinate. The location to be extended.
	 */
	private Coordinate extendLineSeg( Coordinate p1, Coordinate p2 ){
		LineSegment ls = new LineSegment( p1, p2 );

		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		gc.setStartingGeographicPoint( p1.x, p1.y);
		gc.setDestinationGeographicPoint( p2.x, p2.y );
		double distMap = gc.getOrthodromicDistance() / PgenUtil.NM2M;
		
		double dist = EXT_DIST / distMap;

		double x = p2.x + dist * Math.cos( ls.angle() );
		double y = p2.y + dist * Math.sin( ls.angle() );
		
		return new Coordinate( x, y);
	}
	
	/**
	 * A KevLinDev reduction algorithm is applied to the line/polygon to 
	 * potentially reduce the number of points.  There is no explicit number 
	 * of points to be reduced to -- rather this algorithm identifies interim 
	 * points that are within the tolerance value of a straight line and 
	 * excludes them from the return array.                                           
	 *                                                                      
	 * The algorithm can be briefly described as follows:                   
	 *                                                                      
	 *  define lastPoint to be the first point in the input point array      
	 *  loop through the remaining list of points, assigning each to       
	 *           currentPoint to form a line from lastPoint to currentPoint 
	 *   test all points between, but not including lastPoint and currentPoint      
	 *       if the test point's distance from the line is more than the    
	 *        tolerance then add the point before currentPoint to the result array        
	 *        set lastPoint equal to the added point                       
	 *        proceed to the next point in the top-most loop               
	 *       end if                                                         
	 *   end test                                                         
	 *  end loop                                                           
	 * add the last point in the input point array to the output array      
	 *                                                                      
	 * A more complete discussion of the algorithm can be found at:         
	 * http://www.kevlindev.com/tutorials/geometry/simplify_polyline/page2.htm 
	 *                                                                      
	 * The algorithm is taken from the above url, which is copyright 2000-3 
	 * Kevin Lindsey.  This implementation of the algorithm is original work 
	 * based on Mr. Lindsey's algorithm.                                    
	 *                                                                      
	 */	
	private ArrayList<Coordinate> reduceLinePoly( Coordinate[] inPts, double tolerance ){
		ArrayList<Coordinate> outPts = new ArrayList<Coordinate>();
		
		if ( tolerance > 0 && inPts != null ){
			
			if ( inPts.length > 2 ) {
				//Add the first point
				outPts.add( new Coordinate( inPts[0].x, inPts[0].y ));

				int lastPt = 0;
				int curPt = 2;

				boolean lineOk = false;

				while ( curPt < inPts.length ){
					lineOk = checkTolerance( lastPt, curPt, inPts, tolerance );
					if ( !lineOk ){
						outPts.add( new Coordinate ( inPts[curPt-1].x, inPts[curPt-1].y));
						lastPt = curPt - 1;
					}
					curPt++;
				}

				//Add the last point
				outPts.add( new Coordinate( inPts[ inPts.length - 1 ].x, inPts[ inPts.length - 1].y ));
			}
			else {
				
				for ( int ii = 0; ii < inPts.length; ii++ ){
					outPts.add(inPts[ii]);
				}
			}
		}
		
		return outPts;
	}
	
	/**
	 * Check if all points between lastPt and curPt are in tolerance, which means
	 * the distance of every point between lastPt and curPt is less than the input tolerance. 
	 * @param lastPt - index of the end point.
	 * @param curPt - index of the current point.
	 * @param inPts - coordinates of all points.
	 * @param tolerance - screen distance tolerance
	 * @return 
	 */
	private boolean checkTolerance( int lastPt, int curPt, Coordinate[] inPts, double tolerance){
		boolean withinTolerance = true;
		
		LineSegment ls = new LineSegment( inPts[ lastPt ], inPts[ curPt ] );
		
		for ( int ii = lastPt; ii < curPt && withinTolerance; ii++ ){
			
			if ( ls.distancePerpendicular( inPts[ ii ]) > tolerance ){
				withinTolerance = false;
			}
		}
		
		return withinTolerance;
	}
	
	/**
	 * Snaps the input points
	 * @param inPts - coordinates of input points 
	 * @return - snapped result coordinates
	 */
	private Coordinate[] snapFrzl( Coordinate[] inPts ){
		//return inPts;
		Coordinate[] ret = new Coordinate[inPts.length];
		
		for ( int ii = 0; ii < inPts.length; ii++ ){
			ret[ii] = GfaSnap.getInstance().snapOnePt( inPts[ii]);
		}
		
		return ret;
		
	}
	
	/**
	 * Point reduction for closed FRZL
	 * @param inPts - input coordinates
	 * @return - results of point reduction
	 */
	private List<Coordinate> reducePtClosedFrzl( Coordinate[] inPts ){
		
		//Re-order points to clock-wise as the reduce-point method requires.
		boolean reorder = false;
		if ( CGAlgorithms.isCCW( inPts ) ) {
			reorder = true;
			reversePointOrder( inPts );
		}
		
		boolean done = false;
		ArrayList<Boolean> reduceFlg = new ArrayList<Boolean>();

		List<Coordinate> ptList = new ArrayList<Coordinate>(Arrays.asList( inPts ));
		
		//remove first point, since it repeats as the end point
		ptList.remove(0);
		
		double reducePct = REDUCE_INCR_PCT;
		
		while ( !done ){
			
			for ( int ii = 0; ii < ptList.size(); ii++ ){
				reduceFlg.add(true);
			}
			
			//keep the first point and the last point
			reduceFlg.set(0, false);
			reduceFlg.set(reduceFlg.size()-1, false);
			
			if (  !reduceKeepConcav(ptList, reduceFlg, reducePct, REDUCE_INCR_DST ) && reducePct < MAX_REDUCE_PCT ){
				reducePct++;
				reduceFlg.clear();
			}
			else {
				done = true;
			}
		}
		
		//add back first point
		ptList.add(0, ptList.get(ptList.size()-1));
		
		Coordinate[] outPts = ptList.toArray( new Coordinate[ ptList.size() ]);
		
		if ( reorder ){
			outPts = reversePointOrder( ptList.toArray( new Coordinate[ ptList.size() ]) );
		}
		
		List<Coordinate> outList = new ArrayList<Coordinate>(Arrays.asList( outPts ));
		
		outList.remove(0);
		if ( !canBeFormatted( outList ) ){
			outList.remove(1);
		}
		outList.add(0, outList.get(outList.size()-1));
		
		return outList;
		
	}
	
	/**
	 * Point reduction for open-line FRZL
	 * @param inPts - input coordinates
	 * @return - results of point reduction
	 */	
	private ArrayList<Coordinate> reducePtOpenFrzl( Coordinate[] inPts ){
		
		if ( inPts.length > FRZL_MAX_POINTS ){
			//step 1: fit parametric curve to FRZL points
			double[][] linePts = new double[inPts.length][2];

			int ii = 0;
			for ( Coordinate loc : inPts ){
				linePts[ii][0] = loc.x;
				linePts[ii][1] = loc.y;
				ii++;
			}

			double[][] fitCurve = CurveFitter.fitParametricCurve( linePts, 5.0f );

			//step 2: thin the points with reduceLinePoly
			Coordinate[] fitCurvePts = new Coordinate[fitCurve.length];
			for ( ii = 0; ii < fitCurve.length; ii++ ){
				fitCurvePts[ii] = new Coordinate( fitCurve[ii][0], fitCurve[ii][1]); 
			}

			ArrayList<Coordinate> pts = reduceLinePoly( fitCurvePts, TOLERANCE);
			
			ArrayList<Integer> reduceFlg = new ArrayList<Integer>();
			for ( ii = 0; ii < pts.size(); ii++ ){
				reduceFlg.add(1);
			}
			
			reduceFlg.set(0,0);
			reduceFlg.set(reduceFlg.size()-1, 0);
			
			//step 3: reduce to maximum allowable number of points
			CoordinateList cl = new CoordinateList();
			cl.addAll(pts, true);
			
			return reduceLinePoly( snapFrzl(ReducePoints.reduceByAngle(cl, reduceFlg, FRZL_MAX_POINTS).toCoordinateArray()),
					TOLERANCE);
			
		}
		else {
			return reduceLinePoly( snapFrzl( inPts ), TOLERANCE);
		}
		
	}

	private Geometry addBackSmallGaps( Geometry clip, Geometry diff ){
		
		if (!( clip instanceof LineString || clip instanceof MultiLineString ) ) return clip;
		
		//diff 
	//	Geometry diff = new GeometryFactory().createPolygon( bigBox, 
	//			new LinearRing[]{new GeometryFactory().createLinearRing(areaBnd.getCoordinates())}).intersection(
	//					new GeometryFactory().createLineString( frzlCoord ));;
		
		//if (true) return diff;
		if ( diff != null ){
			for( int ii = 0; ii < diff.getNumGeometries(); ii++ ) {
				
				Coordinate[]  gapPts = diff.getGeometryN( ii ).getCoordinates();
				double dist = getLength( gapPts )/PgenUtil.NM2M;
				if ( dist < FZLVL_MAX_GAP ){
					Geometry geo1 = null;
					Geometry geo2 = null;
					Geometry geo3 = null;
					Geometry geo4 = null;
					
					Coordinate[] linePts = null;
					//add back gaps
					for( int jj = 0; jj < clip.getNumGeometries(); jj++ ) {
						
						linePts = clip.getGeometryN( jj ).getCoordinates();
						if ( matchCoords( linePts[ linePts.length-1 ], gapPts[0] ) ) {
							geo1 = clip.getGeometryN( jj ); 
						}
						if ( matchCoords( linePts[ 0 ], gapPts[ gapPts.length - 1 ] )) {
							geo2 = clip.getGeometryN( jj ); 
						}
						if ( matchCoords( linePts[ 0 ], gapPts[0] ) ) {
							geo3 = clip.getGeometryN( jj ); 
						}
						if ( matchCoords( linePts[ linePts.length - 1 ], gapPts[ gapPts.length - 1 ] )) {
							geo4 = clip.getGeometryN( jj ); 
						}
					}
					
					Coordinate[] newLinePts = null;
					
					
					if ( geo1 != null && geo2 != null ) {
						if ( geo1 == geo2 ){ //closed line
							newLinePts = arrayMerge( Arrays.copyOfRange(geo1.getCoordinates(), 1, geo1.getCoordinates().length-1), 
													 Arrays.copyOfRange(gapPts, 1, gapPts.length-1 ), 
													 new Coordinate[]{geo1.getCoordinates()[1]});
							clip = new GeometryFactory().createLinearRing( newLinePts);
							return clip;
						}
						else {
						//	newLinePts = arrayMerge( Arrays.copyOfRange(geo1.getCoordinates(), 0, geo1.getCoordinates().length-1), 
						//							 Arrays.copyOfRange(gapPts, 1, gapPts.length-1 ), 
						//							 Arrays.copyOfRange(geo2.getCoordinates(), 1, geo2.getCoordinates().length) );
							newLinePts = arrayMerge(geo1.getCoordinates(),gapPts, geo2.getCoordinates() );
						}
					}
					else {
						if ( geo1 != null ){
							newLinePts = arrayMerge( Arrays.copyOfRange(geo1.getCoordinates(), 0, geo1.getCoordinates().length-1), 
									 				 Arrays.copyOfRange(gapPts, 1, gapPts.length));
						}

						if ( geo2 != null ){
							newLinePts = arrayMerge( Arrays.copyOfRange(gapPts, 0, gapPts.length-1 ), 
									 				 Arrays.copyOfRange(geo2.getCoordinates(), 1, geo2.getCoordinates().length) );	
						}
					}
					
					if ( geo3 != null && geo4 != null ) {
						reversePointOrder(gapPts);
						if ( geo3 == geo4 ){ //closed line
							newLinePts = arrayMerge( Arrays.copyOfRange(geo3.getCoordinates(), 1, geo3.getCoordinates().length-1), 
									 				 Arrays.copyOfRange(gapPts, 1, gapPts.length-1 ), 
													 new Coordinate[]{geo3.getCoordinates()[1]});
							clip = new GeometryFactory().createLinearRing( newLinePts);
							return clip;
						}
						else {
							newLinePts = arrayMerge( Arrays.copyOfRange(geo4.getCoordinates(), 0, geo4.getCoordinates().length-1), 
									 				 Arrays.copyOfRange(gapPts, 1, gapPts.length-1 ), 
									 				 Arrays.copyOfRange(geo3.getCoordinates(), 1, geo3.getCoordinates().length) );
						}
					}
					else {
						reversePointOrder( gapPts );
						if ( geo3 != null ){

							newLinePts = arrayMerge( Arrays.copyOfRange(gapPts, 0, gapPts.length-1 ), 
													 Arrays.copyOfRange(geo3.getCoordinates(), 1, geo3.getCoordinates().length) );	
						}

						else if ( geo4 != null ){
							newLinePts = arrayMerge( Arrays.copyOfRange(geo4.getCoordinates(), 0, geo4.getCoordinates().length-1), 
					 				 				 Arrays.copyOfRange(gapPts, 1, gapPts.length));
						}
					}
					//create new geometry
					int numLines = clip.getNumGeometries();
					if ( geo1 != null && geo2 != null ) numLines--;
					if ( geo3 != null && geo4 != null ) numLines--;

					LineString[] newLines = new LineString[ numLines ];
					
					newLines[0] = new GeometryFactory().createLineString(newLinePts);
					
					int mm = 1;
					for ( int kk = 0; kk < clip.getNumGeometries(); kk++ ){
						if ( clip.getGeometryN( kk ) != geo1 && clip.getGeometryN(kk) != geo2 
								&& clip.getGeometryN( kk ) != geo3 && clip.getGeometryN(kk) != geo4 ){
							newLines[ mm ] = new GeometryFactory().createLineString( clip.getGeometryN(kk).getCoordinates());
							mm++;
						}
					}

					clip = new GeometryFactory().createMultiLineString(newLines);
					
					geo1 = null;
					geo2 = null;
					geo3 = null;
					geo4 = null;
				}
			}
		}

		return clip;
		
	}
	
	/**
	 * Calculates map length of the line line 
	 * @param pts - input line coordinates
	 * @return - length
	 */
	private double getLength( Coordinate[] pts ){
		
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

		double dist = 0;
		for ( int ii = 0; ii < pts.length -1; ii++ ) {

			gc.setStartingGeographicPoint( pts[ii].x, pts[ii].y  );
			gc.setDestinationGeographicPoint( pts[ii+1].x, pts[ii+1].y );
			
			dist += gc.getOrthodromicDistance();
		}
		
		return dist;
	}
	
	/**
	 * Check if two input points are the same (lat/lon difference less than 0.0001)
	 * @param pt1
	 * @param pt2
	 * @return
	 */
	private boolean matchCoords( Coordinate pt1, Coordinate pt2 ){
		return Math.abs(pt1.x -pt2.x) < 0.0001 && Math.abs( pt1.y - pt2.y ) < 0.0001;
	}
	
	/**
	 * Merge input arrays
	 * @param arrays - arrays to merge
	 * @return - merged array
	 */
	private Coordinate[] arrayMerge(Coordinate[]... arrays) {
	
	    // Determine required size of new array
	 
	    int count = 0;
	    for (Coordinate[] array : arrays)
	    {
	        count += array.length;
	    }
	 
	    // create new array of required class
	 
	    Coordinate[] mergedArray = (Coordinate[]) Array.newInstance(
	       arrays[0][0].getClass(),count);
	 
	    // Merge each array into new array
	 
	    int start = 0;
	    for (Coordinate[] array : arrays)
	    {
	        System.arraycopy(array, 0,
	           mergedArray, start, array.length);
	        start += array.length;
	    }
	    
	    return ( Coordinate[]) mergedArray;
	} 
	
	/**
	 * Reverse the order of the input points
	 * Note: the input array is modified.
	 * @param coords - input points
	 * @return reversed points
	 */
	private Coordinate[] reversePointOrder( Coordinate[] coords ){
		
		for (int ii = 0; ii < coords.length/2; ii++ ){
			Coordinate tmp = coords[ ii ];
			coords[ ii ] = coords[ coords.length - ii - 1];
			coords[ coords.length - ii - 1] = tmp;
		}
		
		return coords;
	}
	
	/**
	 * Check the length of the from-line and see if it fits three lines(65 characters per line). 
	 * @param pts
	 * @return
	 */
	private boolean canBeFormatted( List<Coordinate> pts )  {
		return !( createClosedFromLine( pts ).length() > (MAX_CHARS_PER_LINE - FRZL_INDENT) * (MAX_LINES-0.1) );
	}
	
	/**
	 * Create the from line string for closed FRZL
	 * @param pts
	 * @return
	 */
	private String createClosedFromLine( List<Coordinate> pts ){
		ArrayList<Coordinate> snap = SnapUtil.getSnapWithStation(pts,SnapUtil.VOR_STATION_LIST,10,16, false) ;
		Coordinate[] a = new Coordinate[snap.size()];
		a = snap.toArray(a);
		String fromLine = "BOUNDED BY " + SnapUtil.getVORText(a, "-", "Area", -1, true, false, true );
		return fromLine;
	}
	
	/**
	 * This routine reduces the number of points in a polygon to allow it to
	 * be represented on three 65-character lines of text.  It tries to     
	 * remove allowable points, one at a time, based on the impact their    
	 * Individual removal would have on the size of the polygon.            
	 * Specifically, remove points that increase the size of the polygon the
	 * least,  while not increasing the overall size of the polygon         
	 * by "incrPct" and not allowing any new points to be "incrDst"         
	 * distance from the original polygon points. "reducePct" refers to the 
	 * area percentage increase when a single point is removed from the    
	 * polygon. Point reduction continues until the polygon can be          
	 * represented on three 65-character lines of text, or no more points   
	 * can be removed under the above criteria.                             
	 */

	private boolean reduceKeepConcav( List<Coordinate> inPts, List<Boolean> reduceFlg, double reducePct,
			double maxDist){
		
		boolean done = false;
		boolean canBeFormatted = canBeFormatted( inPts );
		
		Coordinate[] replacementPts ={null,null};
		Coordinate pt1 = null;
		Coordinate pt2 = null;
		double [] sizeDiff = {Double.MAX_VALUE};

		while ( !done && !canBeFormatted ){
			
			double minSizeDiff = Double.MAX_VALUE;
			
			int rmIdx = -1;
			
			for ( int ii = 0; ii < inPts.size(); ii++ ){
				if ( !reduceFlg.get(ii) ) continue;
				if ( removeOnePt( reducePct, maxDist, ii, inPts, reduceFlg, replacementPts, sizeDiff) ){
					if ( sizeDiff[0] < minSizeDiff ) {
						
					minSizeDiff = sizeDiff[0];
					rmIdx = ii;
					if ( replacementPts[0] != null && replacementPts[1] != null ){
						pt1 = replacementPts[0];
						pt2 = replacementPts[1];
						replacementPts[0] = null;
						replacementPts[1] = null;
					}
					else {
						pt1 = pt2 = null;
					}
					
					}
				}
			}
			
			if ( rmIdx < 0 ) done = true;
			else {
				if ( pt1 != null && pt2 != null ){
					inPts.set( ((rmIdx-1)+inPts.size())%inPts.size(), pt1);
					inPts.set( (rmIdx+1)%inPts.size(), pt2);
				}
	//			System.out.println(rmIdx);
				inPts.remove(rmIdx);
				reduceFlg.remove(rmIdx);
				canBeFormatted = canBeFormatted( inPts );
			}
		}
		
		return canBeFormatted;
	}
	
	/**
	 * This function finds the size difference between polygons before and  *
	 * after the removal of a given point P.  If P is outside of the polygon*
	 * without P,  then the replacement points for the points before and    *
	 * after P are calculated.  The removal of a point should not increase  *
	 * the size of polygon by "reducePct" and the replacement points should *
	 * be within "reduceDst" of the origianl points                         *
	 */
	private boolean removeOnePt( double incrPct, double incrDist, int rmIdx, List<Coordinate> inPts, List<Boolean> reduceFlg, 
			Coordinate[] replacementPts, double[] sizeDiff ){
		
		//remove the point
		ArrayList<Coordinate> newPts = new ArrayList<Coordinate>( inPts );
		newPts.remove(rmIdx);
		Coordinate[] newPtArray =  (Coordinate[]) newPts.toArray( new Coordinate[newPts.size()]);
		
		newPts.add( newPtArray[0]);
		GeometryFactory gf = new GeometryFactory();
		SimplePointInRing spir = new SimplePointInRing( gf.createLinearRing( (Coordinate[]) newPts.toArray( new Coordinate[newPts.size()]) ));
		
		int beforeThePt = (( rmIdx - 1 ) + newPtArray.length ) / newPtArray.length;
		int afterThePt = rmIdx / newPtArray.length;
		
		if ( spir.isInside( inPts.get(rmIdx) ) ) {
		}
		else if ( reduceFlg.get((( rmIdx - 1 ) + inPts.size() ) / inPts.size()) && 
				  reduceFlg.get( rmIdx / inPts.size() ) 				//if the neighbor points are remove-able 
				  &&  findReplacementPts( incrDist, rmIdx, inPts.toArray( new Coordinate[ inPts.size()]), replacementPts )){
			
				newPtArray[ beforeThePt ] = replacementPts[0];;
				newPtArray[ afterThePt ] =  replacementPts[1];
				
				//snap????
				
				Coordinate [] snappedPt0 = new Coordinate[1];
				GfaSnap.getInstance().snapPtGFA( (( rmIdx - 1 ) + inPts.size() ) / inPts.size(), 
												(( rmIdx - 1 ) + inPts.size() ) / inPts.size(),
												null, null, Arrays.asList( newPtArray ), 
												false, true, 0.0, snappedPt0 );
				
				newPtArray[ beforeThePt ] = snappedPt0[0];;
				
				Coordinate [] snappedPt1 = new Coordinate[1];
				GfaSnap.getInstance().snapPtGFA( rmIdx / inPts.size(),  rmIdx / inPts.size(),
												null, null, Arrays.asList( newPtArray ), 
												false, true, 0.0, snappedPt1 );
				
				newPtArray[ afterThePt ] =  snappedPt1[0];
				
				
		}
		else return false;
		
		//compare area size
		inPts.add(inPts.get(0));
		double origArea = gf.createPolygon(gf.createLinearRing(inPts.toArray( new Coordinate[ inPts.size()])), null).getArea();
		inPts.remove(inPts.size()-1);
		
		ArrayList<Coordinate> newPtList = new ArrayList<Coordinate>( Arrays.asList( newPtArray )); 
		newPtList.add( newPtArray[0]);
		double newArea = gf.createPolygon(gf.createLinearRing( newPtList.toArray( new Coordinate[ newPtList.size()])), null).getArea();
		 sizeDiff[0] =  newArea - origArea;
		 
		 if ( sizeDiff[0] / origArea * 100 <= incrPct ){
			 return true;
		 }
		
		return false;
	}

	/*
	 * This function finds the size difference between polygons before and  *
	 * after the removal of a given point P.  If P is outside of the polygon*
	 * without P,  then the replacement points for the points before and    *
	 * after P are calculated.  The removal of a point should not increase  *
	 * the size of polygon by "reducePct" and the replacement points should *
	 * be within "reduceDst" of the origianl points                         *
	 */
	private boolean findReplacementPts( double maxDistance, int rmIdx, Coordinate[] inPts, Coordinate[] replacementPts){
		
		Coordinate ptB = inPts[((rmIdx-1)+inPts.length)%inPts.length];
		Coordinate ptBB = inPts[((rmIdx-2)+inPts.length)%inPts.length];
		
		Coordinate ptA = inPts[ (rmIdx + 1) % inPts.length];
		Coordinate ptAA = inPts[ (rmIdx + 2) % inPts.length];
		
		Coordinate pt = inPts[ rmIdx ];
		
		// M: intersection of pb-pbb and p-pa
		Coordinate ptM = lineIntersection( ptB, ptBB, pt, ptA );
		if ( ptM == null || 
				(   ptM.x >= Math.min(pt.x, ptA.x) && ptM.x <= Math.max(pt.x, ptA.x) 
				 && ptM.y >= Math.min(pt.y, ptA.y) && ptM.y <= Math.max(pt.y, ptA.y) ) )
		{
			return false;
		}
		
		// N: intersection of pa-paa and p-pb
		Coordinate ptN = lineIntersection( ptA, ptAA, pt, ptB );
		if ( ptN == null || 
				(   ptN.x >= Math.min(pt.x, ptB.x) && ptN.x <= Math.max(pt.x, ptB.x) 
				 && ptN.y >= Math.min(pt.y, ptB.y) && ptN.y <= Math.max(pt.y, ptB.y) ) )
		{
			return false;
		}
		
		Coordinate ptL = new Coordinate();
		if ( ptA.x == ptB.x ){
			ptL.x = pt.x;
			ptL.y = pt.y - (ptA.y - ptB.y);
		}
		else {
			ptL.x = 0.;
			ptL.y = pt.y - (ptA.y - ptB.y) / (ptA.x - ptB.x) * pt.x; 
		}
		
		// pt 1: intersection of pb-pbb and pt-ptL
		replacementPts[0] = lineIntersection(ptB, ptBB, pt, ptL );
		if ( replacementPts[0] == null ) return false;
		
		// pt 2: intersection of pa-paa and pt-ptL
		replacementPts[1]= lineIntersection(ptA, ptAA, pt, ptL );
		if ( replacementPts[1] == null ) return false;
		
		//distances ptB-->pt1 and ptA-->pt2 should be less maxDist
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		
		gc.setStartingGeographicPoint( ptB.x, ptB.y);
		gc.setDestinationGeographicPoint( replacementPts[0].x, replacementPts[0].y );
		double distMap = gc.getOrthodromicDistance() / PgenUtil.NM2M;
		if ( distMap > REDUCE_INCR_DST ) return false;
		
		gc.setStartingGeographicPoint( ptA.x, ptA.y);
		gc.setDestinationGeographicPoint( replacementPts[1].x, replacementPts[1].y );
		distMap = gc.getOrthodromicDistance() / PgenUtil.NM2M;
		if ( distMap > REDUCE_INCR_DST ) return false;
		
		return true;
	}
	
	/**
	 * This method finds the intersection of line (p1, p2) and line (p3, p4).
	 * @param p1 
	 * @param p2
	 * @param p3
	 * @param p4
	 * @return
	 */
	private Coordinate lineIntersection( Coordinate p1, Coordinate p2, Coordinate p3, Coordinate p4 ){
		double d = (p1.x -p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x);
		if ( d <= 0.000001 ) return null;
		else {
			return new Coordinate( ((p3.x - p4.x)*(p1.x*p2.y - p1.y*p2.x) - (p1.x - p2.x)*(p3.x*p4.y - p3.y*p4.x))/d,
					((p3.y - p4.y)*(p1.x*p2.y - p1.y*p2.x) - (p1.y-p2.y)*(p3.x*p4.y-p3.y-p4.x))/d );
		}
	}
	
	
	private Geometry intersectRings(Geometry g1, Geometry g2){
		Geometry clipc = g1.intersection( g2.getBoundary());
		Geometry clipx = g1.intersection( g2 );

		Geometry ret = null;
		try {
			ret = clipx.getBoundary().difference( clipc );
		}
		catch ( Exception e ){
			
		}
		return ret;
	}
}
