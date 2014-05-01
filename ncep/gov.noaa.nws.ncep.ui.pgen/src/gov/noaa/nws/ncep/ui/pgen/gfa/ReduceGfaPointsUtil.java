/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.ReduceGfaPointsUtil
 * 
 * June 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaSnap;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.quadtree.Quadtree;

/**
 * Utility of Reduce points of a polygon.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/12/2011     430      Q.Zhou      Initial creation. (from cgrreducepts.c)
 * 05/23/2011     430      Q.Zhou      Add intersection to polygon before snap, calculate area after snap
 * 06/27/2011     430      Q.Zhou      Added map and device system transaction
 * 07/06/2011     430      Q. Zhou     Preload station table.
 * 07/11        #450        G. Hull     NcPathManager
 *
 * </pre>
 * 
 * @author Q.Zhou
 * @version 1.0
 */
public class ReduceGfaPointsUtil {
	
	static int NM2M = 1852;
	static int LENFROM = 65; //35; // max # of characters on each formatted line
	static int NUMFROM = 3;
	static double RMISSED = -9999.0;
	static double GDIFFD = 0.000001;
	public static final String dirs[] = {"N","NNE","NE","ENE","E","ESE","SE","SSE",
        "S","SSW","SW","WSW","W","WNW","NW","NNW","N"};
	
	static StationTable stationTable;
	
	public static StationTable getStationTable()  {
		stationTable = PgenStaticDataProvider.getProvider().getVorTbl();
		return stationTable;
	}
	
	/* This function determines whether a sequence of polygon points can	
	 * be formatted on three lines of text of LENFROM characters each.	
	 * The representation is that of a prefixed 'FROM' or 'BOUNDED BY' line 
	 * where the lat,lon points are converted to VOR proximity, i.e., '20SSE XYZ'.	
	 * 
	 * @param 	xyList - polygon points List<Coordinate>
	 * @param 	prefix - prefix of format line
	 * @return 	true -  The fromLine can be formatted, so continue to reduce
	 * 			false		
	 */ 					
	public static boolean canFormatted(List<Coordinate> xyList, String prefix)  {
		String fromLine = "";
		String formattedLine = "";
		String separator = "";

		List<Coordinate> xyCopyList = null;
		List<String> vorList = new ArrayList<String>();
		 
		if (prefix.equalsIgnoreCase("FROM")) {
			separator = " TO ";
		}
		else if (prefix.equalsIgnoreCase("BOUNDED BY")) {
			separator = "-";
		}

		fromLine = prefix + " ";

		int i = 0;
		int xySize = xyList.size();

		if (xyList != null && xyList.size() >3) {
			xyCopyList = new ArrayList<Coordinate>();
			for ( Coordinate c : xyList ) {
	    		xyCopyList.add( c );
			}
		}
		else {
			return true;
		}
		
		xyCopyList  = getNorthMostOrder(xyCopyList);

		GeodeticCalculator gc =  new GeodeticCalculator(DefaultEllipsoid.WGS84);
		for ( i = 0; i < xySize; i++) {
			Coordinate loc = xyCopyList.get(i);
			Station theStation = null;
			if (stationTable != null)
				theStation = stationTable.getNearestStation(loc);
	
// 			if standalone, call this:
//			if (theStation == null) 
//				theStation = getNearestStation(loc); //stationTable.

			if (theStation == null)
				return true; //this true means can't do anything			

			//Another method: SigmetInfo.getVORText(Coordinate[] coors, String vorConnector, String lineType, int numPerLines, boolean isSnapped, boolean useJTS)
			//SigmetInfo.getVORText(xyList.toCoordinateArray(), " TO ", prefix, 65, true, false);
						
			String vor = getRelative(loc, theStation ).trim();
			vor = getRoundedVor(vor);
			vorList.add(vor);
			
		}	

		for (int n = 0; n <vorList.size(); n++)
			fromLine += vorList.get(n) + separator; // 4 more char
	
		if (fromLine.length() > LENFROM*NUMFROM)
			return false;
		else
			formattedLine = getFormattedLine(vorList, prefix);

		if (formattedLine.length() > LENFROM*NUMFROM +2) // 1st line "\n" & 2nd "\n"
			return false;		
		
		return true;
	}
	

	
	/**
	 * Find the size difference between polygons before and	after the removal of a given point P.
	 * If P is outside of the polygon without P,  then the replacement points for the points before and
	 * after P are calculated.  The removal of a point should not increase the size of polygon by "reducePct"
	 * and the replacement points should be within "reduceDst" of the origianl points	
	 * 
	 * @param xyList - 	  polygon points List<Coordinate>
	 * @param reduceFlg - polygon points reduce flag
	 * @param rmFlg -     flag to indicate how to remove the point P	
	 *                       0 - P is inside or on border of the original polygon
	 *                       1 - P is outside & replacement points available
	 *                      -1 - P is outside & no replacement points avail.
	 *                      -2 - size increase exceeds reducePct
	 *        areaDiff -  area difference caused by remove this point
	 *        index  -    index of removing polygon point
	 * @param incrPct -   maximum increase in size allowed
	 * @param incrDst -   maximum distance allowed away from the polygon
	 * @return Coordinate list - normal - replacement point Ob & Oa list
	 * 		   					 null -   no replacement points found or no need to find
	 */
	public static List<Coordinate> findRemovePt(List<Coordinate> xyList, List<Integer>reduceFlg, List<Integer> rmFlg, 
			List<Double> areaDiff, int index, double incrPct, double incrDst) {
		
		List<Coordinate> resultList = new ArrayList<Coordinate>();
		List<Coordinate> tmpList = null;
		List<Coordinate> xyCloseList = null;
		double sizeDiff = 0;
		int xySize = xyList.size();
	
		/*
	     *  Make copies. Temporary remove point "index".
	     */
		if (xyList != null && xyList.size() >3) {
			xyCloseList = new ArrayList<Coordinate>();
			for ( Coordinate c : xyList ) {
	    		xyCloseList.add( c );
			}
			tmpList = new ArrayList<Coordinate>();
			for ( Coordinate c : xyList ) {
				tmpList.add( c );
			}
		}
		else 
			return null;
		
		tmpList.remove(index);
		
		/*
	     *  calculate areas
	     */
		Double xyArea = getArea(xyCloseList);
		Double tmpArea = getArea(tmpList);
		
		/*
	     *  If point "index" is outside, remove it the area become smaller. i.e. (tmpArea < xyArea). If so, replace the point  
	     *  before and after the point "index" if both of them are reduce-able AND the replacement points are available.
	     */		
		int iib = ( (index - 1) + xySize ) % xySize;
	    int iia = (index + 1) % xySize;
	    
	    if (tmpList.get(0).equals(tmpList.get(tmpList.size()-1)))
			tmpList.remove(tmpList.size() -1);
//	    System.out.println("area1 "+ index +" " +tmpArea +" " +xyArea +" "+(tmpArea-xyArea) +" "+100*(tmpArea-xyArea)/xyArea);
		
		if (tmpArea < xyArea) {
			if (reduceFlg == null || (reduceFlg.get(iib)==1 && reduceFlg.get(iia)==1 ) ) { //only to outside point
				Coordinate coA = findReplacePtsA(xyList, index, incrDst);			
				Coordinate coB = findReplacePtsB(xyList, index, incrDst);	
				
				if (coA == null || coB == null) {
					rmFlg.set(index, -1);				
					return null;
				}
				else {
					/* 
					 * coA and coB distance should be greater than 30 nautical miles, otherwise don't remove.
					 * This rule was not in cgrreducepts.c, but should be added in.
					 */
					 Coordinate[] OaArray = {coA};
					 Coordinate[] ObArray = {coB};
					 Coordinate[] trans1 =  PgenUtil.gridToLatlon( OaArray);
					 Coordinate[] trans2 =  PgenUtil.gridToLatlon( ObArray);
					 if (Math.abs(trans1[0].x) > 180 || Math.abs(trans1[0].y) > 90)
					    	return null;	
					 
					 double dist = GfaSnap.getInstance().distance(trans1[0], trans2[0]);
					 if (dist/NM2M  <= 30) {
						coA = null;
						coB = null;
						rmFlg.set(index, -1);				
						return null;
					}
					
					resultList.add(0, coB);
					resultList.add(1, coA);
					rmFlg.set(index, 1); //when nearby point changes, rmFlg may change
				}
			
				// get new polygon size				
				int tmpSize = tmpList.size();
				int ib = ( (index - 1) + tmpSize ) % tmpSize;
			    int ia = (index ) % tmpSize;
			    
				tmpList.set(ia, coA);
				tmpList.set(ib, coB);
				

				Coordinate[] snappedA = new Coordinate[1];
				Coordinate[] snappedB = new Coordinate[1];
				ArrayList<Coordinate> transList = PgenUtil.gridToLatlon( (ArrayList)tmpList);
//				System.out.println("cbCa " +transList.get(ib) + " "+transList.get(ia));
				
				GfaSnap.getInstance().snapPtGFA(ia, ia, null, null, 
						transList, true, true, 3.0, snappedA);
				GfaSnap.getInstance().snapPtGFA(ib, ib, null, null, 
						transList, true, true, 3.0, snappedB);
				
				transList.set(ib, snappedB[0]);
				transList.set(ia, snappedA[0]);
//				System.out.println("snapBA " +transList.get(ib) + " "+transList.get(ia));
				tmpList = PgenUtil.latlonToGrid( (ArrayList) transList);

			}
			else {
				rmFlg.set(index, -1);
				return null;
    		}
		}
		else { // 0,1 are all removable. 0 - inside polygon
			rmFlg.set(index, 0);
		}
		
//		System.out.println("area "+ getArea(tmpList) +" " +xyArea +" "+(getArea(tmpList)-xyArea) +" "+100*(getArea(tmpList)-xyArea)/xyArea);
		if (rmFlg.get(index) >= 0) {
			sizeDiff = Math.abs(xyArea - getArea(tmpList));
			areaDiff.set(index, sizeDiff);
			
			if ( incrPct >= 0.0 && ((sizeDiff/xyArea) * 100) > incrPct ) {
				rmFlg.set(index, -2);  
			}
		}
		
		if (rmFlg.get(index) > 0)  
			return resultList;
		else 
			return null;
	}
	
	/**
	 * Find replacement point Ob according to following rule 
	 * @param 	xyList - polygon points List<Coordinate>
	 * @param 	index  - index of polygon point 
	 * @param 	incrDst - maximum distance allowed away from the polygon
	 * @return  Coordinate Ob. normal -	replacement point Ob
	 * 		   				   null - no replacement points found or distance is greater than incrDst
     *  Definition:
     *    P   - point to be eliminated
     *    Pb  - point before P;    Pbb - point before Pb
     *    Pa  - point after P;     Paa - point after Pa
     *    L   - line through P and parallel to line Pb-Pa 
     *    M   - intersetion point of line Pb-Pbb with P-Pa
     *    N   - intersetion point of line Pa-Paa with P-Pb
     *    Ob  - intersetion point of line Pb-Pbb with line L 
     *    Oa  - intersetion point of line Pa-Paa with line L 
     * 
     *    After replacing Pb with O1 and Pa with O2, the area covered
     *    by triangle Pb-P-Pa must be fully contained within the 
     *    trapezoid Pbb-O1-O2-Paa. SO M cannot falls on segment P-Pa 
     *    AND N cannot falls on segment P-Pb. 
     */
	public static Coordinate findReplacePtsB(List<Coordinate> xyList, int index, double incrDst) {
		Coordinate Ob = null;
		
		int xySize = xyList.size();
		int ib   = ((index - 1) + xySize) % xySize;
		int ibb  = ((index - 2) + xySize) % xySize;
		int ia   = (index + 1)  % xySize;
		
		Coordinate a = xyList.get(ia);
		Coordinate p = xyList.get(index);
		Coordinate b = xyList.get(ib);

		
		/* 
	     *   M - intersection of Pb-Pbb with P-Pa - falls on P-Pb?
	     */		
		LineSegment bb = new LineSegment(xyList.get(ibb), xyList.get(ib));
		LineSegment ap = new LineSegment(xyList.get(ia), xyList.get(index));
		
		Coordinate m = getIntersection(bb, ap);//intersecN.getCoordinate();
		
		if (m != null)
			if (m.x >= Math.min(a.x, p.x) && m.x <= Math.max(a.x, p.x) 
				&& m.y >= Math.min(a.y, p.y) && m.y <= Math.max(a.y, p.y))
				return null;
				
		/* 
	     *   Ob - intersection of Pb-Pbb with line L
	     *   l - a point on l that l.x = 0
	     */
		Coordinate l = new Coordinate();
	    if (xyList.get(index).x !=0) {
			l.x = 0.0;  //get same point if coordinate(index).x =0
			l.y = p.y - ( ( a.y - b.y ) / ( a.x - b.x ) ) * p.x;
		}
		else {
			l.x = 1.0;
			l.y = p.y - ( ( a.y - b.y ) / ( a.x - b.x ) ) * (p.x - l.y);
		}
	    
	    // method lp.intersects(bb) can't do extension line intersection. use following.    
	    LineSegment lp = new LineSegment(l, xyList.get(index));
	    Ob = getIntersection(bb, lp);
	    if (Ob == null)
	    	return null;

		/*
	     *   Pb->O1 and Pa->O2 should not exceed maxDst. 
	     */
		if ( incrDst > 0 ) {

			 Coordinate[] ObArray = {Ob};
			 Coordinate[] PtArray = {xyList.get(ib)};
			 Coordinate[] trans1 =  PgenUtil.gridToLatlon( ObArray);
			 Coordinate[] trans2 =  PgenUtil.gridToLatlon( PtArray);
			 if (Math.abs(trans1[0].x) > 180 || Math.abs(trans1[0].y) > 90)
			    	return null;	
			 
			 double dist = GfaSnap.getInstance().distance(trans1[0], trans2[0]);
//           System.out.println("distb "+dist +" "+ trans1[0] +" " +trans2[0]);
			if ( dist/NM2M > incrDst ) 	 
		         return null; 
		}
		
		return Ob;
	}
	
	/** Find replacement point Oa according to above rule
	 * @param 	xyList - polygon points List<Coordinate>
	 * @param 	index  - index of polygon point 
	 * @param 	incrDst - maximum distance allowed away from the polygon
	 * @return  Coordinate Oa. normal -	replacement point Oa
	 * 		   				   null - no replacement points found or distance is greater than incrDst
	 */
	public static Coordinate findReplacePtsA(List<Coordinate> xyList, int index, double incrDst) {
		Coordinate Oa = null;
		
		int xySize = xyList.size();
		int ib   = ((index - 1) + xySize) % xySize;
		int ia   = (index + 1)  % xySize;
		int iaa  = (index + 2)  % xySize;
		
		Coordinate b = xyList.get(ib);
		Coordinate p = xyList.get(index);
		Coordinate a = xyList.get(ia);
		
		/* 
	     *   N - intersection of Pa-Paa with P-Pb - falls on P-Pb?
	     */
		LineSegment aa = new LineSegment(xyList.get(iaa), xyList.get(ia));
		LineSegment bp = new LineSegment(xyList.get(ib), xyList.get(index));
		
		Coordinate n = getIntersection(aa, bp);//intersecN.get();
		if (n != null)
			if (n.x >= Math.min(b.x, p.x) && n.x <= Math.max(b.x, p.x) 
				&& n.y >= Math.min(b.y, p.y) && n.y <= Math.max(b.y, p.y))
				return null; //no replacement
		
		/* 
	     *   Oa - intersection of Pa-Paa with line L
	     *   l - a point on l that l.x = 0
	     */
		Coordinate l = new Coordinate();
		if (xyList.get(index).x !=0) {
			l.x = 0.0;  //get same point if coordinate(index).x =0
			l.y = p.y - ( ( a.y - b.y ) / ( a.x - b.x ) ) * p.x;
		}
		else {
			l.x = 1.0;
			l.y = p.y - ( ( a.y - b.y ) / ( a.x - b.x ) ) * (p.x - l.y);
		}
	    
	    LineSegment lp = new LineSegment(l, xyList.get(index));
	    Oa = getIntersection(aa, lp);
	    if (Oa == null)
	    	return null;

		/*
	     *   Pb->O1 and Pa->O2 should not exceed maxDst. 
	     */
		if ( incrDst > 0 ) {
//			 double dist1 = Oa.distance(xyList.get(ia)); This method is not correct.
//			 GeodeticCalculator gc =  new GeodeticCalculator(); This method might be correct also
//			 gc.setStartingGeographicPoint(Oa.x, Oa.y);
//			 gc.setDestinationGeographicPoint(xyList.get(ia).x, xyList.get(ia).y);

			 Coordinate[] OaArray = {Oa};
			 Coordinate[] PtArray = {xyList.get(ia)};
			 Coordinate[] trans1 =  PgenUtil.gridToLatlon( OaArray);
			 Coordinate[] trans2 =  PgenUtil.gridToLatlon( PtArray);
			 if (Math.abs(trans1[0].x) > 180 || Math.abs(trans1[0].y) > 90)
			    	return null;
			 
			 double dist = GfaSnap.getInstance().distance(trans1[0], trans2[0]);
//			 System.out.println("dista "+dist  +" "+ trans1[0] +" " +trans2[0]);
			 if ( dist/NM2M > incrDst )
				 return null; 
		}
		
		return Oa;
	}
	
	/**
	 * Find the intersection when two line segments extend. cgrsegint
	 * LineIntersector and lineSegment.lineIntersection are not supported in current version
	 * @param 	line1   - Line Segment
	 * @param 	line2   - Line Segment
	 * @return  coordinate - coordinate or coordinate(RMISSED,RMISSED) if they are parallel 
	 */
	public static Coordinate getIntersection(LineSegment line1,  LineSegment line2) {
		Coordinate intersec = null;
		double x = 0.0;
		double y = 0.0;
		double m1 = 0.0;
		double b1 = 0.0;
		double m2 = 0.0;
		double b2 = 0.0;
		
		double x1a = line1.getCoordinate(0).x;
		double x1b = line1.getCoordinate(1).x;
		double y1a = line1.getCoordinate(0).y;
		double y1b = line1.getCoordinate(1).y;
		double x2a = line2.getCoordinate(0).x;
		double x2b = line2.getCoordinate(1).x;
		double y2a = line2.getCoordinate(0).y;
		double y2b = line2.getCoordinate(1).y;
		
		/*
	     *  Check for vertical first segment and compute (x,y) intersect.
	     */
	    if ( x1a == x1b ) {
	    	x = x1a;
	    	if ( x2a == x2b )  return null; //new Coordinate(RMISSED, RMISSED);
	    	m2 = (y2b-y2a) / (x2b-x2a);
	    	b2 = y2a - m2 * x2a;
	    	y = m2 * x + b2;
	    }

	    /*
	     *  Check for vertical second segment and compute (x,y) intersect.
	     */
	    else if ( x2a == x2b ) {
	    	x = x2a;
	    	if ( x1a == x1b )  return null;
	    	m1 = (y1b-y1a) / (x1b-x1a);
	    	b1 = y1a - m1 * x1a;
	    	y = m1 * x + b1;
	    }

	    /*
	     *  Finally compute (x,y) intersect for all other cases.
	     */
	    else {
	    	m1 = (y1b-y1a) / (x1b-x1a);
	    	b1 = y1a - m1 * x1a;

	    	m2 = (y2b-y2a) / (x2b-x2a);
	    	b2 = y2a - m2 * x2a;

	    	if (m1 == m2)  {
	    		x = RMISSED;
	    		y = RMISSED;
		    }
			else  {
				if ( m1 == 0.0)  {
					x = ( b2 - y1a ) / ( - m2 );
					y = y1a;
				}
		    	else if ( m2 == 0.0 )  {
		    		x = ( y2a - b1 ) / ( m1 );
		    		y = y2a;
		    	}
		    	else  {
		    		x = ( b2 - b1 ) / ( m1 - m2 );
		    		y = m1 * x + b1;
		    	}
			}
	    }
	    
	    if (x == RMISSED || y == RMISSED )
	    	return null;
	    
	    intersec = new Coordinate(x, y);
		return intersec;
	}
	
	/**
	 * get area
	 * @param xyList - polygon points
	 * @return area 
	 */
	public static Double getArea(List<Coordinate> xyList) {
		Double xyArea = 0.0;
		int xySize = xyList.size();
		
//		/*
//	     * Compute the summation of the area and the first moments. Code form cgrcentroid.c
//	     */		
//		double atmp = 0.0;
//		double ai = 0.0;
//		if (xyList.size() < 3)
//			return 0.0;
//	    for ( int i = xySize-1, j = 0; j < xySize; i = j, j++ )  {
//
//	    	ai = xyList.get(i).x * xyList.get(j).y - xyList.get(j).x * xyList.get(i).y;
//	    	atmp += ai;
//
//	    }
//
//	    /*
//	     * Compute the area of the polygon.
//	     */
//	    xyArea = Math.abs ( atmp / 2.0F );
	    
		
		List<Coordinate> xyCloseList = null;
		
		if (xyList != null) {
			xyCloseList = new ArrayList<Coordinate>();
			for ( Coordinate c : xyList ) {
	    		xyCloseList.add( c );
			}					
		}
		
		if ( !xyCloseList.get(0).equals(xyCloseList.get(xySize-1)) )
			xyCloseList.add(xySize, xyCloseList.get(0));
		
		GeometryFactory gf = new GeometryFactory();
		Coordinate[] coorArray = new Coordinate[xyCloseList.size()];
		for(int i=0; i<xyCloseList.size(); i++){
			coorArray[i] = xyCloseList.get(i);
		}
		
		Polygon xyClosePoly = gf.createPolygon(gf.createLinearRing(coorArray), null);
		xyArea = xyClosePoly.getArea();
		
		return xyArea;
	}
	
	/**
	 * get relative VOR (with dir and dist) of a point with a station
	 * @param pt - a point
	 * @param st - a station
	 * @return vor string 
	 */
	public static String getRelative(Coordinate pt, Station st ){
		
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);

		gc.setStartingGeographicPoint(st.getLongitude(), st.getLatitude() );
		gc.setDestinationGeographicPoint(pt.x, pt.y  );

		long dist = Math.round( gc.getOrthodromicDistance()/NM2M);
		long dir = Math.round(gc.getAzimuth());
		if ( dir < 0 ) dir += 360;
		//String str = String.format("%1$4d%2$s%3$%5s", dist, dirs[(int)Math.round(dir/22.5)], st.getStid()); not used.
		String str = dist + dirs[(int)Math.round(dir/22.5)]+ " " + st.getStid() ;
		return str;
	}

	/**
	 * called by stand alone package
	 *
	 */
	public static Station getNearestStation( Coordinate loc) { //standalone
		double DIST = 1.0;
		double min = Double.MAX_VALUE;
		Station found = null;
		
		String path = ReduceGfaPointsUtil.class.getProtectionDomain().getCodeSource().getLocation().toString(); // file:/usr1/qzhou/to11dr3/workspace/gov.noaa.nws.ncep.pgen/bin/
		String subPath = path.substring(5, path.length()-1);
		subPath = subPath.substring(0, subPath.lastIndexOf("/"));
		String tbl = subPath + "/build.edex/esb/data/utility/cave_ncep/base/stns/vors.xml"; 
		
		StationTable stationTable = new StationTable(tbl);
		Station s = stationTable.getNearestStation(loc);
		
		List<Station> listOfItems = stationTable.getStationList();
		
		GeodeticCalculator gc =  new GeodeticCalculator();
		gc.setStartingGeographicPoint(loc.x, loc.y);
		
		Envelope searchEnv = new Envelope(loc.x-DIST, loc.x+DIST, loc.y-DIST, loc.y+DIST);
		
		Quadtree stTree = new Quadtree(); 
		for ( Station st : listOfItems ) {
			Coordinate c = new Coordinate(st.getLongitude(), st.getLatitude());
			Envelope env = new Envelope(c.x-DIST, c.x+DIST, c.y-DIST, c.y+DIST);
			stTree.insert(env, st);
		}
		
		List<?> res = stTree.query(searchEnv);
		if (res == null || res.isEmpty()) {
			DIST = 5.0;
			searchEnv = new Envelope(loc.x-DIST, loc.x+DIST, loc.y-DIST, loc.y+DIST);
			for ( Station st : listOfItems ) {
				Coordinate c = new Coordinate(st.getLongitude(), st.getLatitude());
				Envelope env = new Envelope(c.x-DIST, c.x+DIST, c.y-DIST, c.y+DIST);
				stTree.insert(env, st);
			}
			res = stTree.query(searchEnv);
			if (res == null || res.isEmpty()) 
				return null;
		}
		
		Iterator<?> iter = res.iterator();
		
		while ( iter.hasNext() ) {
			Station st = (Station)iter.next();
			Coordinate where = new Coordinate(st.getLongitude(), st.getLatitude());
			gc.setDestinationGeographicPoint(where.x, where.y);
			
			double dist = gc.getOrthodromicDistance();
			if ( dist < min ) {
				min = dist;
				found = st;
			}			
		}
		
		return found;
	}
	
	/** This function returns a "from" line given a series of VORs.
	 *  The polygon points can be formatted on three lines of text of LENFROM characters each.
	 *  The representation is that of a prefixed 'FROM' or 'BOUNDED BY' line.
	 *  The end-of-line string '\n' is added to the first two lines, but that don't take space.
	 * 
	 * @param vorList - vor list
	 * @param prefix - prefix of fromLine. "FROM" or "BOUNDED BY"
	 */
	public static String getFormattedLine( List<String> vorList, String prefix) {
		String formattedLine = "";
		String separator = "";
		String line1 = "";
		String line2 = "";
		String line3 = "";
		String linePart1 = "";
		String linePart2 = "";
			
		if (prefix.equalsIgnoreCase("FROM")) {
			separator = " TO ";
		}
		else if (prefix.equalsIgnoreCase("BOUNDED BY")) {
			separator = "-";
		}
		 
		/*
		 * remove duplicate. clo_cleanFmLine
		 */
		for (int i = 0; i < vorList.size()-1; i++) //size is changing
			if (vorList.get(i).toString().equalsIgnoreCase(vorList.get(i+1).toString())) {
				vorList.remove(i);
				i -= 1; //vorList reduced 1
			}
		
		/* 
		 * put vors to a string line1. If line1.size >64, move extra characters to next line.
		 */
		line1 = prefix + " ";
		for (int i = 0; i < vorList.size(); i++)
			line1 += vorList.get(i) + separator;
			

		/* 
		 * first formatted line:  
		 * Divide line1; Add first vor & \n to end of line, if the line characters are less than 64; 
		 * move rest of characters from last " " (and fill " " to 65) to next line if the line characters are great than 64
		 */
		linePart1 = "";
		linePart2 = "";
		if (line1.length() > LENFROM) {
			linePart1 = line1.substring(0, LENFROM); //0-64
			linePart2 = line1.substring(LENFROM);
		}
		else {
			formattedLine = formattedLine + line1 +vorList.get(0) + "\n"; // last vor need to be fist vor
			return formattedLine;
		}
		
		if (linePart1.length() !=0 && (linePart1.endsWith(" ") || linePart1.endsWith("-")) ) { //break on " "
			formattedLine = formattedLine + linePart1 + "\n"; 
			line2 = linePart2;
		}
		else if (linePart2.length() !=0 && (linePart2.startsWith(" ") || linePart2.startsWith("-")) ) { //break on " "
			formattedLine = formattedLine + linePart1 + "\n"; 
			if (linePart2.startsWith("-"))
				line2 = linePart2;
			else
				line2 = linePart2.substring(1);
		}
		else {
			line2 = linePart2;
			int lastEmp = linePart1.lastIndexOf(" ");
			int lastHyph = linePart1.lastIndexOf("-");
			String tmp = linePart1.substring(0, Math.max(lastEmp, lastHyph)+1);	//include " " or "-"	
			for (int i = Math.max(lastEmp, lastHyph)+1; i < LENFROM; i++)
				tmp += " ";
			formattedLine = formattedLine + tmp + "\n"; //this line is not the end
			line2 = (linePart1.substring(Math.max(lastEmp, lastHyph) +1 ) + line2);
		}
		
		/* 
		 * 2nd formatted line
		 */
		linePart1 = "";
		linePart2 = "";
		if (line2.length() > LENFROM) {
			linePart1 = line2.substring(0, LENFROM); //0-64
			linePart2 = line2.substring(LENFROM);
		}
		else {
			formattedLine = formattedLine + line2 +vorList.get(0) + "\n"; // last vor need to be fist vor
			return formattedLine;
		}
		
		if (linePart1.length() !=0 && (linePart1.endsWith(" ") || linePart1.endsWith("-")) ) { 
			formattedLine = formattedLine + linePart1 + "\n"; 
			line3 = linePart2;
		}
		else if (linePart2.length() !=0 && (linePart2.startsWith(" ") || linePart2.startsWith("-")) ) { 
			formattedLine = formattedLine + linePart1 + "\n";
			if (linePart2.startsWith("-"))
				line3 = linePart2;
			else
				line3 = linePart2.substring(1);
		}
		else {
			line3 = linePart2;
			int lastEmp = linePart1.lastIndexOf(" ");
			int lastHyph = linePart1.lastIndexOf("-");
			String temp = linePart1.substring(0, Math.max(lastEmp, lastHyph)+1);			
			for (int i = Math.max(lastEmp, lastHyph)+1; i < LENFROM; i++)
				temp += " ";
			formattedLine = formattedLine + temp + "\n";
			line3 = linePart1.substring(Math.max(lastEmp, lastHyph) +1) + line3;
		}
			
		/* 
		 * 3rd formatted line might be great or less than 65
		 */
		formattedLine = formattedLine + line3 + vorList.get(0) + "\n"; 	
			
		
		return formattedLine;
	}
	
	/*
	 * Round the distance in the vor to nearst 10. 5 will round to 10.
	 * @param vor - vor 
	 * @return vor - 
	 */
	private static String getRoundedVor(String vor) {
		String numStr = "";
		int i = 0;
		int rounded = 0;
		int vorSize = vor.length();
		
		if (vor != null && !vor.isEmpty()) {
			for ( i = 0; i < vorSize; i++) {
				String s = vor.substring(i, i+1);
				if (s.matches("[0-9]"))
					numStr += s;
				else
					break;
			}
			
			int dist = 0;
			try {
				dist = Integer.parseInt(numStr);
			}
			catch (NumberFormatException e) {
				
			}
			
			rounded = ((dist + 5) / 10) * 10;	
			if (rounded == 0)
				numStr =vor.substring(vor.indexOf(" ")+1, vorSize);
			else
				numStr =(new Integer(rounded)).toString() + vor.substring(i, vorSize);
		}
	
		return numStr;
	}
	
	/*
	 * reorder a polygon into a clockwise fashion and the first point is the northernmost point. cloreorder
	 * @param xyCopyList - polygon points list
	 * @return newList   - new polygon points list
	 */
	private static List<Coordinate> getNorthMostOrder(List<Coordinate> xyCopyList) {

		List<Coordinate> newList = new ArrayList<Coordinate>();
		double northMostLat = 0.0; //largest lat
		int northMostI = 0;
		int xySize = 0;
		
		if (xyCopyList != null && !xyCopyList.isEmpty()) {
			xySize = xyCopyList.size();
		
			northMostLat = xyCopyList.get(0).y;
			// find north most
			for (int i = 0; i < xySize; i++) {
				if (northMostLat < xyCopyList.get(i).y ) {
					northMostLat = xyCopyList.get(i).y;
					northMostI = i;
				}
			}
			
//			// Check directions for next adjacent point; assume either clockwise or the opposite
//	        // The point with the smallest angle from north is in the clockwise direction. Similar to clo_direct
			
			// reorder the xyCopyList
			for (int i = northMostI; i < xySize; i++)
				newList.add(xyCopyList.get(i));
			for (int i = 0; i < northMostI;i++)
				newList.add(xyCopyList.get(i)); 
			}
		
		return newList;
	}

}

