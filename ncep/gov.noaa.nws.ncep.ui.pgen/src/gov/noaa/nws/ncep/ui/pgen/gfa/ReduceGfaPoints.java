/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.ReduceGfaPoints
 * 
 * June 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Reduce points of a polygon. There are three algorithms to choose.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/12/2011     430      Q.Zhou      Initial creation. (from cgrreducepts.c)
 * 05/23/2011     430      Q.Zhou      Modified listIb, listIa and rmFlg 
 * 06/27/2011     430      Q.Zhou      Added map and device system transaction
 * </pre>
 * 
 * @author Q.Zhou
 * @version 1.0
 */
public class ReduceGfaPoints {
	List<Coordinate> xyList;
    
    List<Integer> reduceFlg;
    List<Integer> rmFlg;
    List<Integer> orig = new ArrayList<Integer>();
    List<Double> areaDiff; 
    
	int reduceNum;
	int index;
	double incrPct;
	double incrDst;
	double incrPctOrig;	
	String prefix;
	
	public ReduceGfaPoints( String option) {
		
		if (option.equalsIgnoreCase("ALG_SIZE"))  
	    	reduceBySize(xyList, reduceFlg, orig, reduceNum, incrPct, incrDst);
		else if (option.equalsIgnoreCase("ALG_PCT_DIST") ) 
			reduceByPctDist(xyList, reduceFlg, orig, incrPct, incrPctOrig, incrDst, prefix);
		else  // ALG_KEEP_CONCAV
			reduceKeepConcav(xyList, reduceFlg, orig, incrPct, incrDst, prefix);
		
		ReduceGfaPointsUtil.getStationTable(); //might be used for standalone
	}
	

	/*
     *  Reduce points by examining the size difference of the polygons
     *  before and after the removal of a point. The one resulting in 
     *  the least size expansion is removed. Repeat until the desired
     *  the desired reduction has been achieved.
     *
     *  The point will be removed as following:
     *  1. if the point P lies inside of the polygon, simply remove it.
     *  2. if the point P lies outside of the polygon, the points 
     *     immediately before (Pb) and after (Pa) it will be adjusted 
     *     include P. 
     *     
     * @param xyList - 	polygon points List<Coordinate>
	 * @param reduceFlg - polygon points reduce flag
	 * @param orig - 	polygon points original flag
	 * @param reduceNum - reduce points number to this number
	 * @param incrPct - maximum increase in size allowed
	 * @param incrDst - maximum distance allowed away from the polygon
	 * @return   - 	 return code					
	 *                  2 - partial success - still cannot be represented on 3 lines 
	 *                      but no reduction is allowed anymore 
	 *                  1 - reduceNum >= points, no reduction	
	 *                  0 - normal return
	 *                 -1 - bad reduceNum value (<3)		
	 *                 -2 - bad polygon points 		
	 *                 -4 - incrPct <= 0 or incrDst <= 0, no reduction possible.
     */   
	public static int reduceBySize(List<Coordinate> xyList, List<Integer> reduceFlg,
			List< Integer> orig, int reduceNum, double incrPct, double incrDst) {
		
		List<Coordinate> resultList = new ArrayList<Coordinate>(); // 0- b point, 1- a point
		List<Coordinate> listIa = new ArrayList<Coordinate>(); //list size is xyList size
		List<Coordinate> listIb = new ArrayList<Coordinate>(); //list size is xyList size
		List<Integer> rmFlg = new ArrayList<Integer>(); 	   //list size is xyList size
		List<Double> areaDiff = new ArrayList<Double>();
		int xySize = xyList.size();
		int ptRemoveIdx = -1;
		double sizeDiff = 0;
		double minDiff = 1.0e10;
		
		if (xyList == null || xyList.isEmpty())
			return -2;
			
		xySize = xyList.size();
		
		if (reduceNum < 3 )
			return -1;
		if (reduceNum >= xySize)
			return 1;
		
		GeometryFactory gf = new GeometryFactory();
		GfaSnap.getInstance().reorderInClockwise(xyList, gf);
		
		for (int i = 0; i < xySize; i++) {
			rmFlg.add(1);
			areaDiff.add(1.0e10);
		}
		
		/** Convert latlon map to Grid system to calculate topology.
		 * Do most calculation in Grid system, but do distance and snap in map system. Area difference is in grid originally.
		*/
		ArrayList<Coordinate> gridList = PgenUtil.latlonToGrid( (ArrayList<Coordinate>)xyList);
		
		/*
		 *  Find the point to be removed.
		 */
		while (reduceNum < xySize) {
			minDiff = 1.0e10;
			listIb.clear();
			listIa.clear();
			int i = 0;
			
			for ( i = 0; i< xySize; i++ )  {

			    if ( reduceFlg == null || reduceFlg.get(i) >0) { 
			    	resultList = ReduceGfaPointsUtil.findRemovePt(gridList, reduceFlg, rmFlg, areaDiff, i, incrPct, incrDst);
			    	
			    	sizeDiff = areaDiff.get(i);
			    	if ( rmFlg.get(i) >=0 && sizeDiff < minDiff )  {
			        	
			    		minDiff = sizeDiff;
			    		ptRemoveIdx = i;
			        	
			    		if ( rmFlg.get(i) ==1) {
			    			if (resultList != null && !resultList.isEmpty()) {
			    				listIb.add(i, resultList.get(0));
			    				listIa.add(i, resultList.get(1));
			    			}
						    else {
						    	listIb.add(null);
						        listIa.add(null);
						    }
			    		}
			    		else {
				    			listIb.add(null);
				    			listIa.add(null);
				    	}
			    	}
			    	else {
			    			listIb.add(null);
			    			listIa.add(null);
			    	}
			    }
			    else {
			    	listIb.add(null);
		        	listIa.add(null);
		        	rmFlg.set(i, -1);
			    }
		    }
				
			/*
			 *  Remove the minimum increase point. Modify b & a points if rmFlg is 1
			 */
			if (ptRemoveIdx == -1) {//not reducible anymore
				/* 
				 * convert grid back to latlon. Reset xyList.
				 */
				ArrayList<Coordinate> temp = PgenUtil.gridToLatlon( gridList);
				
				int j=0;
				for (j=0; j<temp.size(); j++)
					xyList.set(j, temp.get(j));
					
				while (xyList.size() > j) //note: xyList.size() is changing
					xyList.remove(xyList.size()-1);
				
				return 2;
			}
			else {
				if ( rmFlg.get(ptRemoveIdx) ==1) {
					int ib   = ((ptRemoveIdx - 1) + xySize) % xySize;
					int ia   = (ptRemoveIdx + 1)  % xySize; //since ptRemoveIdx was removed above
					gridList.set(ib, listIb.get(ptRemoveIdx));
					gridList.set(ia, listIa.get(ptRemoveIdx));
					if ( orig != null ) {							
						orig.set(ib, 0);
						orig.set(ia, 0);
					}
				}		
					
				if ( rmFlg.get(ptRemoveIdx) >= 0) { 
					try {		
						gridList.remove( ptRemoveIdx);
						listIb.remove(ptRemoveIdx);
			        	listIa.remove(ptRemoveIdx);  
			        	
						if ( orig != null ) 
							orig.remove(ptRemoveIdx);
						rmFlg.remove(ptRemoveIdx);
						areaDiff.remove(ptRemoveIdx);
						if ( reduceFlg != null ) 
							reduceFlg.remove(ptRemoveIdx);
						
						xySize -= 1;
					}
					catch (IndexOutOfBoundsException e){
					}						
				}
				
				ptRemoveIdx = -1;
			}
		}	
		
		/* 
		 * convert grid back to latlon. Reset xyList.
		 */
		ArrayList<Coordinate> temp = PgenUtil.gridToLatlon( gridList);
		
		int j=0;
		for (j=0; j<temp.size(); j++)
			xyList.set(j, temp.get(j));
			
		while (xyList.size() > j) //note: xyList.size() is changing
			xyList.remove(xyList.size()-1);
		
		return 0;
	}
	
	/* This routine reduces the number of points in a polygon to allow it to
	 * be represented on three 65-character lines of text.	It tries to 	
	 * remove allowable points, one at a time, based on the impact their 	
	 * Individual removal would have on the size of the polygon.  		
	 * Specifically, remove points that increase the size of the polygon the
	 * least,  while not increasing the overall size of the polygon 	
	 * by "incrPct" (per individual point removal), not increasing the 	
	 * overall size of the polygon by "incrPctOrig" (per cumulative point 	
	 * removal compared with the original size of the polygon), and not 	
	 * allowing any new points to be "incrDst" distance from the original 	
	 * polygon points. "reducePct" refers to the area percentage increase 	
	 * when a single point is removed from the polygon. Point reduction 	
	 * continues until the polygon can be represented on three 65-character	
	 * lines of text, or no more points can be removed under the above criteria.	
	 * 
	 * @param xyList - 	polygon points List<Coordinate>
	 * @param reduceFlg - polygon points reduce flag
	 * @param orig - 	polygon points original flag
	 * @param incrPctOrig - maximum increase in total area size allowed
	 * @param incrPct - maximum increase in area size allowed
	 * @param incrDst - maximum distance allowed away from the polygon
	 * @param prefix -  prefix of format line 
	 * @return   - 	 return code					
	 *                  2 - partial success - still cannot be represented on 3 lines 
	 *                      but no reduction is allowed anymore 
	 *                  0 - normal return
	 *                 -2 - bad polygon points 		
	 *                 -4 - incrPct <= 0, incrPctOrig <=0 or incrDst <= 0, no reduction possible.
	 */ 
	public static int reduceByPctDist(List<Coordinate> xyList, List<Integer> reduceFlg, List< Integer> orig,
			double incrPct, double incrPctOrig, double incrDst, String prefix) {
		
		List<Coordinate> resultList = new ArrayList<Coordinate>(); // 0- b point, 1- a point
		List<Coordinate> listIa = new ArrayList<Coordinate>(); //list size is xyList size
		List<Coordinate> listIb = new ArrayList<Coordinate>(); //list size is xyList size
		List<Integer> rmFlg = new ArrayList<Integer>(); 	   //list size is xyList size
		List<Double> areaDiff = new ArrayList<Double>();
		
		int xySize = xyList.size();
		int ptRemoveIdx = -1;
		double sumSizeDiff = 0;
		double minDiff = 1.0e10;
		
		if (xyList == null || xyList.isEmpty())
			return -2;
			
		xySize = xyList.size();
		
		if (xySize <= 3)
			return -2;
		
		GeometryFactory gf = new GeometryFactory();
		GfaSnap.getInstance().reorderInClockwise(xyList, gf);
		
		for (int i = 0; i < xySize; i++) {
			rmFlg.add(1);
			areaDiff.add(1.0e10);
		}
		
	/** Convert latlon map to Grid system to calculate topology.
	 * Do most calculation in Grid system, but do distance, format and snap in map system. Area difference is in grid originally.
	 */
	ArrayList<Coordinate> gridList = PgenUtil.latlonToGrid( (ArrayList<Coordinate>)xyList);
	
    if (!ReduceGfaPointsUtil.canFormatted(xyList, prefix) && xySize > 3) {
		/*
	     * Compute area of original polygon
	     */   	
    	List<Coordinate> xyCloseList = new ArrayList();
    	for ( Coordinate c : gridList ) {
    		xyCloseList.add( c );
		}
   
		Double xyArea = ReduceGfaPointsUtil.getArea(xyCloseList);
//		System.out.println("gridList "+gridList);

		/*
	     * Compute the potentials for all points.
	     */
	    for ( int i = 0; i < xySize; i++ ) {	    
		
	    	if ( reduceFlg == null || reduceFlg.get(i) > 0 ) {
	    		resultList = ReduceGfaPointsUtil.findRemovePt(gridList, reduceFlg, rmFlg, areaDiff, i, incrPct, incrDst);
	    		
	    		if (resultList != null && !resultList.isEmpty()) {
	    			listIb.add(i, resultList.get(0));
	    			listIa.add(i, resultList.get(1));
	    		}
	    		else {
	    			listIb.add( null);
	    			listIa.add( null);
	    		}
	        }
	    	else {
    			listIb.add( null);
    			listIa.add( null);
    			rmFlg.set(i, -1);
    		}
	    }
	      
	    sumSizeDiff = 0;
	    
	    ArrayList<Coordinate> formatList = PgenUtil.gridToLatlon( (ArrayList)gridList);
		while (!ReduceGfaPointsUtil.canFormatted(formatList, prefix)  && xySize > 3) {
			/*
			 *  Find the point to be removed.
			 */		
			minDiff = 1.0e10;
			int i = 0;
			
			for ( i = 0; i< xySize; i++ )  {

			    if ( reduceFlg == null || reduceFlg.get(i) >0) { //rmFlg, areaDiff skip point that reduceFlg <=0
			    	if ( rmFlg.get(i) >= 0 && areaDiff.get(i) < minDiff ) {
			    		minDiff = areaDiff.get(i);
			        	ptRemoveIdx = i;
			    	}
			    }
			}
			
			/*
			 * Check if the total size differential exceeds that requested 
			 * (in terms of percentage of original polygon size).
			 */
			sumSizeDiff += minDiff; //in while
			int ptFlg = 0; // copy of rmFlg.get(ptRemoveIdx)
			
			//test
//			System.out.println("*****min " +minDiff +" "+sumSizeDiff +" " +100*sumSizeDiff/xyArea);
//			ArrayList<Coordinate> tem = PgenUtil.gridToLatlon( gridList);			
//			System.out.println("ptRemoveIdx "+ptRemoveIdx +" " +tem.get(ptRemoveIdx));
			
			
			if (ptRemoveIdx == -1 || 100*sumSizeDiff/xyArea >= incrPctOrig ) {//not reducible anymore
				/* 
				 * convert grid back to latlon. Reset xyList.
				 */
				ArrayList<Coordinate> temp = PgenUtil.gridToLatlon( gridList);
				
				int j=0;
				for (j=0; j<temp.size(); j++)
					xyList.set(j, temp.get(j));
					
				while (xyList.size() > j) //note: xyList.size() is changing
					xyList.remove(xyList.size()-1);
				
				return 2;
			}
			else {
				/*
				 *  Modify b & a points if rmFlg is 1
				 */
				if ( rmFlg.get(ptRemoveIdx) ==1) {
					int ib   = ((ptRemoveIdx - 1) + xySize) % xySize;
					int ia   = (ptRemoveIdx + 1)  % xySize; 
					gridList.set(ib, listIb.get(ptRemoveIdx));
					gridList.set(ia, listIa.get(ptRemoveIdx));
					if ( orig != null ) {							
						orig.set(ib, 0);
						orig.set(ia, 0);
					}
					ptFlg = rmFlg.get(ptRemoveIdx); //copy
				}		
				/*
				 *  Remove the minimum increase point. 
				 */	
				if ( rmFlg.get(ptRemoveIdx) >= 0) { 
					try {		
						gridList.remove(ptRemoveIdx);
						listIb.remove(ptRemoveIdx);
			        	listIa.remove(ptRemoveIdx);  
			        	
						if ( orig != null ) 
							orig.remove(ptRemoveIdx);
						rmFlg.remove(ptRemoveIdx);
						areaDiff.remove(ptRemoveIdx);
						if ( reduceFlg != null ) 
							reduceFlg.remove(ptRemoveIdx);
						
						xySize -= 1;
					}
					catch (IndexOutOfBoundsException e){
					}				
				}
				
				/*
				 *  New points should be snapped so canBeFormatted can
				 *  check on the same FROM line as in af_fmt2xml. It is only for convex removal.
				 */
				if ( ptFlg == 1 )  {	
					
					int  ia = (ptRemoveIdx + xySize) % xySize; //new xySize				            
					if ( orig != null && orig.get(ia) == 0
					            && (reduceFlg == null || reduceFlg.get(ia) == 1) ) {
					         
						Coordinate[] snappedA = new Coordinate[1];
						ArrayList<Coordinate> transList = PgenUtil.gridToLatlon( (ArrayList)gridList);
						
						GfaSnap.getInstance().snapPtGFA(ia, ia, null, null, 
								transList, true, true, 3.0, snappedA);
						
						transList.set(ia, snappedA[0]);
						gridList = PgenUtil.latlonToGrid( (ArrayList) transList);
						
					}
					
					int  ib = ( ( ptRemoveIdx - 1 ) + xySize ) % xySize;
					if ( orig != null && orig.get(ib) == 0 
						            && (reduceFlg == null || reduceFlg.get(ib) == 1) ){
						        
						Coordinate[] snappedB = new Coordinate[1];
						ArrayList<Coordinate> transList = PgenUtil.gridToLatlon( (ArrayList)gridList);
						
						GfaSnap.getInstance().snapPtGFA(ib, ib, null, null, 
								transList, true, true, 3.0, snappedB);
						transList.set(ib, snappedB[0]);
						gridList = PgenUtil.latlonToGrid( (ArrayList) transList);
					}
				}	
						
				/* Check if the new polygon can be formatted on 3 lines.
				 * Calculate the new area potentials for the adjacent points.  The two adjacent points need to be recalculated  
				 * for all removals, however for convex, the four adjacent points need to be recalculated (2 on either side).
				 */
				formatList = PgenUtil.gridToLatlon( (ArrayList)gridList);
				if (!ReduceGfaPointsUtil.canFormatted(formatList, prefix) && xySize > 3) {
							
					int ib = ( ( ptRemoveIdx - 1 ) + xySize ) % xySize;
					    if ( reduceFlg == null || reduceFlg.get(ib) >0 ) {
					        resultList = ReduceGfaPointsUtil.findRemovePt(gridList, reduceFlg, rmFlg, areaDiff, ib, incrPct, incrDst);
					        
					        if (resultList != null && !resultList.isEmpty()) {
					        		listIb.set(ib, resultList.get(0));
					        		listIa.set(ib, resultList.get(1));    
					        }
					        else {
					    			listIb.set(ib, null);
					    			listIa.set(ib, null);
					    	}
					    }
					    else {
			    			listIb.set(ib, null);
			    			listIa.set(ib, null);
			    			rmFlg.set(ib, -1);
					    }
					        
					int ia = (ptRemoveIdx + xySize) % xySize;
					    if ( reduceFlg == null || reduceFlg.get(ia) >0 ) {
					        resultList = ReduceGfaPointsUtil.findRemovePt(gridList, reduceFlg, rmFlg, areaDiff, ia, incrPct, incrDst);
					        
					        if (resultList != null && !resultList.isEmpty()) {
					        		listIb.set(ia, resultList.get(0));
					        		listIa.set(ia, resultList.get(1));
					        }
					        else {
					    			listIb.set(ia, null);
					    			listIa.set(ia, null);
					    	}
				        }
					    else {
			    			listIb.set(ia, null);
			    			listIa.set(ia, null);
			    			rmFlg.set(ia, -1);
					    }
					    
					if ( ptFlg == 1 ) { //ibb
					        ib = ( ( ptRemoveIdx - 2 ) + xySize ) % xySize;
					        if ( reduceFlg == null || reduceFlg.get(ib) >0 ) {
					        	resultList = ReduceGfaPointsUtil.findRemovePt(gridList, reduceFlg, rmFlg, areaDiff, ib, incrPct, incrDst);
					        
					        	if (resultList != null && !resultList.isEmpty()) {
					            	listIb.set(ib, resultList.get(0));
					            	listIa.set(ib, resultList.get(1)); 
					        	}
					        	else {
					    			listIb.set(ib, null);
					    			listIa.set(ib, null);
					        	}
					        } else {
				    			listIb.set(ib, null);
				    			listIa.set(ib, null);
				    			rmFlg.set(ib, -1);
						    }
					            
					        ia = ( ptRemoveIdx + 1 + xySize ) % xySize;
					        if ( reduceFlg == null || reduceFlg.get(ia) >0 ) {
					        	resultList = ReduceGfaPointsUtil.findRemovePt(gridList, reduceFlg, rmFlg, areaDiff, ia, incrPct, incrDst); 
					        
					        	if (resultList != null && !resultList.isEmpty()) {
					            	listIb.set(ia, resultList.get(0));					     
					            	listIa.set(ia, resultList.get(1)); 
					        	}
					        	else {
					    			listIb.set(ia, null);
					    			listIa.set(ia, null);
					        	}
					        } else {
				    			listIb.set(ia, null);
				    			listIa.set(ia, null);
				    			rmFlg.set(ia, -1);
						    }				    	
					}										
			    }
			}	
				
			ptRemoveIdx = -1;
			} // End of while loop 
			
		}  
		
    	/* 
    	 * convert grid back to latlon. Reset xyList.
    	 */
		ArrayList<Coordinate> temp = PgenUtil.gridToLatlon( gridList);
		
		int j=0;
		for (j=0; j<temp.size(); j++)
			xyList.set(j, temp.get(j));
		
		while (xyList.size() > j) //note: xyList.size() is changing
			xyList.remove(xyList.size()-1);
		
		return 0;
	}

	
	/*This routine reduces the number of points in a polygon to allow it to
	 * be represented on three 65-character lines of text. It is similar to above 		
	 * except not consider "incrPctOrig" (per cumulative point 	
	 * removal compared with the original size of the polygon)
	 * 
	 * @param xyList - 	polygon points List<Coordinate>
	 * @param reduceFlg - polygon points reduce flag
	 * @param orig - 	polygon points original flag
	 * @param incrPct - maximum increase in area size allowed
	 * @param incrDst - maximum distance allowed away from the polygon
	 * @param prefix -  prefix of format line 
	 * @return   - 	 return code					
	 *                  2 - partial success - still cannot be represented on 3 lines 
	 *                      but no reduction is allowed anymore 
	 *                  0 - normal return
	 *                 -2 - bad polygon points 		
	 *                 -4 - incrPct <= 0, incrPctOrig <=0 or incrDst <= 0, no reduction possible.
	 */
	public static int reduceKeepConcav(List<Coordinate> xyList, List<Integer> reduceFlg, 
			List< Integer> orig, double incrPct, double incrDst, String prefix) {
		
		List<Coordinate> resultList = new ArrayList<Coordinate>(); // 0- b point, 1- a point
		List<Coordinate> listIa = new ArrayList<Coordinate>(); //list size is xyList size
		List<Coordinate> listIb = new ArrayList<Coordinate>(); //list size is xyList size
		List<Integer> rmFlg = new ArrayList<Integer>(); 	   //list size is xyList size
		List<Double> areaDiff = new ArrayList<Double>();
		int xySize = xyList.size();
		int ptRemoveIdx = -1;
		double sizeDiff = 0;
		double minDiff = 1.0e10;
		
		if (xyList == null || xyList.isEmpty())
			return -2;
			
		xySize = xyList.size();
		
		if (xySize <= 3)
			return -2;
		
		GeometryFactory gf = new GeometryFactory();
		GfaSnap.getInstance().reorderInClockwise(xyList, gf);
		
		for (int i = 0; i < xySize; i++) {
			rmFlg.add(1);
			areaDiff.add(1.0e10);
		}
		
		/*
		 *  Find the point to be removed.
		 */
		ArrayList<Coordinate> gridList = PgenUtil.latlonToGrid( (ArrayList<Coordinate>)xyList);
		
		while (!ReduceGfaPointsUtil.canFormatted(xyList, prefix) && xySize > 3) { //!
			minDiff = 1.0e10;
			listIb.clear();
    		listIa.clear();
			int i = 0;
			
			for ( i = 0; i< xySize; i++ )  {

			    if ( reduceFlg == null || reduceFlg.get(i) >0) {
			    	resultList = ReduceGfaPointsUtil.findRemovePt(gridList, reduceFlg, rmFlg, areaDiff, i, incrPct, incrDst);			    	
//			    	System.out.println("resultList" +resultList);
			    				    	
			    	sizeDiff = areaDiff.get(i);
			    	if ( rmFlg.get(i) >=0 && sizeDiff < minDiff )  {
			        	
			    		minDiff = sizeDiff;
			    		ptRemoveIdx = i;
			        	
			    		if ( rmFlg.get(i) ==1) {
			    			if (resultList != null && !resultList.isEmpty()) {
			    				listIb.add(i, resultList.get(0));
			    				listIa.add(i, resultList.get(1));
			    			}
						    else {
						    	listIb.add(null);
						        listIa.add(null);
						    }
			    		}
			    		else {
				    			listIb.add(null);
				    			listIa.add(null);
				    	}
			    	}
			    	else {
			    			listIb.add(null);
			    			listIa.add(null);
			    	}
			    	
			    }
			    else {
			    	listIb.add(null);
		        	listIa.add(null);
		        	rmFlg.set(i, -1);
			    }
		    }
				
			/*
			 *  Remove the minimum increase point. Modify b & a points if rmFlg is 1
			 */
			if (ptRemoveIdx == -1) {//not reducible anymore
				/* 
				 * convert grid back to latlon. Reset xyList.
				 */
				ArrayList<Coordinate> temp = PgenUtil.gridToLatlon( gridList);
				
				int j=0;
				for (j=0; j<temp.size(); j++)
					xyList.set(j, temp.get(j));
					
				while (xyList.size() > j) //note: xyList.size() is changing
					xyList.remove(xyList.size()-1);
				
				return 2;
			}
			else {
											
				if ( rmFlg.get(ptRemoveIdx) ==1) {
					int ib   = ((ptRemoveIdx - 1) + xySize) % xySize;
					int ia   = (ptRemoveIdx + 1)  % xySize; 
					gridList.set(ib, listIb.get(ptRemoveIdx));
					gridList.set(ia, listIa.get(ptRemoveIdx));
					if ( orig != null ) {							
						orig.set(ib, 0);
						orig.set(ia, 0);
					}
				}		
					
				if ( rmFlg.get(ptRemoveIdx) >= 0) { 
					try {		
						gridList.remove(ptRemoveIdx);
						listIb.remove(ptRemoveIdx);
			        	listIa.remove(ptRemoveIdx);  
			        	
						if ( orig != null ) 
							orig.remove(ptRemoveIdx);
						rmFlg.remove(ptRemoveIdx);
						areaDiff.remove(ptRemoveIdx);
						if ( reduceFlg != null ) 
							reduceFlg.remove(ptRemoveIdx);
						
						xySize -= 1;
					}
					catch (IndexOutOfBoundsException e){
					}						
				}	
								
				ptRemoveIdx = -1;
			}
			
			/*
			 *  Check if the new polygon can be formatted on 3 lines.
			 *  New points should be snapped so cgr_canBeFormatted can
			 *  check on the same FROM line as in af_fmt2xml.
			 */
   
			for (int  j = 0; j < xySize; j++ ) {
			    if ( orig != null && orig.get(j) == 0
			    		&& (reduceFlg == null || reduceFlg.get(j)  == 1) ) {
			    	//snap  
			    	Coordinate[] snapped = new Coordinate[1];
			    	ArrayList<Coordinate> transList = PgenUtil.gridToLatlon( (ArrayList)gridList);
					
					GfaSnap.getInstance().snapPtGFA(j, j, null, null, 
							transList, true, true, 3.0, snapped);

					transList.set(j, snapped[0]);	
					gridList = PgenUtil.latlonToGrid( (ArrayList) transList);
			    }
			}	    		
		}  // End of while loop 	
		
		/* 
		 * convert grid back to latlon. Reset xyList.
		 */
		ArrayList<Coordinate> temp = PgenUtil.gridToLatlon( gridList);
		
		int j=0;
		for (j=0; j<temp.size(); j++)
			xyList.set(j, temp.get(j));
			
		while (xyList.size() > j) //note: xyList.size() is changing
			xyList.remove(xyList.size()-1);
		return 0;
	}
	
	/*
	 *  get & set
	 */

	public void setXyList(List<Coordinate> xyList) {
		this.xyList = xyList;
	}

	public List<Coordinate> getXyList() {
		return xyList;
	}
 
	public void setReduceFlg(List<Integer> reduceFlg) {
		this.reduceFlg = reduceFlg;
	}

	public List<Integer> getReduceFlg() {
		return reduceFlg;
	}   

	public void setRmFlg(List<Integer> rmFlg) {
		this.rmFlg = rmFlg;
	}

	public List<Integer> getRmFlg() {
		return rmFlg;
	} 
	
	public void setOrig(List<Integer> orig) {
		this.orig = orig;
	}

	public List<Integer> getOrig() {
		return orig;
	} 
 
	public void setAreaDiff(List<Double> areaDiff) {
		this.areaDiff = areaDiff;
	}

	public List<Double> getAreaDiff() {
		return areaDiff;
	} 

	public void setReduceNum(int reduceNum) {
		this.reduceNum = reduceNum;
	}

	public int getReduceNum() {
		return reduceNum;
	}
	
	public void setIndex(int index) {
		this.index = index;
	}

	public int getIndex() {
		return index;
	}
	
	public void setIncrPct(double incrPct) {
		this.incrPct = incrPct;
	}

	public double getIncrPct() {
		return incrPct;
	}
	
	public void setIncrDst(double incrDst) {
		this.incrDst = incrDst;
	}

	public double getIncrDst() {
		return incrDst;
	}

	public void setIncrPctOrig(double incrPctOrig) {
		this.incrPctOrig = incrPctOrig;
	}

	public double getIncrPctOrig() {
		return incrPctOrig;
	}
	
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public String getPrefix() {
		return prefix;
	}	
	
}
