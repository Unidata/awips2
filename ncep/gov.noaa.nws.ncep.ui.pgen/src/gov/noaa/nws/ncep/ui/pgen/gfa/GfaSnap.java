/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaSnap
 * 
 * April 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import static java.lang.Math.abs;
import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductInfo;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductTime;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.viz.common.SnapUtil.SnapVOR;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.TreeMap;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;


/**
 * GFA snapping functionality.
 * 
 * Note: See in-method documentation for algorithms used.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * 04/11					J. Wu		Initial creation.
 * 05/11					J. Wu		Added checking against a list 
 * 										of used points to make sure that
 * 										no duplicate points in the result.
 * 05/11					J. Wu		Make sure point ii will not cluster
 * 										with "snapped" point ii-1.
 * 05/11					J. Wu		No insertion for sparse snap point
 * 										cases, use the original point.
 * 06/11					J. Wu		Sort snap stations with both simple
 * 										distance and great circle distance.
 * 07/11					J. Wu		When snapping a polygon, do not snap those 
 * 										points that are outside of the FA
 *                                      international bound (snapOneRound()).
 * 07/11					J. Wu		Add a check to see if a point is already
 * 										a snap point (within a precision), if so, 
 * 										no snapping is necessary.
 * 07/11					J. Wu		Used the legacy algorithm to check if a 
 * 										point is at left/right of a line. 
 * 07/11					B. Yin		SnapPtGfa to handle null or points less than two.
 *
 * 10/11					J. Wu		Do not add "usedPts" check when snapping a whole
 * 										polygon (snapOneRound).
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 1
 */

public class GfaSnap {

	private static			List<Station> snapStns = null;
	private static double	EARTH_RADIUS = 6371200.0;
	private static double	RMISSD = -9999.0;
	protected static double CLUSTER_DIST = 30.0;		/* clustering distance */
	private static int  	NUM_SNAP = 100;		   

	/** Tolerance for coordinates to be treated as the same /close points in MAP coordinate */
	protected static final double MAP_PRECISION = 0.0001;
	private static final double	GDIFFD	= 0.000001;
	private static final double	ONLINE_TOL	= 0.001;

	private static GfaSnap	instance = new GfaSnap();

	/**
	 * Private constructor, load the snap points once.
	 */
	private GfaSnap() {		
		if ( snapStns == null ) {
		    snapStns = SnapVOR.getSnapStns( null,  16 );
		}
	}

	/**
	 * Singleton instance.
	 * 
	 * @return
	 */
	public static GfaSnap getInstance() {
		return instance;
	}			
    
	/**
	 * Snaps the coordinates to the VOR stations from
	 * SigmetInfo.VOR_STATION_LIST
	 * 
	 * @param coors		the points to be snapped
	 * @param gf		a GeometryFactory
	 * @return
	 */
	public ArrayList<Coordinate> snapPolygon1( List<Coordinate> poly, GeometryFactory gf ) {
        		
		ArrayList<Coordinate> snapPtsList = new ArrayList<Coordinate>();
		
		// Default to GfaFormat's same GeometryFactory.
		if ( gf == null ) gf = GfaFormat.getGeometryFactory();
		
		// Reorder points in clockwise order.
		ArrayList<Coordinate> coors = reorderInClockwise( poly, gf );
						
		// Do snapping
		TreeMap<Double, Station> snapPtsTreeMap;

		Coordinate coor = null;

		for ( int ii = 0; coors != null && ii < coors.size(); ii++ ) {
			// keep the order in snapPtsList
			coor = coors.get( ii );
			
			// Sort the points based on a simple distance.
            snapPtsTreeMap = sortSnapPoints( coor, NUM_SNAP );
			
			// Keep the first one, just in case
			Station first = snapPtsTreeMap.firstEntry().getValue();
			
			// Segment coordinates
			Coordinate coorNext = (ii + 1) >= coors.size() ? coors.get(0) : coors.get(ii + 1);
			Coordinate coorPrev = (ii - 1) < 0 ? coors.get(coors.size() - 1) : coors.get(ii - 1);
			
			/*
			 * take the first one, check whether it is on the right side from
			 * both segments if not, take the next
			 */
			while ( !snapPtsTreeMap.isEmpty() ) {
				Double key = snapPtsTreeMap.firstKey();
				Station s = snapPtsTreeMap.remove( key );
				Coordinate coorSnap = new Coordinate( s.getLongitude(), s.getLatitude() );
				
				if ( onLeft( coorSnap, coorPrev, coor )   && 
					 onLeft( coorSnap, coor, coorNext )   && 
					 !snapPtsList.contains( coorSnap ) ) {
					
					snapPtsList.add( coorSnap );
					break;
				}
			}
			
			// If nothing satisfied, use first
			if ( snapPtsTreeMap.isEmpty() ) {
				snapPtsList.add( new Coordinate( first.getLongitude(), first.getLatitude() ) );
			}

			snapPtsTreeMap.clear();
			
		}
		
		return snapPtsList;
	}
	
		
	/**
	 *  Customized snapping for GFA smears with options used in legacy NMAP:
	 *  
	 *  expandOnly	= true
	 *  tolerance	= 3.0 (nm)
	 *  reorder		= true
	 *  closed		= true
	 * 
     * @param	pointsIn	Coordinate	input points in lat/lon.
	 */
	public ArrayList<Coordinate> snapPolygon( ArrayList<Coordinate> pointsIn ) {
		
		return snapPolygon( true, 0.0F, true, true, pointsIn );
				
	}

	
	/**
	 *  Snaps points of the input polygon to the nearest VOR points without 
	 *  clustering.  The resulting polygon will feature: (1)any two consecutive 
	 *  points are at least CLUSTER_DIST apart and (2) always contains (larger than) 
	 *  the input polygon, if "expandOnly" is TRUE.	
	 * 
     * @param	expandOnly	Boolean		flag to expand GFA only
     * @param	tolerance 	Boolean		closeness tolerance in nautical miles
     * @param	reorder 	Boolean		flag to re-order the polygon
     * @param	closed 		Boolean		if the polygon is closed
     * @param	pointsIn	Coordinate	input points in lat/lon
	 */
	public ArrayList<Coordinate> snapPolygon( Boolean expandOnly, float tolerance, Boolean reorder,
		                       Boolean closed, ArrayList<Coordinate> pointsIn ) {
		
		ArrayList<Coordinate> snappedPts = new ArrayList<Coordinate>();
		
		// Sanity check: if the polygon points are less than 3
		if ( pointsIn.size() < 3 ) {
			snappedPts.addAll( pointsIn );
			return snappedPts;
		}
		

		//Snap the polygon - may still end up with clustering points.
		ArrayList<Coordinate> snappedOne = snapOneRound ( expandOnly, tolerance, reorder,
				                                          closed, pointsIn );
				
		//Continue for additional clustering processing until no more clustering left.       
		boolean done = false;
		int nround = 0;
		int npts;
		boolean clusterFound;

		while ( !done ) {
			
			npts = snappedOne.size();
			clusterFound = false;
            
			//Suppose the first and the last point is not the same!
			int next;
			for ( int ii = 0; ii < npts; ii++ ) {
				
				next = ( ( ii + 1 ) < npts ) ? ( ii + 1 ) : 0;
				
				if ( isCluster( snappedOne.get( ii ) , snappedOne.get( next ) ) ) {
					clusterFound = true;
					break;				
				}
			}

			//Run snapping again without reordering.
			if ( clusterFound ) {
				snappedOne = snapOneRound( expandOnly, tolerance, false, closed, snappedOne );
			}
			else {
				done = true;
			}

			//Need something to break the deadlock if it happens.
			nround++;
			if ( nround > 5 ) done = true;
		}

		snappedPts.addAll( snappedOne );
		
		return snappedPts;
		
	}

	/**
	 * snaps all points of an input polygon to the nearest VOR points.  
	 * The resulting polygon may still have clustering points.	
	 * 
     * @param	expandOnly	flag to expand GFA only
     * @param	tolerance 	closeness tolerance
     * @param	reorder 	flag to re-order the polygon
     * @param	closed 		if the polygon is closed
     * @param	pointsIn	input points in lat/lon
     * @return				Snapped polygon (may have less points than input).
	 */
	private ArrayList<Coordinate> snapOneRound( Boolean expandOnly, float tolerance, 
				boolean reorder, boolean closed, ArrayList<Coordinate> inCoors ) {

		ArrayList<Coordinate> snappedPts = new ArrayList<Coordinate>();		
		
		// Sanity check if the polygon points are less than 3
		if ( inCoors.size() < 3 ) {
			snappedPts.addAll( inCoors );
			return snappedPts;
		}
		
	    
		// Reorder points in clockwise order.
		ArrayList<Coordinate> coors = new ArrayList<Coordinate>();
		if ( reorder ) {
			coors.addAll( reorderInClockwise( inCoors, null ) );			    
		}
		else {
			coors.addAll( inCoors );
		}

		/*
	     *  Shift (rotate) the array if necessary to ensure that the first point
	     *  is not part of a cluster - if it is, it must be the first point of one.         
	     * 
	     *  Note: only do this shift if the input is a CLOSED polygon.
	     */    
	    int ii = 0;
        int jj = 0;
        
	    if ( closed ) {
		    while ( jj < coors.size() && 
		    		isCluster( coors.get( ii ), coors.get( coors.size() - 1 ) ) ) {
				
		    	coors = shiftArray( coors );
		        jj += 1;
	        }	
	    }
	    
	    /*
	     *  Find the nearest VOR for each point
	     */    
	    ii = 0;
	    boolean done = false;
	    int ier_prev = 0;
	    int npts = coors.size();
	    int ii2, nshift, status;
	    
		Coordinate[] snappedPt = new Coordinate[1];
		ArrayList<Coordinate> chkPts = new ArrayList<Coordinate>();
	    
	    while ( !done )  {
			           	    	
	    	//Find clusterings
		    ii2 = ii;
		    while ( ( ii2 + 1 ) < npts &&
		            isCluster( coors.get( ii ), coors.get( ii2 + 1 ) ) ) {
		        ii2 += 1;
		    }
		    nshift = ii2 - ii;
									
		    /*
		     * Snap this point
		     * 
		     * The point should not cluster with the point before it (snapped). Since 
		     * the point snapped (ii-1) is not passed in polygon "coors", we add the
		     * snapped point (ii-1) as a check point to do the clustering check.
		     */		    
		    if ( (ii-1) >= 0 ) {
		    	chkPts.clear();
		    	chkPts.add( snappedPts.get( ii - 1 ) );
			    
		    	if ( ii == (coors.size() - 1) ) {
		    	    chkPts.add( snappedPts.get( 0 ) );
			    }		    	
		    }		    
			
//		    status = snapPtGFA( ii, ii2, snappedPts, chkPts, coors, true, true, tolerance, snappedPt );			
			
		    /*
		     *  If the point to be snapped is outside of the FA international bound, no snapping, keep
		     *  it as is - it will be cutoff in clipping process later.
		     */
			if ( !isInFaBound( coors.get( ii ) ) ) {
				snappedPt[ 0 ] = new Coordinate( coors.get( ii ) );
				status = 0;
			}
			else {				
//			    status = snapPtGFA( ii, ii2, snappedPts, chkPts, coors, true, true, tolerance, snappedPt );			
			    status = snapPtGFA( ii, ii2, null, null, coors, false, true, tolerance, snappedPt );			
			}

			snappedPts.add( new Coordinate( snappedPt[0] ) );
						
		    
			/*
			 * Further processing with clustering/inserting....
			 */
		    if ( status == 1 ) {  		/* keep the original points */		   
                snappedPts.set( snappedPts.size()-1, new Coordinate( coors.get( ii ) ) );
		    }
		    else if ( status == 2 )  {	
				
		    	/*
		    	 * special processing for the case of sparse snap point availability.
		    	 * in this case, attempt to insert the returned snap point and
		    	 * send the old one in again.
		    	 */
		    	if ( ier_prev == 2 )  {

		    		/*
		    		 * previous point also could not be placed... this is an
		    		 * unsolvable problem! remove the previously inserted point
		    		 * and keep the original (unsnapped) point.
		    		 * Note: if the snapshots are drawn correctly (within 
		    		 * international boundaries), this section of code should 
		    		 * never be executed.
		    		 */
		    		if ( (ii - 1) >= 0 ) {  //watch out...
		    		    coors = collapseArray( coors, ii-1, 1 );		    					    		
		    		    npts = coors.size();
		    		    ii--;
		    		}
	    		    
		    		snappedPts.set( snappedPts.size() - 1, new Coordinate( coors.get( ii ) ) );
		    	}
		    	else  {
		    		//insert the returned point BEFORE the point being snapped.
		    		coors = insertArray( coors, ii, snappedPt[0] );
		    		npts = coors.size();
		    	}
		    }
	        else if ( status < 0 ) {
                done = true;
                status = -2;
		    }		    
		    else if ( nshift != 0 )  { 
		    	//Cluster processing - collapse	the entire array back 'nshift' points.
		    	coors = collapseArray( coors, ii, nshift );
		    	coors.set( ii, snappedPt[0] );
		    	npts = coors.size();		    	
		    }
		    
		    ii++;
		    if ( ii >= npts )  done = true;
		    ier_prev = status;
	    }

	    return snappedPts;
	}
		
	
	/**
	 * Snaps a coordinate point to the VOR point in SigmetInfo.VOR_STATION_LIST.
	 * 
	 * The input is assumed to be in MAP coordinate.
	 * 
	 * Note: 1. The coordinates of the polygon must be ordered in clockwise.
	 *       2. For closed polygon, do not repeat the first point at the end.
	 *       3. All input points should be in map coordinate (lat/lon)
	 * 
	 * @param index			index of the point to be snapped
	 * @param index2		index of the second point
	 * @param usedPts		points having been used and do not want to be used again.
	 * @param checkPts		extra check points - snapped point should not cluster with.
	 * @param smearPts		the points of the polygon in clockwise
     * @param checkDist     flag for additional distance check 
     * @param expandOnly    flag to expand GFA only 
	 * @param tolerance		closeness tolerance for being considered as snapped point.
	 * @param snappedPt		snapped point
	 * @return snapPtGFA()
     *                      =  0    normal
     *                      =  1    no change
     *                      =  2    point insertion
	 */
	public int snapPtGFA( int index, int index2, List<Coordinate> usedPts, 
			              List<Coordinate> checkPts, List<Coordinate> smearPts, 
			              boolean checkDist, boolean expandOnly, double tolerance, 
						  Coordinate[] snapped ) {
        		
	    //Default case, no change
		int status = 1;		
		int snap_indx1 = index;
		int snap_indx2 = index2;
		Coordinate tPoint = smearPts.get( snap_indx1 );
		
		Coordinate snappedPt = new Coordinate( tPoint );
				
		//Default tolerance in legacy
		if ( tolerance <= 0.0 ) {
           tolerance = 3.0;
		}
		
		//Find NUM_SNAP nearest snap points
		ArrayList<Coordinate> snapPts = getNumSnapPoints( tPoint, NUM_SNAP );

		/*
		 * !!!For single point, no need to snap if it is already a snap point.
		 */
    	if ( index == index2 && isSnapPoint( tPoint, snapPts ) ) {	
            snapped[ 0 ] = new Coordinate( tPoint );
            status = 0;
            return status;    		
    	}
		
	    /*
	     *  Get the snap point
	     * 
	     * The point Ps should meet the following criteria:
	     * 									
	     * (1) away from points P(index-1) & P(index2+1) with clustering distance
	     *     when checkDist is True.
	     * (2) all points from P(index) to P(index2) should be at the right of segment
	     *     P(index-1)->Ps and at the left of P(index2+1)->Ps, if "expandOnly" is
	     *     True.
	     * (3) away from the additional check points specified.
	     */    
	    int nclose = snapPts.size();
	    int npts = smearPts.size();
	    	    
	    // Segment from P(index2+1) to P(index2)
	    Coordinate[] coorL2 = new Coordinate[2];
	    coorL2[ 0 ] = smearPts.get( ( snap_indx2 + npts + 1 ) % npts );
	    coorL2[ 1 ] = smearPts.get( ( snap_indx2 + npts  ) % npts );
        
	    // Segment from P(index1-1) to a potential snap point	    
	    Coordinate[] coorB = new Coordinate[2];
	    coorB[ 0 ] = smearPts.get( (snap_indx1 + npts - 1 ) % npts );
	    
	    // Segment from P(index2+1) to a potential snap point	    
	    Coordinate[] coorA = new Coordinate[2];
	    coorA[ 0 ] = new Coordinate( coorL2[ 0 ] );
	    
	    // Create a polygon from the smear point (convert to grid coordinate )
    	Coordinate[] gfaPts = smearPts.toArray( new Coordinate[ smearPts.size() ] );
    	Polygon gfaPolygon = null;
    	if ( gfaPts.length > 2 ){
    		gfaPolygon = GfaClip.getInstance().pointsToPolygon( PgenUtil.latlonToGrid( gfaPts ) );
    	}
    	
    	boolean done = false;
	    boolean firstSnap = false;
        
	    int ii, jj, firstSnapIndex = -1;
	    Coordinate  candidatePt;  
			    
	    while ( !done ) {
            
	        for ( ii = 0; ii < nclose; ii++ ) {
	    			        	
	        	/*
	             *  The point should not be the used ones in usedPts.
	             */
	        	if ( usedPts != null && usedPts.contains( snapPts.get( ii ) ) ) {
	        		continue;
	        	}
	    	    	        	
	        	/*
	             *  The point should not be within the clustering distance of
		         *  point index-1 and point index2+1. 
	             */
	        	if ( checkDist && ( isCluster( coorB[ 0 ], snapPts.get( ii ) ) ||
	        					    isCluster( coorA[ 0 ], snapPts.get( ii ) ) ) ) {
	        		continue;
	        	}
	        	
	    	    /*
	             *  The point should not be within the clustering distance of
		         *  those points specified in checkPts. 
	             */
	        	boolean qualify = true;
	        	if ( checkPts != null ) {
	        	    for ( Coordinate cc : checkPts  ) {
	        		    if ( isCluster( cc, snapPts.get( ii ) ) ) {
	        			    qualify = false;	            
	        			    break;
	        		    }
	        	    }
	        	}

	        	if ( !qualify )  continue;
	        	
	    	    /*
	             *  If the dist <= tolerance and we are not in the "expansion"
		         *  mode, then the starting point is assumed to be a SNAP point
		         *  and we are done.  Otherwise, we need more checks.  
	             */
	        	double dist = distance( tPoint, snapPts.get( ii ) ) / PgenUtil.NM2M;
	        	if ( dist <= tolerance && !expandOnly )  {
	        		snappedPt = new Coordinate( snapPts.get( ii ) );
	        		done = true;
	        		break;
	        	}
	        	
	    	    /*
	             *  If the snap is outward (expandOnly) then we need to
	             *  consider whether the candidate point is inside the starting
	             *  figure.  If it is, we can't use it, regardless of distance.
	             *
	             *  Note: currently, expandOnly is True only for SMEAR functionality
		         *        to make the smear larger than the snapshots.  When
		         *        drawing/editing GFAs,  the flag is false and thus will
		         *        not require the checks within the "expandOnly" bracket.
	             */
	        	if ( expandOnly ) {
		        	                 
	        		coorB[1] = snapPts.get( ii );
	        		coorA[1] = snapPts.get( ii );
                    
	        		boolean accept = true;
	        		
	        		for ( jj = snap_indx1;  jj <= snap_indx2; jj++ ) {
	        			
	        			candidatePt = smearPts.get( ( jj + npts ) % npts );
/*	algorithm1        			
	        			if ( onLeft( candidatePt, coorB[0], coorB[1] ) ||
	        				!onLeft( candidatePt, coorA[0], coorA[1] )	) {
*/
	        			
/*	algorithm2
         			    if ( atLeft( coorB[0], coorB[1], candidatePt ) ||
	        				!atLeft( coorA[0], coorA[1], candidatePt )	) {
*/		        			
// Legacy algorithm	        			
	        			if ( atLeft( candidatePt, coorB, false, ONLINE_TOL ) ) {
	        				accept = false;
	        				break;
	        			}
	        			
	        			if ( !atLeft( candidatePt, coorA, false, ONLINE_TOL ) ) {
	        				accept = false;
	        				break;
	        			}	        					         	        			
	        		}
	        		
                    	        	
	        		if ( accept )  {

	        			if ( !firstSnap ) {
	        				firstSnap = true;
	        				firstSnapIndex = ii;
	        			}

	        			snappedPt = new Coordinate( snapPts.get( ii ) );
	        			status = 0;
	        			done = true;
	        			break;

	        		}

	        		if ( !done && ( ii == nclose - 1 ) && firstSnap &&
	        				                              firstSnapIndex >= 0 ) {
 
	        			//use the first acceptable snap point if no others are found
	        			snappedPt = new Coordinate( snapPts.get( firstSnapIndex ) );
	        			status = -2;
	        			done = true;
	        			break;

	        		}

	        	}
	        	else { // Non-expansion mode
	        			
       			    Coordinate[] line = new Coordinate[]{ tPoint, snapPts.get(ii) };
	        		Geometry seg = GfaClip.getInstance().pointsToGeometry( 
	        				       PgenUtil.latlonToGrid( line ) );       	
	        		
	        		if ( gfaPolygon == null || !gfaPolygon.isValid() || seg.intersection( gfaPolygon ).getCoordinates().length == 1 ) {
	        		    snappedPt = new Coordinate( snapPts.get( ii ) );
	        		    status = 0;
	        		    done = true;
	        		    break;
	        	    }
	        		
	        	}
	        }

	        if ( ii >= nclose ) {
	        	break;
	        }	        	
	        
	    }
	    
	    /*
	     * Insert a point for sparse snap point cases.
	     */
/*	    if ( status != 0 )  {
    		
	    	Geometry	aPt;
	    	for ( Coordinate cc : snapPts ) {
	    		aPt = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{cc} );
	    		if ( !gfaPolygon.intersects( aPt ) ) {	  //outside of the smear.  	
		            if ( onLeft( cc, coorL2[ 0 ], coorL2[ 1 ] ) ) {
		            	snappedPt = new Coordinate( cc );
		            	status = 2;
		            	break;
		            }
	    		}
		    }

	    }
*/	    
	    snapped[ 0 ] = new Coordinate( snappedPt );
  	        
		return status;
	}

	/**
	 *  Finds the nearest snap point to an input point.
	 * 
	 * @param	coor		point to be snapped
	 * @param	coor		point snapped
 	 */	
	public Coordinate snapOnePt ( Coordinate coor ) {
						
		//Default - return the original
        Coordinate snapped = coor;        
		
        //Find nearest snap point
        ArrayList<Coordinate>  snapPts = getNumSnapPoints( coor, 1 );
        if( snapPts.size() > 0 ) {
        	snapped = new Coordinate( snapPts.get( 0 ) );
		}
        
        return snapped;
        
	}
	

	/**
	 *  Sort snap points around a given point into a treeMap by simple distance.
	 * 
	 * Note: Needs to calculate the great circle distance to make it consistent
	 *       with legacy result.
	 * 
	 * @param coor
	 * @param num	number of closest points desired.
	 */
	private TreeMap<Double, Station> sortSnapPoints( Coordinate coor, int nclosest ) {
		
		TreeMap<Double, Station> tmapSimple = new TreeMap<Double, Station>();
		TreeMap<Double, Station> tmap = new TreeMap<Double, Station>();
		
		double dist;
		       
		for ( Station stn : snapStns  ) {
            
			double dx = coor.x - stn.getLongitude();
			double dy = coor.y - stn.getLatitude();
			dist = dx * dx + dy * dy;
						
			tmapSimple.put( dist, stn );
						
		}
		
		int ii = 0;       
		for ( Station stn : tmapSimple.values()  ) {            			
			dist = distance( coor, new Coordinate( stn.getLongitude(), stn.getLatitude() ) );			
			tmap.put( dist, stn );			
			
			ii++;
			
			if ( ii > (nclosest * 5) ) break;
			
		}
		
		return tmap;
	}
	
		
	/**
	 * Checks if two points is within a given clustering distance.
	 * 	 
	 * @param p1	first point
	 * @param p2	second point
	 */
	public boolean isCluster( Coordinate p1, Coordinate p2 ) {
		
		boolean clustered = false;
		
		double dist = distance( p1, p2 ) / PgenUtil.NM2M;
		if ( dist > 0 && dist <= CLUSTER_DIST ) {
			clustered = true;
		}
		
		return clustered;
	}

		
	/**
	 * Shift an ArrayList of Coordinate by 1 (0->1, 1->2,...)
	 *  
	 * @param list
	 * @param start
	 */
	public ArrayList<Coordinate> shiftArray( ArrayList<Coordinate> list ) {
		
		ArrayList<Coordinate> newlist = new ArrayList<Coordinate>();
		
		Coordinate first = list.get( 0 );
		List<Coordinate> after = list.subList( 1, list.size() );
		
		for ( Coordinate c : after ) {
		    newlist.add( new Coordinate( c ) );
		}
		
		newlist.add( new Coordinate( first ) );
				
		return newlist;
	}		
	

	/**
	 * Collapse the entire array back "nshift" points from a given index.
	 *  
	 * @param list
	 * @param start
	 */
	public ArrayList<Coordinate> collapseArray( ArrayList<Coordinate> list,
												 int start, int nshift ) {

		ArrayList<Coordinate> newlist = new ArrayList<Coordinate>();
		
		if ( start < 0 || nshift < 0 || start > ( list.size() - 1 ) || 
			 (start + nshift ) > list.size() ) {
		    // A exact copy
			for ( Coordinate c : list ) {
		        newlist.add( new Coordinate( c ) );
		    }

		}
		else { // Collapse back
		
		    List<Coordinate> before = list.subList( 0, start );
		    List<Coordinate> after = list.subList( start+nshift, list.size() );
		
		    for ( Coordinate c : before ) {
		        newlist.add( new Coordinate( c ) );
		    }

		    for ( Coordinate c : after ) {
		        newlist.add( new Coordinate( c ) );
		    }
		}
				
		return newlist;
	}

	
	/**
	 * Insert a value into an ArrayList at given location.
	 *  
	 * @param list
	 * @param index
	 * @param pt
	 * 
	 */
	public ArrayList<Coordinate> insertArray( ArrayList<Coordinate> list, int index,
											   Coordinate pt ) {
		
		ArrayList<Coordinate> newlist = new ArrayList<Coordinate>();
		
		List<Coordinate> before = list.subList( 0, index );
		List<Coordinate> after = list.subList( index, list.size() );
		
		for ( Coordinate c : before ) {
		    newlist.add( new Coordinate( c ) );
		}
		
		newlist.add( new Coordinate( pt ) );
				
		for ( Coordinate c : after ) {
		    newlist.add( new Coordinate( c ) );
		}
		
		return newlist;
	}
	
	
	/**
	 *  Get a given number of closest snap points around a given point.
	 * 
	 * @param coor
	 * @param num
	 */
	private ArrayList<Coordinate> getNumSnapPoints( Coordinate coor, int num ) {
		
		TreeMap<Double, Station> snapPtsTreeMap = sortSnapPoints( coor, num );
		
		ArrayList<Coordinate> snapPts = new ArrayList<Coordinate>();
		
		if ( num <= 0 ) {
			num = NUM_SNAP;
		}
		
		int nn = 0;
		while ( nn <= num && !snapPtsTreeMap.isEmpty() ) {
			
			Double key = snapPtsTreeMap.firstKey();
			Station s = snapPtsTreeMap.remove( key );
			
			snapPts.add( new Coordinate( s.getLongitude(), s.getLatitude() ) );
            nn++;
           
		}
		
		return snapPts;
	}
		
	/**
	 *  Order the points in clockwise.
	 * 
	 * @param coors		the array to be reordered
	 * @param gf		GeometryFactory
	 *     
	 * @return 			an array ordered in clockwise
	 */
	public ArrayList<Coordinate> reorderInClockwise( List<Coordinate> coors, GeometryFactory gf ) {

		ArrayList<Coordinate> newList = new ArrayList<Coordinate>();

		// Default to GfaFormat's same GeometryFactory.
		if ( gf == null ) gf = GfaFormat.getGeometryFactory();

		Coordinate[] tmp = new Coordinate[ coors.size() + 1 ];
		coors.toArray( tmp );
		tmp[ tmp.length - 1 ] = tmp[ 0 ];
		
		/*
		 * Reverse if the input is counter-clockwise
		 */
		if ( ! CGAlgorithms.isCCW( tmp ) ) {
			newList.addAll( coors );
		}
		else { 
			LinearRing shell = gf.createLinearRing( tmp );
			LineString ls = (LineString)shell.reverse();
			shell = gf.createLinearRing( ls.getCoordinates() );
			tmp = shell.getCoordinates();
						
			newList.addAll( Arrays.asList( tmp ) );
			newList.remove( newList.size() - 1 );
		}
						
		return newList;	
	}
	

	/**
	 * Check if a point is at the left of a given line segment.
	 * 
	 * Note: the input is assumed to be in map coordinate (lat/lon).
	 * 
	 * @param snapPt	point to be checked
	 * @param startPt		starting point of the segment
	 * @param endPt		end point of the segment
	 * @return
	 */
	public boolean onLeft( Coordinate snapPt, Coordinate start, Coordinate end ) {
		
		// Convert to grid coordinate.
		Coordinate[] points = new Coordinate[]{ snapPt, start, end };
		Coordinate[] gridPts = PgenUtil.latlonToGrid( points );
		
		Coordinate coorSnap = gridPts[0];
		Coordinate c1 = gridPts[1];
		Coordinate c2 = gridPts[2];
				
		// coorStart to coorEnd segment
		double theta1 = Math.atan2( c2.y - c1.y, c2.x - c1.x );

		// coorStart to coorSnap segment
		double theta2 = Math.atan2( coorSnap.y - c1.y, coorSnap.x - c1.x );
		
		// theta2-theta1
		double delta = theta2 - theta1;
		
		// if delta is not within (-180,180) then normalize
		if ( delta > Math.PI ) {
			delta -= 2*Math.PI;
		}
		else if ( delta < -Math.PI ) {
			delta += 2*Math.PI;
		}

		return delta > 0; // left side

	}
	
	/**
	 * Calculates the great-circle distance between an Earth point and
	 * a list of other Earth points.
	 * 
	 * Migrated from clodist.c
	 * 
	 * @param	startPt		the start point
	 * @param	endPts		a list of end points	
	 * @return	distance	a list of distances	
	 */
	private ArrayList<Double> distance( Coordinate startPt, ArrayList<Coordinate> endPts ) {
		
		ArrayList<Double> dist = new ArrayList<Double>();
		for ( Coordinate pt : endPts ) {
			dist.add( distance( startPt, pt ) );
		}
		
		return dist;
	}

	/**
	 * Calculates the great-circle distance between two	Earth points
	 * 
	 * Migrated from clodist.c
	 * 
	 * @param	startPt		starting point in lat/lon
	 * @param	endPt		end point in lat/lon
	 * @return	distance	the great-circle distance in meters
	 */
	public double distance( Coordinate startPt, Coordinate endPt ) {
		
		double dist = RMISSD;
		
		//Check the latitude/longitude for out of range.
		double lat1 = startPt.y;
		double lon1 = startPt.x;
		double lat2 = endPt.y;
		double lon2 = endPt.x;
		
		if  ( lat1 < -90.0F  || lat1 > 90.0F   ||
			  lon1 < -180.0F || lon1 >  180.0F ||
			  lat2 < -90.0F || lat2 > 90.0F    ||
			  lon2 < -180.0F || lon2 >  180.0F ) {
			dist = RMISSD;
		}				
		else {
				
			double dtr = Math.PI / 180.0;				
			double rlat1 = dtr * lat1;
			double rlat2 = dtr * lat2;
			double rlon1 = dtr * lon1;
			double rlon2 = dtr * lon2;

			double dlon = rlon1 - rlon2;

			//Compute the distance using spherical geometry.
			double val = Math.sin( rlat1 ) * Math.sin( rlat2 ) +
			             Math.cos( rlat1 ) * Math.cos( rlat2 ) * Math.cos( dlon );
            
			dist = 0.0;
			if  ( -1.0 <= val && val <= 1.0 )  {
				dist = EARTH_RADIUS * Math.acos( val );
			}
		}
		
		return dist;
	}
	
	/*
	 * Try to split all snap points into different parts and save to files.
	 */
	public void createSnapPointFiles() {
        				
		//Find snap points in an area
		ArrayList<Coordinate> pts = new ArrayList<Coordinate>();
		for (Station s : snapStns ) {			
			pts.add( new Coordinate(s.getLongitude(), s.getLatitude() ) );
		}
		
		ArrayList<Coordinate> ptsIn = new ArrayList<Coordinate>();
		for ( Coordinate c : pts ) {		
			if ( c.x > -79 && c.y > 22 &&  c.y < 35 ){
			    ptsIn.add( c );
			}
		}
 
		writeSnapFile( "snapTest", ptsIn );
		ptsIn.clear();
		
		//Find snap points outside of international bound		
		Geometry bnd = GfaClip.getInstance().getFaInternationalBound();
		
		for ( Coordinate c : pts ) {
			Geometry p = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
			if ( !bnd.contains(  p ) ) {
				ptsIn.add( c );
			}
		}
		
		writeSnapFile( "snapPointsOutside", ptsIn );
		ptsIn.clear();
		
		//Find snap points outside of international bound		
		HashMap<String, Geometry> areabnd = GfaClip.getInstance().getFaAreaBounds();
		Geometry SFO = areabnd.get("SFO");
		Geometry SLC = areabnd.get("SLC");
		Geometry CHI = areabnd.get("CHI");
		Geometry DFW = areabnd.get("DFW");
		Geometry BOS = areabnd.get("BOS");
		Geometry MIA = areabnd.get("MIA");
		
		ArrayList<Coordinate> ppp = new ArrayList<Coordinate>();

		//Find snap points for SFO		
		for ( Coordinate c : pts ) {
			if ( c.x <= -113 ){
			    ptsIn.add( c );
			}
		}
		
		for ( Coordinate c : ptsIn ) {
			Geometry p = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
			if ( !SLC.contains(  p ) ) {
				ppp.add( c );
			}
		}
		
		writeSnapFile( "snapPointsSFO", ppp );
		ptsIn.clear();
		ppp.clear();
		
		//Find snap points SLC	
		for ( Coordinate c : pts ) {
			if ( c.x >= -122 && c.x <= -101 ){
			    ptsIn.add( c );
			}
		}
		
		for ( Coordinate c : ptsIn ) {
			Geometry p = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
			if ( !SFO.contains( p ) && !CHI.contains( p ) && !DFW.contains( p )) {
				ppp.add( c );
			}
		}
				
		writeSnapFile( "snapPointsSLC", ppp );
		ptsIn.clear();
		ppp.clear();

		//Find snap points CHI	
		for ( Coordinate c : pts ) {
			if ( c.x >= -105 && c.x <= -81 && c.y >= 36 ){
			    ptsIn.add( c );
			}
		}
		
		for ( Coordinate c : ptsIn ) {
			Geometry p = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
			if ( !SLC.contains( p ) && !BOS.contains( p ) && !DFW.contains( p ) && !MIA.contains( p )) {
				ppp.add( c );
			}
		}
				
		writeSnapFile( "snapPointsCHI", ppp );
		ptsIn.clear();
		ppp.clear();

		//Find snap points DFW	
		for ( Coordinate c : pts ) {
			if ( c.x >= -107 && c.x <= -81 && c.y <= 37.5 ){
			    ptsIn.add( c );
			}
		}
		
		for ( Coordinate c : ptsIn ) {
			Geometry p = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
			if ( !SLC.contains( p ) && !CHI.contains( p ) && !MIA.contains( p ) && !BOS.contains( p )) {
				ppp.add( c );
			}
		}
		
		writeSnapFile( "snapPointsDFW", ppp );
		ptsIn.clear();
		ppp.clear();

		//Find snap points BOS	
		for ( Coordinate c : pts ) {
			if ( c.x >= -86 && c.y >= 36 ){
			    ptsIn.add( c );
			}
		}
		for ( Coordinate c : ptsIn ) {
			Geometry p = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
			if ( !MIA.contains( p ) && !CHI.contains( p ) && !DFW.contains( p )) {
				ppp.add( c );
			}
		}
		
		writeSnapFile( "snapPointsBOS", ppp );
		ptsIn.clear();
		ppp.clear();
	
		//Find snap points MIA	
		for ( Coordinate c : pts ) {
			if ( c.x >= -88.5 && c.y <= 37.5 ){
			    ptsIn.add( c );
			}
		}
		for ( Coordinate c : ptsIn ) {
			Geometry p = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
			if ( !BOS.contains( p ) && !CHI.contains( p ) && !DFW.contains( p )) {
				ppp.add( c );
			}
		}
		
		writeSnapFile( "snapPointsMIA", ppp );
		ptsIn.clear();
		ppp.clear();
	}
	
	/*
	 * Write a set of points into a PGEN file - each point represents a Symbol.
	 */
	private void writeSnapFile( String fname, List<Coordinate> pts ) {
		
		//Default product/layer to hold symbols
		Product activeProduct = new Product("Default", "Default", "Default",
	  		      new ProductInfo(), new ProductTime(), new ArrayList<Layer>() );
		    
		Layer activeLayer = new Layer();
		activeProduct.addLayer( activeLayer );
		    
		List<Product> productList = new ArrayList<Product>();
		productList.add( activeProduct );
		
		for ( Coordinate c : pts ) {
    	     Symbol cmm = new Symbol( null, new Color[] {Color.green},
        			1.0F, 0.5, false, c, "Symbol","TRIANGLE_SPCL" );  
    	     activeLayer.add( cmm );
		}

		System.out.println( "Total snap stations for " + fname + ": " + pts.size() );
        Products filePrds1 = ProductConverter.convert( productList  );
        String aa = "/export/cdbsrv/jwu/" + fname + ".xml";
        FileTools.write( aa, filePrds1 ); 		
	}
	
	
	/*
	 * Check if a point is within the FA international bound.
	 * 
	 * @param pt	pt in MAP coordinate.
	 */
	private boolean isInFaBound( Coordinate pt ) {
		
		Geometry intlBoundInGrid = GfaClip.getInstance().getFaInternationalBoundInGrid();
		
		Coordinate[] points = new Coordinate[]{ pt };
		Coordinate[] ptInGrid = PgenUtil.latlonToGrid( points );
		
		Geometry ptGeom = GfaClip.getInstance().pointsToGeometry( ptInGrid );
		
		return intlBoundInGrid.covers( ptGeom );
		
	}	
	
	/**
	 * Compares two MAP Coordinate objects by comparing x and y coordinates. If the coordinates
	 * are close within a precision, returns true, otherwise false.
	 * 
	 * @param c1
	 * @param c2
	 * 
	 * @return true is c1 and c2 are very close, false otherwise
	 */
	public boolean isSamePoint( Coordinate c1, Coordinate c2, double pres ) {
		
		if ( abs( c1.x - c2.x ) < pres && abs( c1.y - c2.y ) < pres ) {
			return true;
		}
		
		return false;
	}

	/*
	 * Check if a point is already a snap point.
	 * 
	 * The input list of snap points is assumed to be the closest snap points 
	 * around the point to be snapped and sorted via distance.
	 * 
	 * @param cc
	 * @param snapPtsList
	 * 
	 * @return true cc is a snap point already, false otherwise
	 */
	private boolean isSnapPoint( Coordinate cc, ArrayList<Coordinate> snapPtsList ) {
		
		if ( snapPtsList.size() >= 0 && isSamePoint( cc, snapPtsList.get( 0 ), MAP_PRECISION ) ) {
			return true;
		}
		else {
			return false;		    	
		}
		
	}

	/*
	 * Check if a point is already in a list (within a precision)
	 * 
	 * @param cc
	 * @param clist
	 * 
	 * @return true is cc is very close to one of the points in clist, false otherwise
	 */
	private boolean isInList( Coordinate cc, ArrayList<Coordinate> clist ) {
		
		for ( Coordinate pp : clist ) {
		    if ( isSamePoint( cc, pp, MAP_PRECISION ) ) {
			    return true;
		    }
		}
		
		return false;
	}

	/**
	 *  Determine if a point c3 is to the left of a line from c1 to c2.
	 *  
	 *  For input points p1 = (x1,y1), p2 = (x2,y2), p3 = (x3,y3), computes the value of 
	 *  | x1 y1 1 |
	 *  | x2 y2 1 |
	 *  | x3 y3 1 |
	 * 
	 *  value > 0,  c3 is to the left of the line from c1 to c2; 
	 *  value = 0,  c3 is on the line
	 *  value < 0   c3 is to the right of the line.
	 *
	 * @param c1
	 * @param c2
	 * @param c3
	 * 
	 * @return boolean	  Position indicator:  1-right of line; 0 - on the line; -1 - left of line
	 */
	 private boolean atLeft( Coordinate c1, Coordinate c2, Coordinate c3 ) {
	     
	     // Convert to grid coordinate.
		 Coordinate[] points = new Coordinate[]{ c1, c2, c3 };
		 Coordinate[] gridPts = PgenUtil.latlonToGrid( points );
			
		 Coordinate p1 = gridPts[0];
		 Coordinate p2 = gridPts[1];
		 Coordinate p3 = gridPts[2];
		 
		 double value = p1.x * p2.y + p3.x * p1.y + p2.x * p3.y 
			                - p3.x * p2.y - p2.x * p1.y - p1.x * p3.y; 
		 
		 return  value > 0;
	 }  

	 /*
	  *  Determines if a given point is to the right of a multi-points line whose direction 
	  *  is determined by order of points. The evaluation tolerance is used to determine if 
	  *  a point is on the line. A tolerance of 0.0F forces the point to be COMPUTATIONALLY 
	  *  EXACTLY on the line regardless of whether the point is THEORETICALLY EXACTLY on the 
	  *  line.	
	  *  
	  *  Note: the input is assumed to be in MAP coordinate.
	  *  				
	  * @param pp			point to be checked
	  * @param line 		line to be checked against
	  * @param closed 		if the line is closed
	  * @param tolerance 	evaluation tolerance
	  * 
	  * @return boolean	  true -left of or on line; false - right of line
	  */
     private boolean atLeft( Coordinate pp, Coordinate[] line, boolean closed, double tolerance ) {
    	 
    	 // Check if closed line has duplicate end point.
    	 boolean clsd = closed;	
    	 boolean duplicate;
    	 if ( isSamePoint( line[0], line[ line.length - 1], MAP_PRECISION ) ) {
    		 duplicate = true;
    	 }
    	 else {
    		 duplicate = false;			 
    	 }

    	 if ( duplicate && ! clsd ) clsd = true;


    	 // Convert to grid coordinate.
    	 Coordinate[] points = new Coordinate[]{ pp };
    	 Coordinate[] gridPts = PgenUtil.latlonToGrid( points );

    	 Coordinate cp = gridPts[0];

    	 Coordinate[] linep = PgenUtil.latlonToGrid( line );
    	 
    	 // Query the position.
    	 int flag = queryPointPosition( cp, linep, clsd, duplicate, tolerance );
        	 
         return flag <= 0;
         
    }
    
    /*
     *  Determines if a given point is to the right of a multi-points line whose direction 
     *  is determined by order of points. The evaluation tolerance is used to determine if 
     *  a point is on the line. A tolerance of 0.0F forces the point to be COMPUTATIONALLY 
     *  EXACTLY on the line regardless of whether the point is THEORETICALLY EXACTLY on the 
     *  line.	
     *  
     *  This code is converted from legacy cgrqrol.c.
     *  
     *  Note: even though no coordinate system is assumed here, a Cartesian system is
     *        preferred (e.g., Device, Grid, Pixel).
     *  				
     * @param pp			point to be checked
     * @param line 		    line to be checked against
     * @param closed 		if the line is closed
     * @param closed 		if the first and the last point of the line are duplicated
     * @param tolerance 	evaluation tolerance
     * 
     * 
     * @return boolean	  Position indicator:  1-right of line; 0 - on the line; -1 - left of line
     */
    private int queryPointPosition( Coordinate cp, Coordinate[] linep, boolean closed, boolean duplicate,
    		                        double tolerance ) {
    	 
    	 int flag = 0;
		 
	     // Check if closed line has duplicate end point.
		 boolean clsd = closed;			 
		 if ( duplicate && ! clsd ) clsd = true;
		 
		 //Get the nearest point on the line from the given point.
		 int[] indx = new int[ 2 ];
		 double[] dinfo = segmentDist( linep, cp, indx );
		 
		 int nearest_vtx = indx[ 0 ];
		 int next_vtx = indx[ 1 ];
		 double xner = dinfo[ 0 ]; 
		 double yner = dinfo[ 1 ];
		 boolean linend;
		 int np = linep.length;
		 
		 int l0, l1, l2;
		 double[] va = new double[3], vb = new double[ 3 ],  vc;
		 double z1, z2, z3;
		 
		 /*
		  * Check if the nearest point is a segment end-point.
		  */
		 if ( ( gdiff( xner, linep[nearest_vtx].x, GDIFFD ) && gdiff( yner, linep[nearest_vtx].y, GDIFFD) ) ||
			  ( gdiff( xner, linep[next_vtx].x, GDIFFD )  && gdiff( yner, linep[next_vtx].y, GDIFFD ) ) ) {
			
			 //Check if the nearest point is also a line end-point.
			 if ( ( gdiff( xner, linep[0].x, GDIFFD) && gdiff( yner, linep[0].y, GDIFFD) ) ||
				  ( gdiff( xner, linep[np-1].x, GDIFFD) && gdiff( yner, linep[np-1].y, GDIFFD ) ) ) {
				 linend = true;
			 } else {
				 linend = false;
			 }

			 if ( clsd || ( ! clsd && ! linend ) ) {
				 l1 = nearest_vtx;
				 l0 = l1 - 1;
				 l2 = l1 + 1;
				 if ( l0 < 0 ) {
					 if ( duplicate ) {
						 l0 = np - 2;
					 } else {
						 l0 = np - 1;
					 }
				 }
				 if ( l2 == np ) {
					 if ( duplicate ) {
						 l2 = 1;
					 } else {
						 l2 = 0;
					 }
				 }

				 va[0] = cp.x - linep[l0].x;
				 va[1] = cp.y - linep[l0].y;
				 vb[0] = linep[l1].x - linep[l0].x;
				 vb[1] = linep[l1].y - linep[l0].y;
				 vc = crossProduct( va, vb );
				 z1 = vc[2];

				 va[0] = cp.x - linep[l1].x;
				 va[1] = cp.y - linep[l1].y;
				 vb[0] = linep[l2].x - linep[l1].x;
				 vb[1] = linep[l2].y - linep[l1].y;
				 vc = crossProduct( va, vb );
				 z2 = vc[2];

				 va[0] = linep[l1].x - linep[l0].x;
				 va[1] = linep[l1].y - linep[l0].y;
				 vb[0] = linep[l2].x - linep[l1].x;
				 vb[1] = linep[l2].y- linep[l1].y;
				 vc = crossProduct ( va, vb );
				 z3 = vc[2];

				 if ( gdiff( z1 * z2, 0.0F, tolerance ) ) {
					 if ( z3 > 0.0F ) {
						 flag = ( z1 > 0.0F || z2 > 0.0F ) ? 1 : 0;
					 } else if ( z3 < 0.0F ) {
						 flag = ( z1 < 0.0F || z2 < 0.0F ) ? -1 : 0;
					 } else {
						 flag = 0;
					 }
				 } else if ( z1 * z2 > 0.0F ) {
					 flag = z1 > 0.0F ? 1 : -1;
				 } else {
					 flag = z3 > 0.0 ? 1 : -1;
				 }
			 } else {
				 l1 = Math.min( nearest_vtx, next_vtx );
				 l2 = l1 + 1;
				 va[0] = cp.x - linep[l1].x;
				 va[1] = cp.y - linep[l1].y;
				 vb[0] = linep[l2].x - linep[l1].x;
				 vb[1] = linep[l2].y - linep[l1].y;
				 vc = crossProduct( va, vb );
				 flag = ( gdiff( vc[2], 0.0F, tolerance ) ? 0 : vc[2] > 0.0 ? 1 : -1 );
			 }
		 } else {
			 l1 = Math.min( nearest_vtx, next_vtx );
			 l2 = l1 + 1;
			 va[0] = cp.x - linep[l1].x;
			 va[1] = cp.y - linep[l1].y;
			 vb[0] = linep[l2].x - linep[l1].x;
			 vb[1] = linep[l2].y - linep[l1].y;
			 vc = crossProduct( va, vb );
			 flag = ( gdiff ( vc[2], 0.0F, tolerance ) ? 0 : vc[2] > 0.0 ? 1 : -1 );
		 }
		 		 
		 return  flag;
	 }  
	 
	 
	 /*
	  * Computes the cross product of two three dimensional	vectors
	  * 
	  * This code is from legacy cgrvectxprod.c
	  * 
	  * @param va 	Vector A
	  * @param va 	Vector B
	  * 
	  * @return vc	Cross product vector
	  */
	 private double[] crossProduct( double[] va, double vb[] ) {
		 
		 double[] vc = new double[ 3 ];
		 
		 vc[0] = va[1] * vb[2] - va[2] * vb[1];
		 vc[1] = va[2] * vb[0] - va[0] * vb[2];
		 vc[2] = va[0] * vb[1] - va[1] * vb[0];
		 
		 return vc;
	 }
	 
	 
	 /*
	  * Determines the nearest and next vertices of a multi-point line to a fixed point, 
	  * the closest point (on the line segment defined by those two vertices) to the fixed
	  * point, and the distance between the fixed point and the closest point. 
	  * 
	  * The "next vertex" is simply the other vertex of the closest segment than the nearest 
	  * vertex, not the next closest vertex to the fixed point.
	  * 
	  * This code is coverted from legacy cgrsegdist.c
	  * 
	  * @param line		line to be checked against
	  * @param fixPt	point in check
	  * @param index	indexes of the nearest and next vertices
	  * 
	  * @return dist	dist[0] - Distance to the point
	  * 				dist[1] - Nearest x coord on figure
	  * 				dist[2] - Nearest y coord on figure
	  */
	 private double[] segmentDist( Coordinate[] line, Coordinate fixPt, int[] index ) {
		 
		 double[] dist = new double[ 3 ];
		 
		 int np = line.length;
		 
		 if ( np == 1 ) {
				
			 index[ 0 ] = 0;
			 index[ 1 ] = 0;
			 
			 dist[ 0 ] = line[ 0 ].x;
			 dist[ 1 ] = line[ 0 ].y;
			 dist[ 2 ] = gdist( line[ 0 ].x, line[ 0 ].y, fixPt.x, fixPt.y );
			 
			 return dist;
			 
		 }
		 
		 double dt = 1.0e38;
		 double qx, qy, m1, m2, b1, b2, curDist, d0, d1;
		 

		 //Isolate which line segment is closest to desired point.
		 for ( int ii = 0; ii < np-1; ii++ ) {

			 double xmin = Math.min( line[ii].x, line[ii+1].x );
			 double xmax = Math.max( line[ii].x, line[ii+1].x );
			 double ymin = Math.min( line[ii].y, line[ii+1].y );
			 double ymax = Math.max( line[ii].y, line[ii+1].y );

			 /*
			  * Must find the closest point on vertical and horizontal
			  * separately since the slope formula would cause a 
			  * divide by zero error
			  */

			 //Vertical segments
			 if ( gdiff( xmin, xmax, GDIFFD ) ) {
				 qx = xmin;
				 if ( fixPt.y < ymin)
					 qy = ymin;
				 else if ( fixPt.y > ymax)
					 qy = ymax;
				 else
					 qy = fixPt.y;
			 }
			
			 //Horizontal segments
			 else if ( gdiff( ymin, ymax, GDIFFD ) ) {
				 qy = ymin;
				 if ( fixPt.x < xmin )
					 qx = xmin;
				 else if ( fixPt.x > xmax)
					 qx = xmax;
				 else
					 qx = fixPt.x;
			 }

			 //All the rest
			 else {

				 //find slope and intercept for initial line
				 m1 = ( line[ii+1].y - line[ii].y ) / ( line[ii+1].x - line[ii].x );
				 b1 = line[ii].y - (m1 * line[ii].x );

				 //find slope and intercept for perpendicular
				 m2 = - 1.0F / m1;
				 b2 = fixPt.y - (m2 * fixPt.x );

				 /* 
				  * find the intersection of the two lines
				  * which would be the closest point
				  *
				  * formula for a line is y = mx + b
				  * y = (m1 * x) + b1  &&  y = (m2 * x) + b2
				  * (m1 * x) + b1 = (m2 * x) + b2
				  * (m1 * x) - (m2 * x) = (b2 - b1)
				  * x * (m1 - m2) = (b2 - b1)
				  * x = (b2 - b1) / (m1 - m2)
				  */
				 qx = (b2 - b1) / (m1 - m2);
				 qy = (m2 * qx) + b2;
			 }
			 
			 //find the distance
			 if ( xmin <= qx && qx <= xmax ) {
				 curDist = gdist( fixPt.x, fixPt.y, qx, qy );
			 }
			 else {
				 d0 = gdist( fixPt.x, fixPt.y, line[ii].x, line[ii].y );
				 d1 = gdist( fixPt.x, fixPt.y, line[ii+1].x, line[ii+1].y );
				 curDist = ( d0 <= d1 ) ? d0 : d1;
			 }
			 
			 
			 if ( curDist < dt ) {
				 
				 dt = curDist;

				 dist[ 0 ] = qx;
				 dist[ 1 ] = qy;	

				 //Figure which end of segment is closest to point.
				 d0 = gdist( fixPt.x, fixPt.y, line[ii].x, line[ii].y );
				 d1 = gdist( fixPt.x, fixPt.y, line[ii+1].x, line[ii+1].y );

				 if ( d0 < d1 ) {
					 index[0] = ii;
					 index[1] = ii + 1;
				 }
				 else {
					 index[0] = ii + 1;
					 index[1] = ii;
				 }

				 if ( ( dist[0] < xmin) || (xmax < dist[0]) ) {
					 dist[0] = line[ index[0] ].x;
					 dist[1] = line[ index[0] ].y;
				 } 
			 }					 
			 
		 }
		 
		 return dist;
	 }
	 
	 /*
	  * Compare floating point equality
	  */
	 private boolean gdiff( double v1, double v2, double pres ) {
		 return ( Math.abs(  v1 - v2 ) < pres );
	 }
		 
	 /*
	  * Compute distance of two points
	  */
	 private double gdist( double x1, double y1, double x2, double y2 ) {
//		 return Math.sqrt( ( x2 - x1 ) * ( x2 - x1 ) + ( y2- y1 ) * ( y2 - y1 ) );
		 return ( x2 - x1 ) * ( x2 - x1 ) + ( y2- y1 ) * ( y2 - y1 );
	 }
	 
}


