/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaReducePoint
 * 
 * May 2011
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.TreeSet;

//import org.apache.log4j.Logger;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * GFA point reduction algorithms, including regular point reduction, 
 * area clipping and bisection splitting.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/11					J. Wu		Initial creation
 * 01/11					J. Wu		Compute in grid coordinate
 * 										instead of in map coordinate. 
 * 06/11                    Q. Zhou     Added the call reduceByPctDist to reducePoints()
 * 06/11					J. Wu		No area clipping if two areas are not in the
 *                                      same FA region.
 * 07/11					J. Wu		Force the same logic as in legacy.
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 1
 */
public class GfaReducePoint {

//	private final static Logger logger = Logger.getLogger(GfaClip.class);

	/**
	 *  SMEAR_INCR_PCT - maximum percentage allowed for the increase in size
	 *	                 when a point is removed from a smeared GFA polygon 
	 *					 in the AIRMET formatter.
	 *
	 *  SMEAR_INCR_DST - maximum distance allowed between a new point and the
	 *  				 point to be removed from a smeared GFA polygon.
	 *  
	 *  REDUCEPTS_INCR_PCT_ORIG - maximum size increase of a GFA polygon, as a percentage, 
	 *					when reducing points to format on three lines.
	 *  
	 */
	private static final int SMEAR_INCR_PCT  = 3;
	private static final int SMEAR_INCR_DST  = 100;
	private static final int REDUCEPTS_INCR_PCT_ORIG = 3;
	
	/**
	 *  GFA formatting bisection percentage maximum
	 */                                   
	private static final int GFA_AF_BISECT_PCT = 60;

	/**
	 *  Bisecting middle point
	 */                                   
	private static final String BISECT_MIDDLE_POINT	= "BISECT_MIDDLE_POINT";
	
	/**
	 * Static instance
	 */
	private static GfaReducePoint instance = new GfaReducePoint();

	/**
	 * Private constructor
	 */
	private GfaReducePoint() {
	}

	/**
	 * Singleton instance.
	 * 
	 * @return
	 */
	public static GfaReducePoint getInstance() {
		return instance;
	}
   
	/**
     * Reduces the number of points GFA smears to allow it to be represented 
     * on three 65-character lines of text.
	 * 
	 * The point reduction goes through two phases:	
	 * 
	 * Phase I - It first tries to remove allowable points, one at a time,
	 * based on the impact their individual removal would have on the 
	 * size of the polygon.  Specifically, remove points that increase
	 * the size of the polygon the least,  while not increasing the overall
	 * size of the polygon SMEAR_INCREASE_PCT and not allowing any new
	 * points to be SMEAR_INCR_DST distance from the original polygon
	 * points. The parameters SMEAR_INCREASE_PCT and SMEAR_INCR_DST are set
	 * in prefs.tbl. SMEAR_INCREASE_PCT refers to the area percentage 
	 * increase when a single point is removed from the polygon. Point
	 * reduction continues until the polygon can be represented on three 
	 * 65-character lines of text, or no more points can be removed under
	 * the above criteria.							
	 * 
	 * Phase II - If the polygon still cannot be formatted on three 
	 * 65-character lines, logically divide it into two or more sections:
	 * 
	 * 1. FA Area Boundary Clip: First, try to divide the polygon along the	
	 * FA Area boundary. If this can be accomplished, check the overall size
	 * of each part. Any part less than 3K sq nm will remain with the larger
	 * portion; any part greater than 3K sq nm will become another AIRMET.
	 *	
	 * Note that the areal clipping behaves the same as the regional
	 * clipping and we should make intact the boundary between two FA areas,
	 * which means if a polygon is clipped into two sections in two areas,
	 * they must share the exact same points along the boundary.
	 *
	 * 2. Simple Bisection: Check the results from the FA Area Boundary
	 * Bisection. Any section that cannot be formatted in three 65-character
	 * lines of text must be divided. Choose as a dividing segment the two
	 * closest opposite points in the polygon array which also does not 
	 * intersect any part of the polygon. Determine the midpoint of this 
	 * segment and snap to the closest snap point. Bisect the polygon using	
	 * this three-point segment into two new AIRMET polygons. Again, check
	 * to make sure these two polygons can be formatted. If not, re-divide
	 * again except without a midpoint. Two bisections should be sufficient
	 * to format correctly. If not, the user must break it up manually
	 * (training issue).
     *
	 * Note: new smears may be created and added in this routine.
	 *
	 * @param	glist		list of GFA smears
	 * @param	snapshots	GFA snapshots associated with this set of smears
	 */ 
	public void reducePoints ( ArrayList<Gfa> glist, ArrayList<Gfa> snapshots) {

		ArrayList<Gfa> smearList = new ArrayList<Gfa>(); 

		// Do area clipping and bisection for each smear.
		for ( Gfa gg : glist ) {
			smearList.addAll( reducePoints( gg, snapshots ) );; 
		}
		
		// Replace the smear list with the new list.
		glist.clear();
		glist.addAll( smearList );
		
	}
    
	/**
	 *  Reduce points for a single smear.
	 * 
	 * @param smear		the Gfa smear
	 * @param snapshots	GFA snapshots associated with this set of smears
	 */
	public ArrayList<Gfa> reducePoints( Gfa smear, ArrayList<Gfa> snapshots ) {
				
		ArrayList<Gfa> glist = new ArrayList<Gfa>();
      
		/*
		 * Step 1 - reduce points on the whole smear polygon.  Note that freezing level 
		 * contours are point reduced in af_fzlvl2fmt().
		 */ 		
		Gfa tSmear = regularPointReduction( smear );
		
		/*
		 * Create a smear with the reduced points and return if the smear can be 
		 * formatted into 3-line text now.
		 */
		if ( canBeFormatted( tSmear ) ) {
		    smear.setPointsOnly( tSmear.getPoints() );
		    smear.setReduceFlags( tSmear.getReduceFlags() );
		    glist.add( smear );
		    return glist;
		}
        					
		/*
		 * Step 2 - Do area clipping to split up the ORIGINAL smear 
		 * 			along area boundary, if needed.
		 */
		ArrayList<Gfa> listOfAreaClip = new ArrayList<Gfa>();
		listOfAreaClip.addAll( faAreaClip( smear, snapshots ) );
		
		// Step 3 - Do first round of bisection, if necessary. 		
		ArrayList<Gfa> listOfFirstBisect = new ArrayList<Gfa>();		
		for ( Gfa gg : listOfAreaClip ) {		    			
			
			//First do point reduction on each of the clipped smears.
			Gfa tmpSmear = regularPointReduction( gg );
    		gg.setPointsOnly( tmpSmear.getPoints() );
    		gg.setReduceFlags( tmpSmear.getReduceFlags() );
			
            if ( canBeFormatted( tmpSmear ) ) {
                glist.add( gg );              	
            }
            else {
               listOfFirstBisect.addAll( bisect( gg, true ) );
            }
		}
		
		//See if any middle point are generated in bisection.
		Coordinate midPt = null;
		if ( listOfFirstBisect.size() > 0  ) {
		    midPt = (Coordinate)listOfFirstBisect.get(0).getAttribute( BISECT_MIDDLE_POINT );
		}
		
		// Step 4 - Do second round of bisection, if necessary. 
		for ( Gfa gg : listOfFirstBisect ) {		    
			
			//First do point reduction on each of the bisected smears.
			Gfa tmpSmear = regularPointReduction( gg );						
			gg.setPointsOnly( tmpSmear.getPoints() );
			gg.setReduceFlags( tmpSmear.getReduceFlags() );
			
			if ( canBeFormatted( tmpSmear ) ) {
				glist.add( gg ); 
			}
			else { 
				//Another round of bisect, done.
				glist.addAll( bisect( gg, false) );
			}
		}		

		
		//
		// If bisection happens, check if the middle point generated in the first round
		// has been used for the second round. If not, remove the middle point from 
		// all smears.
		//
		boolean removeMidPt = false;
		if ( midPt != null )  {
			int ii = 0;
			for ( Gfa gg : glist ) {
				for ( Coordinate cc : gg.getPoints() ) {
					if ( GfaSnap.getInstance().isSamePoint( cc , midPt, GfaSnap.MAP_PRECISION ) ) {
						ii++;
						break;
					}
				}
			}
			
			if ( ii < 3 ) {
				removeMidPt = true;
			}
		}

		if ( removeMidPt ) {
			
			for ( Gfa gg : glist ) {
				Coordinate removePt = null;
				for ( Coordinate cc : gg.getPoints() ) {
					if ( GfaSnap.getInstance().isSamePoint( cc , midPt, GfaSnap.MAP_PRECISION ) ) {
						removePt = cc;
						break;
					}
				}
				
				if ( removePt != null ) gg.getPoints().remove( removePt ) ;
			}			
		}
														
		return glist;

	}
	
	/**
	 *  Check if a smear can be formatted into 3 65-character lines
	 * @param glist
	 */
	private boolean canBeFormatted( Gfa smear ) {
		
		boolean formattable = false;
		if ( !smear.isSnapshot() ) {                    
		    String prefix = getPrefixString( smear );;
		    formattable = 	canBeFormatted( smear.getPoints(), prefix );
		}
		
	    return formattable;
	    
//		return  ( smear.getLinePoints().length <= 5 ) ? true : false;  //test purpose only
		
	}
	
	/*
	 *  Return the proper prefix string based on type of the smear.
	 *  
	 * @param glist
	 */
	private String getPrefixString( Gfa smear ) {
		        
		String prefix = "";
		if ( !smear.isSnapshot() ) {
		    if ( smear.isOutlook() ) {
		    	prefix = Gfa.BOUNDED_BY;
		    }
		    else {
		    	prefix = Gfa.FROM;
		    }		    
		}
		
	    return prefix;
		
	}
	
	/*
	 *  Check if a list of points can be formatted into 3 65-character lines
	 *  
	 *  Note: the points are assumed to be in map coordinate.
	 *  
	 * @param pts
	 * @param prefix
	 */
	private boolean canBeFormatted( ArrayList<Coordinate> pts, String prefix ) {				
	    return ReduceGfaPointsUtil.canFormatted( pts, prefix );	    		
	}

	/**
	 * Clips the smear polygon against its FA area AND adjacent area if the 
	 * points on the polygon cannot be represented on three 65-character lines 
	 * of text.  Each clipped part with an area > 3K square nautical miles	
	 * will produce a new smear. 
	 * 
	 * The input is assumed to be in MAP coordinate.
	 * 
	 * Note: 1. If a smear has ONLY one FA area, no clipping  will be done, and 
	 *          the original smear is returned.
	 *       2. If a smear belongs two FA areas (a few little kinks less than
	 *          3K along the boundary could add up to an area > 3K), but these 
	 *          two FA areas do not belong to the same FA region, do not do 
	 *          area clipping.
	 *
	 * @param	smear		the GFA smear
	 * @param	snapshots	GFA snapshots associated with this set of smears
	 * @return  ArrayList	A list of smears after clipping. 
	 *
	 */
	private  ArrayList<Gfa> faAreaClip( Gfa smear, ArrayList<Gfa> snapshots ) {
    
		ArrayList<Gfa> newSmears = new ArrayList<Gfa>();
		
		//If smear can be formatted, return.
		if ( canBeFormatted( smear ) ) {
			newSmears.add( smear );
			return newSmears;
		}

		/*
		 * If the smear resides only in one FA area or two areas belong to different 
		 * FA regions, no need to clip. 
		 */
		String faAreas = smear.getGfaArea();
		if ( !faAreas.contains( "-" ) ||
			 !isSameFaRegion( faAreas ) ) {
			newSmears.add( smear );
			return newSmears;
		}
		
		

		// Retrieve extended FA area bounds
		HashMap<String, Geometry> xareaBnds = GfaClip.getInstance().getFaAreaXBoundsInGrid();

		
		/*
		 *  Find the intersection points with the extended FA area bounds and snap them.  
		 */
		Polygon smearPoly = GfaClip.getInstance().gfaToPolygon( smear );
		Polygon smearPolyInGrid = GfaClip.getInstance().gfaToPolygonInGrid( smear );
		HashMap<Coordinate, Coordinate> areaInterPts = getAreaIntersectionPt( smearPoly );

		/*
		 *  Do area clipping. The strategy is the same as regional clipping.
		 *  
		 *  1. clip the smear polygon against each FA region (WEST, CENTRAL, EAST)
		 *  2. create a list of parts bigger than/equal to 3K for each region
		 *  3. hold the parts smaller than 3K for all regions in another list.         
		 */
		HashMap<String, ArrayList<Geometry> > clipWithAreas = new HashMap<String, ArrayList<Geometry> >();                       

		ArrayList<Geometry> smallPoly = new ArrayList<Geometry>();                     

		for ( String areaName : xareaBnds.keySet() ) {

			if ( !faAreas.contains( areaName ) ) continue;

			clipWithAreas.put( areaName, new ArrayList<Geometry>() );

			//Do area clipping as described above.
			Geometry areaBnd = xareaBnds.get( areaName );				 
			if ( areaBnd.intersects( smearPolyInGrid ) ) {

				Geometry areaPoly = areaBnd.intersection( smearPolyInGrid );	

				if ( areaPoly != null ) {					        							
					for ( int kk = 0; kk < areaPoly.getNumGeometries(); kk++ )  {					    	    
						Geometry bigPoly = areaPoly.getGeometryN( kk );					    	        

						if ( GfaClip.getInstance().isBiggerInGrid( bigPoly ) )  {					        	
							clipWithAreas.get( areaName ).add( bigPoly ); 
						}
						else {
							smallPoly.add( bigPoly ); 
						}
					}
				}                
			}	   

		}								

		/*
		 *  Create smears from VALID parts and add into the return list
		 *  
		 *  1. Union the parts in each FA region with the list of small parts -
		 *     If a small part touches one of the bigs (only one, I believe), it
		 *     will be united into that big and thus be included into a new
		 *     smear.  So  no part gets lost and all will be included eventually.
		 *  2. When a small part is united into a big one (if they intersect), 
		 *     the common points are still retained in the result, we need to 
		 *     remove those extra point. The common points are resulted from the
		 *     previous clipping with the area boundaries.
		 *  3. check the resulting union for VALID geometries and create new
		 *     smears for each of them.	    			
		 */
		for ( String areaName : clipWithAreas.keySet() ) {

			ArrayList<Geometry>  bigs = clipWithAreas.get( areaName );

			ArrayList<Geometry> toBeUnioned = new ArrayList<Geometry>();
			toBeUnioned.addAll( bigs );
			toBeUnioned.addAll( smallPoly );
			Geometry result = GfaClip.getInstance().quickUnion( toBeUnioned );           			

			ArrayList<Coordinate> commPts = GfaClip.getInstance().getCommonPoints( bigs, smallPoly );

			if ( result != null ) {		    

				for ( int kk = 0; kk < result.getNumGeometries(); kk++ ) {

					Geometry one = result.getGeometryN( kk );

					//Remove residue points by clipping followed by subsequent union.
					Geometry onePart = GfaClip.getInstance().removeCommonPoints( one, commPts );

					//Clean up some improper cases
					Coordinate[] gPts = PgenUtil.gridToLatlon( onePart.getCoordinates() );
					ArrayList<Coordinate> points = new ArrayList<Coordinate>();
					for ( Coordinate c : gPts ) {
						points.add( c );
					}
					points.remove( points.size() - 1 );

					//Geometry cleanPts = GfaClip.getInstance().cleanupPoints( onePart );
					
					Geometry cleanPts = GfaClip.getInstance().cleanupPoints( 
							            GfaClip.getInstance().pointsToGeometry( points ) );

					if ( GfaClip.getInstance().isAddableAsSmear( cleanPts, snapshots, smear ) ) {

						//Replace intersection points with their pre-snapped pair.
						Geometry rplPts = GfaClip.getInstance().replacePts( cleanPts, areaInterPts );

						//Create a new GFA smear.
						cleanPts = GfaClip.getInstance().cleanupPoints( rplPts );
						
						Gfa newElm = GfaClip.getInstance().geometryToGfa( smear, cleanPts );
												
						newElm.setGfaArea( areaName );
						
						newSmears.add( newElm );
						
					}
				}
			}
			
		}
		
		for ( Gfa gg : newSmears ) {
		    boolean[] reduFlg = new boolean[ gg.getPoints().size() ];
			int ii = 0;
		    for ( Coordinate newPt : gg.getPoints() ) {
		    	reduFlg[ ii ] = false;
		    	
		    	for ( int jj = 0; jj < smear.getPoints().size(); jj++ ) {
		    		if ( GfaSnap.getInstance().isSamePoint( newPt, 
		    				             smear.getPoints().get( jj ), GfaSnap.MAP_PRECISION ) ) {
		    			reduFlg[ ii ] = smear.getReduceFlags()[ jj ];
		    			break;		    			
		    		}
		    	}
		    	ii++;
		    }
			
			gg.setReduceFlags( reduFlg ); 
		}
								
		return newSmears;
	
    }
	
    /**
	 * This routine clips a GFA polygon against common bounds of the extended FA 
	 * area bounds and finds the intersection points of the polygon with the bound.
	 * Then the intersection points are snapped individually to points outside
	 * of the polygon.  Both the intersection points and their snapped matches are 
	 * returned in map coordinate.
	 * 
	 *  @param	gfaPoly		Polygon from GFA 
	 *  
     */
	private HashMap<Coordinate, Coordinate> getAreaIntersectionPt( Geometry gfaPoly )
	{
		HashMap<Coordinate, Coordinate>	interPtsPair = new HashMap<Coordinate, Coordinate>();	

		ArrayList<Coordinate> interPts = new ArrayList<Coordinate>();		
		ArrayList<Coordinate> pts;
		ArrayList<Integer> interIndex = new ArrayList<Integer>();
		ArrayList<Integer> indx = new ArrayList<Integer>();
		
		//Reorder in clockwise - first point is repeated at the end.
		ArrayList<Coordinate> gfaPoints = new ArrayList<Coordinate>();
		for ( Coordinate c : gfaPoly.getCoordinates() ) {
			gfaPoints.add( c );
		}
		
		ArrayList<Coordinate> cwGfaPts = GfaSnap.getInstance().reorderInClockwise( gfaPoints, null );
		
		//Get all intersection point with FA region common border.
		HashMap<String, Geometry> areaCommBnds = GfaClip.getInstance().getFaAreaXCommBounds();	
		for ( Geometry  bnd : areaCommBnds.values() ) {
			pts = GfaClip.getInstance().lineIntersect( cwGfaPts.toArray( new Coordinate[ cwGfaPts.size() ]), 
					                  bnd.getCoordinates(), indx  );
			
			if ( pts.size() > 0 ) {
				interPts.addAll( pts );
				interIndex.addAll( indx );
			}
		}

		if ( interPts.size() <= 0 ) {
			return interPtsPair;
		}

		/*
		 *  Find the area common bound points inside the polygon to ensure
		 *  the new snap points will not cluster with these points.
		 */
		 ArrayList<Coordinate> checkPoints = new ArrayList<Coordinate>();
		 for ( Geometry g : areaCommBnds.values() ) {			 
			 for ( Coordinate c : g.getCoordinates() ) {
				 Geometry pp = GfaClip.getInstance().pointsToGeometry( new Coordinate[]{ c } );
				 if ( !checkPoints.contains( c ) && pp.within( gfaPoly ) ) {
					 checkPoints.add( c );
				 }
			 }
		 }

		 /*
		  *  Now insert each common intersection point into the el polygon
		  *  and snap it outside of the el polygon.
		  *  
		  *  Note: the point cannot be any point of the el polygon or any 
		  *  snap point that has been used.
		  *   
		  *  If an intersection point is not within the clustering distance of 
		  *  the point before it (Pb) or point after it (Pa), simply insert it into 
		  *  the polygon and snap it. Otherwise, do the following:
		  *  
		  *  1. Pick the closer one of Pb and Pa as Pn.  
		  *  2. Check if Pn is within the clustering distance of the common boundary
		  *     points inside the FROM line.  
		  *  3. If so, snap Pn to the closest point not within the clustering distance
		  *     and match the intersection point and Pn to the new point. 
		  *  4. If not, match the intersection point with Pn.
		  */
		 ArrayList<Coordinate> usedPoints = new ArrayList<Coordinate>();
		 usedPoints.addAll( cwGfaPts );
		 
		 ArrayList<Coordinate> ePts = new ArrayList<Coordinate>();		 
		 for ( int ii = 0; ii < interPts.size(); ii++ ) {
			 
		     Coordinate interP = interPts.get( ii );
			 Coordinate ptBefore = cwGfaPts.get( interIndex.get( ii ) );
			 Coordinate ptAfter = cwGfaPts.get( interIndex.get( ii ) + 1 );
			 
			 double qdist1 = GfaSnap.getInstance().distance( interP, ptBefore );
			 double qdist2 = GfaSnap.getInstance().distance( interP, ptAfter );

			 int addOne = -1;            	 
			 int qmatch = -1;
			 double qdist;

			 if ( qdist1 < qdist2 )  {
				 qmatch = interIndex.get( ii ); 
				 qdist  = qdist1;
			 }
			 else {
				 qmatch = interIndex.get( ii ) + 1;
				 qdist  = qdist2;
			 }

			 if ( ( qdist / PgenUtil.NM2M ) < GfaSnap.CLUSTER_DIST ) {

				 Coordinate ptMatch = cwGfaPts.get( qmatch );
				 for ( Coordinate c : checkPoints ) {
					 if ( GfaSnap.getInstance().isCluster( c, ptMatch ) ) {
						 addOne = qmatch;
						 break;
					 }
				 }
				 				 
				 if ( addOne < 0 ) {
					 interPtsPair.put( new Coordinate( interP ),
							           new Coordinate( ptMatch ) );
					 continue;  //Done - ptMatch is a snapped point.
				 }
			 }
			 
			 // Snap
			 int kk, kk2;
			 if ( addOne >= 0 ) {
			     kk = addOne;
			     kk2 = kk;

			     ePts.clear();
			     ePts.addAll( cwGfaPts );
			 }
			 else {				 
			     ePts.clear();
				 ePts.addAll( GfaSnap.getInstance().insertArray( cwGfaPts, interIndex.get( ii ) + 1, interP ) );
				 kk = interIndex.get( ii ) + 1;
				 kk2 = kk;
			 }

			 
			 ePts.remove( ePts.size() - 1 );
			 
			 Coordinate[] snapped = new Coordinate[1];
             
			 //snap....
			 int status = GfaSnap.getInstance().snapPtGFA( kk, kk2, usedPoints, checkPoints, 
						                        ePts, true, true, 3.0F, snapped );
			 
			 if ( status != 0 ) {
				 if ( addOne >= 0 ) {
					 snapped[ 0 ] = new Coordinate( ePts.get( kk ) );
				 }
				 else {
					 snapped[ 0 ] = new Coordinate( interP );
				 }
			 }
			 
			 interPtsPair.put( new Coordinate( interP ), 
			                   new Coordinate( snapped[ 0 ] ) );
			 
			 if ( addOne >= 0 ) {
				 interPtsPair.put( new Coordinate( cwGfaPts.get( addOne ) ), 
		                           new Coordinate( snapped[ 0 ] ) );				 
			 }	
			 
			 usedPoints.add( snapped[ 0 ] );
			 
		 }
		
		return interPtsPair;
	}	
	
	
	/**
	 * Divides the smear polygon into two polygons if it cannot be represented
	 * on 3 lines of 65-character text. Two rounds of bisection may be needed. 
	 * 
	 * For the first round bisection, we divide the splitting segment into two 
	 * segments at the middle ( the middle point is snapped to the closest VOR 
	 * point) and check if these two segments intersect the original polygon.  
	 * If not, the snapped middle point will be added to the bisected polygons.  
	 * 
	 * For the second round and on,  no middle points are inserted.
	 * 
	 * The smears from each successful splitting are added to the return list.
	 * 
	 * @param	smear		The GFA smear to be bisected
	 * @param	addMidPt	Flag to add a middle point (true for first round; 
	 * 													false for second round)
	 * @return	ArrayList	List of GFA smears from bisection
	 */
	private ArrayList<Gfa> bisect( Gfa smear, boolean addMidPt ) {
	    
		ArrayList<Gfa> newSmears = new ArrayList<Gfa>();
		
	    //First check if the polygon can be represented on 3 lines of text.
		boolean cbf = canBeFormatted( smear );
				
	    //Return if it can be formatted already.
		if ( cbf ) {
			newSmears.add( smear );
			return  newSmears;
		}
		
	    /*
	     *  Find the all potential splitting segments - from the one with the 
	     *  shortest length among those segments that do not intersect the polygon.
	     */		
		Coordinate[] smearPts = smear.getLinePoints();		
		TreeSet<BisectDistInfo> segments = findAllSegments( smearPts );
	    
	    /*
	     * Compute the size of the original polygon.
	     */
	    double polyArea = PgenUtil.getSphPolyArea( smearPts );

	    /*
	     * Now loop over all the possible splitting segments, from shortest
	     * to longest, to find the shortest segment which divides the original 
	     * polygon in an acceptable proportion.
	     */
		double prefPct = 100.0 - GFA_AF_BISECT_PCT;
		double prefPctMax = Math.max( prefPct, GFA_AF_BISECT_PCT );
		double prefPctMin = Math.min( prefPct, GFA_AF_BISECT_PCT );

	    ArrayList<Coordinate> tmpPoly = new  ArrayList<Coordinate>();
        double tarea;
        double pct;
        BisectDistInfo bisectSeg = null;
        
	    for ( BisectDistInfo bd : segments ) {
	    	
	        //Generate the polygon starting from indx1 and ending at indx2.
	        tmpPoly.clear();
	        for ( int ii = bd.start; ii <= bd.end; ii++ ) {
	        	tmpPoly.add( new Coordinate( smearPts[ ii ] ) );
	        }
	        
	   	    //Compute and compare it's size to the original polygon as a percentage
	        tarea = PgenUtil.getSphPolyArea( tmpPoly.toArray( new Coordinate[ tmpPoly.size() ] ) );
	        pct =  100.0F * ( tarea / polyArea );
	        

	    	//If the percentage is within the limits, we've found our splitting segment.
	    	if ( pct >= prefPctMin && pct <= prefPctMax )  {
	    	    bisectSeg = new BisectDistInfo( bd.distance, bd.start, bd.end );
	    	    break;
	    	}	        
	        
	    }
	    
	    /*
	     *  Find the correct number of points for new polygons.  For first round
	     *  division, the middle point on the splitting segment is calculated
	     *  and snapped to be used later. The splitting segment is thus divided
	     *  into two segments at the middle point. If either one of them intersects
	     *  with the original polygon,  the middle point will not be added.
	     */
	    if ( bisectSeg == null ) {
	    	newSmears.add( smear );
	    }
	    else {
	    	
	    	//First figure out if we need and could add a middle point.
    		Coordinate midPt = null;
	    	if ( addMidPt ) {
	    		midPt = new Coordinate();
	    		midPt.x = ( smearPts[ bisectSeg.start ].x + smearPts[ bisectSeg.end ].x ) / 2.0;
	    		midPt.y = ( smearPts[ bisectSeg.start ].y + smearPts[ bisectSeg.end ].y ) / 2.0;
	    		
				Coordinate snappedMidPt = GfaSnap.getInstance().snapOnePt( midPt );

	    		if ( !GfaSnap.getInstance().isCluster( snappedMidPt, smearPts[ bisectSeg.start ] ) && 
	    			 !GfaSnap.getInstance().isCluster( snappedMidPt, smearPts[ bisectSeg.end] ) ) {

	    			Coordinate[] seg1 = new Coordinate[]{ smearPts[ bisectSeg.start ], snappedMidPt };
	    			Coordinate[] seg2 = new Coordinate[]{ smearPts[ bisectSeg.end ], snappedMidPt };

	    			if ( segIntPoly( seg1,  smearPts ) && segIntPoly( seg2,  smearPts ) ) {
	    				midPt.x = snappedMidPt.x;
	    				midPt.y = snappedMidPt.y;	    					    					    				
	    			}
	    			else {
	    				addMidPt = false;	    				
	    			}
	    		}	    		
	    	}
	    	
	    	
	    	/*
	         *  First polygon - from bisectSeg.start to bisectSeg.end, plus a middle point.
	         */    
			Gfa first = smear.copy();
						
			ArrayList<Coordinate> coor1 = new  ArrayList<Coordinate>();
			ArrayList<Boolean> reduFlg1 = new ArrayList<Boolean>();
			for ( int ii = bisectSeg.start; ii <=  bisectSeg.end; ii++ ) {
				coor1.add( new Coordinate( smearPts[ ii ] ) );
				reduFlg1.add( new Boolean( smear.getReduceFlags()[ ii ] ) );
			}
			
			if ( addMidPt ) {
				Coordinate mpt1 = new Coordinate( midPt );
				coor1.add( mpt1 );
				first.addAttribute( BISECT_MIDDLE_POINT, mpt1  );
				reduFlg1.add( new Boolean( false ) );				
			}
			
			first.setPointsOnly( coor1 );
			first.setGfaTextCoordinate( first.getCentroid() );
			
			boolean[] redu1 = new boolean[ reduFlg1.size() ];
			int kk = 0;
			for ( Boolean bb : reduFlg1 ) {
				if ( bb ) {
					redu1[ kk ] = true;
				}
				else {
					redu1[ kk ] = false;
				}
				
				kk++;
			}
			first.setReduceFlags( redu1 );
	    		    		    	
	        /*
	         *  Second polygon -  from 0 to bisectSeg.start, then middle point, continue 
	         *  from  bisectSeg.end to the end.
	         */    
			Gfa second = smear.copy();
			ArrayList<Coordinate> coor2 = new  ArrayList<Coordinate>();
			ArrayList<Boolean> reduFlg2 = new ArrayList<Boolean>();
						
			for ( int ii = 0; ii <= bisectSeg.start; ii++ ) {
				coor2.add( new Coordinate( smearPts[ ii ] ) );
				reduFlg2.add( new Boolean( smear.getReduceFlags()[ ii ] ) );				
			}
			
			if ( addMidPt ) {
				Coordinate mpt2 = new Coordinate( midPt );				
				coor2.add( mpt2 );
				second.addAttribute( BISECT_MIDDLE_POINT, mpt2  );
				reduFlg2.add( new Boolean( false ) );				
			}
			
			for ( int ii = bisectSeg.end; ii < smearPts.length; ii++ ) {
				coor2.add( new Coordinate( smearPts[ ii ] ) );
				reduFlg2.add( new Boolean( smear.getReduceFlags()[ ii ] ) );				
			}
			
			second.setPointsOnly( coor2 );		
			second.setGfaTextCoordinate( second.getCentroid() );
			
			boolean[] redu2 = new boolean[ reduFlg2.size() ];
			int kj = 0;
			for ( Boolean bb : reduFlg2 ) {
				if ( bb ) {
					redu2[ kj ] = true;
				}
				else {
					redu2[ kj ] = false;
				}
				
				kj++;
			}
			second.setReduceFlags( redu2 );
	    	
			//Add new smears to the list.
	        newSmears.add( first );
	        newSmears.add( second );
	        
	    }

		return  newSmears;

	}
	
	/**
	 *  A Class to hold bisection distances for sorting
	 */
	class BisectDistInfo implements Comparable<BisectDistInfo> {

		double	distance;
		int		start;
		int		end;

		public BisectDistInfo( double dist, int index1, int index2 ) {
			this.distance = dist;
			this.start = index1;
			this.end = index2;
		}

		@Override
		public int compareTo( BisectDistInfo o ) {

			if( this == o ) {
				return 0;
			} 
			else {
				if( this.distance > o.distance ) {
				    return 1;
			    } else {
				    return -1;
			    }
			}
		}
	}


	/**
	 *  Finds all segments that does not intersects with other segments of 
	 *  the polygon and has non-clustering length. 
	 *  
	 *  The input is assumed to be in MAP coordinate.
	 *                                                                  
     *  Note: do not repeat the first point of the polygon at the end. 
	 */
	private TreeSet<BisectDistInfo> findAllSegments( Coordinate[] poly ) {
		
		TreeSet<BisectDistInfo> distInfo = new TreeSet<BisectDistInfo>();
						
	    Coordinate[] seg = new Coordinate[2];
	    boolean qualify;
	    
		for ( int ii = 0; ii < (poly.length - 2); ii++  ) {
			for ( int jj = ii + 2; jj <  (poly.length); jj ++ ) {
		        seg[0] = poly[ii];
		        seg[1] = poly[jj]; 
		            
			    qualify = polysegIntPoly( seg, poly );
			    if ( qualify ) {
				    double dist = GfaSnap.getInstance().distance( seg[0], seg[1] ) / PgenUtil.NM2M;
				    if ( dist > GfaSnap.CLUSTER_DIST ) {
					    distInfo.add( new BisectDistInfo( dist, ii, jj ) );
				    }
			    }
			}			
        }

		return distInfo;
		
	}
	

	/**
	 *  Verifies if a segment, that consists of any two points on a polygon,
	 *  intersects with any other segments on the polygon other than the 4 segments 
	 *  starting or ending from the the specified segments' two end points.. 
	 * 
	 * 	The input is assumed to be in MAP coordinate.
	 * 
	 *  Note: Do not repeat the first point at the end of the polygon.
	 *  
	 *  @return reducePtsSegIntPoly()	If the segments intersects the polygon
	 */
	private boolean polysegIntPoly( Coordinate[] seg, Coordinate[] poly ) {
		
		boolean qualify = true;
		
		//Disqualify if the segment's two end points are adjacent on the polygon.
		List<Coordinate> polyList = Arrays.asList( poly );
		int index1 = polyList.indexOf( seg[0] );
		int index2 = polyList.indexOf( seg[1] );
		int igap = Math.abs(index2 - index1 );
		
		if ( igap == 1 || igap == ( (poly.length) - 1 ) ) {
			qualify = false;
		}
		else {							
			qualify = segIntPoly( seg, poly );
		}
		
		return qualify;
	}
	
	/**
	 *  Verifies if a segment crosses a polygon - defined as it lies within
	 *  the polygon and intersects the polygon with only two points.
	 * 
	 * 	The input is assumed to be in MAP coordinate.
	 * 
	 *  Note: 1. Do not repeat the first point at the end of the polygon.
	 *  
	 *  @param	segIn			segment to be checked in MAP coordinate
	 *  @param	polyIn			polygon to be checked against in MAP coordinate
	 *  @return segIntPoly()	If the segments intersects the polygon
	 */
	private boolean segIntPoly( Coordinate[] segIn, Coordinate[] polyIn ) {
		
		boolean qualify = true;
		
		//Convert to grid coordinate.
		Coordinate[] seg = PgenUtil.latlonToGrid( segIn );
		Coordinate[] poly = PgenUtil.latlonToGrid( polyIn );
		
		//Get mid-point and form geometries.		
		Coordinate[] midp = new Coordinate[1];		
		midp[0] = new Coordinate( (seg[0].x + seg[1].x) / 2.0, (seg[0].y + seg[1].y) / 2.0 );
		Geometry point = GfaClip.getInstance().pointsToGeometry( midp );
		Geometry polygon = GfaClip.getInstance().pointsToGeometry( poly );		

		//Disqualify if the segment lies outside of the polygon
		if ( !point.within( polygon ) ) {
			qualify = false;
		}
		else {
			Geometry segment = GfaClip.getInstance().pointsToGeometry( seg );	        	

			if ( segment.intersects( polygon ) ) {
				Coordinate[] ipts = segment.intersection( polygon ).getCoordinates();
				/*
				 * Disqualify if the segment intersects other segments of the polygon - 
				 * except the segments that share either the starting or ending 
				 * point with the segment on check.
				 */
				if ( ipts.length != 2 ) {
					qualify = false;
				}
			}            
		}

		
		return qualify;
	}

	
	/**
	 *  Finds the segments that does not intersects with other segments of 
	 *  the polygon and has non-clustering length.  "2*slack+1" segments 
	 *  are checked - each starting at "index"-th vertex of the polygon & ends 
	 *  at a vertex within "index + np/2 - slack" to "index + np/2 + slack", 
	 *  where np = total number of point in the polygon. 
	 *                                                                  
     *  Note: do not repeat the first point of the polygon at the end. 
	 */
	private TreeSet<BisectDistInfo> findSegmentsBySlack( int index, int slack, Coordinate[] polyIn ) {
		
		TreeSet<BisectDistInfo> distInfo = new TreeSet<BisectDistInfo>();
		
		//Convert to grid coordinate.
		Coordinate[] poly = PgenUtil.latlonToGrid( polyIn );
				
	    /*
	     *  Find the index of ending points to form a splitting segment - 
	     *  from "index + np/2 - slack" to "index + np/2 + slack"
	     */    
	    int npts =  poly.length;
	    int mid = npts / 2;
	    int nseg = 2 * slack + 1;
	    
	    ArrayList<Integer> pointsToCheck = new ArrayList<Integer>();
	    for ( int ii = 0; ii < nseg; ii++ ) {
	    	pointsToCheck.add( ( ii - slack + index + mid + npts ) % npts );
	    }
		
	    /*
	     *  Find out segments that does not intersect other segments of the
	     *  polygon and has a length greater than the clustering distance.
	     */	    
	    Coordinate[] seg = new Coordinate[2];
	    seg[0] = poly[ index ];
	        	
	    boolean qualify;
        for ( Integer jj : pointsToCheck  ) {
		    
        	//Sanity check
        	if( jj >= poly.length ) continue;
        	
        	seg[1] = poly[jj]; 
		        
		    qualify = polysegIntPoly( seg, poly );
			if ( qualify ) {
				double dist = GfaSnap.getInstance().distance( seg[0], seg[1] ) / PgenUtil.NM2M;
				if ( dist > GfaSnap.CLUSTER_DIST ) {
					distInfo.add( new BisectDistInfo( dist, index, jj ) );
				}
			}			
        }
		
		return distInfo;
		
	}

	/*
	 * Check if two FA areas (connected by "-") belong to the same FA region.
	 */
	private boolean isSameFaRegion( String areas ) {
		
		return ( areas.equalsIgnoreCase( Gfa.BOS + "-" + Gfa.MIA ) ||
				 areas.equalsIgnoreCase( Gfa.MIA + "-" + Gfa.BOS ) || 
				 areas.equalsIgnoreCase( Gfa.SLC + "-" + Gfa.SFO ) ||
				 areas.equalsIgnoreCase( Gfa.SFO + "-" + Gfa.SLC ) || 
				 areas.equalsIgnoreCase( Gfa.CHI + "-" + Gfa.DFW ) ||
				 areas.equalsIgnoreCase( Gfa.DFW + "-" + Gfa.CHI ) );
	}
	
	/*
	 * Wrapper to do regular point reduction on a smear.
	 * 
	 * @param	smearIn		Gfa smear to be point-reduced.
	 * @return	Gfa			A Gfa to hold the new points and reduce flags.	
	 */
	private Gfa regularPointReduction( Gfa smearIn ) {
		
		ArrayList<Coordinate> xyList = new ArrayList<Coordinate>( smearIn.getPoints() );
	    List<Integer> reduceFlg = new ArrayList<Integer>(); 
	    List<Integer> orig = new ArrayList<Integer>();
		
	    boolean[] reduceF = smearIn.getReduceFlags();
			    	    
	    for ( int ii = 0; ii < reduceF.length; ii++ ) {
			
			if ( reduceF[ ii ] ) {
				reduceFlg.add( 1 );
			}
			else {
				reduceFlg.add( 0 );
			}
			
			orig.add( 1 );
		}

	    /*
	    for ( int ii = 0; ii < smearIn.getPoints().size(); ii++ ) {
			System.out.println( "Point " + ii + " (" + smearIn.getPoints().get( ii ).x + "," +
					smearIn.getPoints().get( ii ).y + ") is " + reduceFlg.get(ii) );
		}
		*/			
		double incrPct = SMEAR_INCR_PCT;
		double incrPctOrig = REDUCEPTS_INCR_PCT_ORIG;
		double incrDst = SMEAR_INCR_DST;
		
	    String prefix = getPrefixString( smearIn );

	    int ier = ReduceGfaPoints.reduceByPctDist( xyList, reduceFlg, orig, 
				                                   incrPct, incrPctOrig, incrDst, prefix );
        
		boolean[] reduceFl = new boolean[ xyList.size() ];
		for (int ii = 0; ii < xyList.size(); ii++ ) {
			if ( reduceFlg.get( ii ) == 1 )
				reduceFl[ ii ] = true;
			else
				reduceFl[ ii ] = false;
		}
		
		Gfa newSmear = smearIn.copy();
	    newSmear.setPointsOnly( xyList );
	    newSmear.setReduceFlags( reduceFl );
		
		return newSmear;

	}
	

	/*
	 *  Test - could be removed later.
	 */
	private void test() {
		Coordinate[] polyPts = new Coordinate[]{ 
				 new Coordinate(-98.50165524249245,45.216163960194365), 
				 new Coordinate(-96.07562805826798,39.725652270504796), 	
				 new Coordinate(-88.95179971518155,39.08088689505274), 	
				 new Coordinate(-87.96893561066132,44.647653935023214),
		 		 new Coordinate(-89.72876449996915,41.54448973482366)
				 };
        Coordinate[] seg = new Coordinate[2]; 
        
    	Coordinate midPt = new Coordinate();
    	midPt.x = ( polyPts[ 0 ].x + polyPts[ 2 ].x ) / 2.0 - 1.0;
    	midPt.y = ( polyPts[ 0 ].y + polyPts[ 2 ].y ) / 2.0 - 1.0;
        seg[0] = polyPts[ 0 ];
        seg[1] = midPt; 
              
        
		for ( int ii = 0; ii < (polyPts.length - 2); ii++  ) {
			for ( int jj = ii + 2; jj <  (polyPts.length); jj ++ ) {
		        seg[0] = polyPts[ii];
		        seg[1] = polyPts[jj]; 
		            
			    boolean good = polysegIntPoly( seg, polyPts );
	            String s;
			    if ( good ) {
					s =  "Qualify!";
				}
				else {
					s = "Not qualify";				
				}			    
				
			    System.out.println( "\npoint " + ii + "\t<Point Lat=\"" + 
					                    polyPts[ii].y + "\" Lon=\"" + polyPts[ii].x + "\"/>" 
					                    + "\t=>\t" + "point " + jj+ "\t<Point Lat=\"" + 
                                        polyPts[jj].y + "\" Lon=\"" + polyPts[jj].x + "\"/>"
                                        + "\t" + s );
            
			}
	    }

	}
			
}