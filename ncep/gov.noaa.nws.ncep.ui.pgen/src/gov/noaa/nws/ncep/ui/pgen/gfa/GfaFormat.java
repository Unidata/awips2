/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaFormat
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import java.io.IOException;
import java.io.File;
import java.io.BufferedWriter;
import java.io.Writer;
import java.io.FileWriter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.TreeSet;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;

//import org.apache.log4j.Logger;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

/**
 * GFA formatting functionality.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 03/11					J. Wu		Implemented 'Format by Tag"
 * 04/11					J. Wu		Implemented basic regional clipping
 * 											and area rules
 * 04/11					J. Wu		Implemented snapping/de-clustering
 * 06/11					J. Wu		Apply computational coordinate and clipped
 * 										smears before snapping.
 * 06/11					J. Wu		Fixed missing CIG type in findGfaSubTypes().
 * 07/11					J. Wu		Removed the pre-clipping before snapping.
 * 07/11		?			B. Yin		Added FRZL
 * 05/12		#808		J. Wu		Update vor text for airmets.
 * 06/12		#594		J. Wu		TTR393 - fixed invalid polygons from clipping.
 * 08/12		#610		B. Yin		Remove M_FZLVL outlooks
 * 08/12		#859		J. Wu		Fixed smearing when snapshots is too small.
 * 11/12		#911		J. Wu   	TTR 652 - Validate GFA before smearing, warn the user and
 *                                                exclude invalid GFAs from FROM. Also, prevent
 *                                                LLWS from generating Outlook.
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class GfaFormat {
//	private final static Logger logger = Logger.getLogger(GfaFormat.class);

	PgenResource drawingLayer; 
	private static GeometryFactory geometryFactory;

	public GfaFormat() {
	}
	
	public GfaFormat( PgenResource drawingLayer ) {
		this.drawingLayer = drawingLayer;
	}

	/**
	 * Format all button pressed.
	 */
	public void formatAllPressed() {
		GfaClip.getInstance().updateGfaBoundsInGrid();		
				
		if (drawingLayer != null) {
			validateAllGfas();
			for (Product p : drawingLayer.getProducts()) {
				for (Layer layer : p.getLayers()) {
					// formatting each layer separately
					formatLayer(layer, false );
				}
			}
		}		
	}

	/**
	 * Format layer button pressed.
	 */
	public void formatLayerPressed() {				

		GfaClip.getInstance().updateGfaBoundsInGrid();
		if (drawingLayer != null) {
			validateActiveGfas();
			Layer layer = drawingLayer.getActiveLayer();
			formatLayer(layer, false );
		}
	}

	/**
	 * Format tag button pressed.
	 */
	public void formatTagPressed() {				
		GfaClip.getInstance().updateGfaBoundsInGrid();
		if (drawingLayer != null) {
			validateActiveGfas();
			Layer layer = drawingLayer.getActiveLayer();
			formatLayer(layer, true );
		}
	}

	/**
	 * Formatting is done layer by layer and check the hazard/tag/desk for
	 * "Format by Tag".
	 * 
	 * @param layer
	 * @param checkTag
	 */
	private void formatLayer(Layer layer, boolean checkTag ) {

		ArrayList<AbstractDrawableComponent> oldList = new ArrayList<AbstractDrawableComponent>();
		ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>();
//		ArrayList<AbstractDrawableComponent> toRemove = new ArrayList<AbstractDrawableComponent>();
    	
		DrawableElement de = drawingLayer.getSelectedDE();
		
		for (AbstractDrawableComponent adc : layer.getDrawables()) {
			
			if (!(adc instanceof Gfa) || !((Gfa)adc).isValid() ) continue;
			    
			if ( checkTag ) { 
				if ( de != null && !isSameHazardAndTag( ((Gfa)adc), ((Gfa)de) ) ) {
					continue;
				}
			}
            
			//make sure M_FZLVL does not generate outlooks
			if ( !(  ( ((Gfa)adc).getGfaHazard().equalsIgnoreCase("M_FZLVL") ||
					   ((Gfa)adc).getGfaHazard().equalsIgnoreCase("LLWS") ) &&
					((Gfa)adc ).isSnapshot() &&
							(Gfa.getHourMinInt(((Gfa)adc).getGfaFcstHr())[0] +
							Gfa.getHourMinInt(((Gfa)adc).getGfaFcstHr())[1]/60.0) > 6 )) {
			oldList.add(adc);
			}
			
			if ( ( (Gfa)adc ).isSnapshot() 
					&& !( ( ((Gfa)adc).getGfaHazard().equalsIgnoreCase("M_FZLVL") ||
							   ((Gfa)adc).getGfaHazard().equalsIgnoreCase("LLWS") ) &&
							(Gfa.getHourMinInt(((Gfa)adc).getGfaFcstHr())[0] +
							Gfa.getHourMinInt(((Gfa)adc).getGfaFcstHr())[1]/60.0) > 6 )) {
//				oldList.add(adc);
				newList.add(adc);
			} 
//			else {
//				toRemove.add(adc);
//			}
		}

		// Generate smears for this list of snapshots
		createSmears( newList );

		//format frzl
		List<Gfa> frzl =  formatFrzl( layer, checkTag );
		if ( frzl != null && !frzl.isEmpty()){
			newList.addAll(frzl);
		}
		
//		for ( AbstractDrawableComponent adc : toRemove ) {
//			drawingLayer.removeElement( adc );
//		} 
		
		if ( !newList.isEmpty() ) {
			drawingLayer.replaceElements( oldList, newList );
		}

	}

	/**
	 * Create smears (airmets or outlooks) from the list of snapshots.
	 * 
	 * Note: New smears are attached to the input list and returned.
	 * 
	 * @param list	list of snapshots
	 * @return 
	 */
	protected void createSmears( List<AbstractDrawableComponent> list ) {
//		System.out.println("Create Smear.....call GfaClip.getInstance().validateGfaBounds");
//		GfaClip.getInstance().validateGfaBounds();
		
		/*
		 * Put snapshots into a tree set and sort them by fcstHr in ascending order.
		 */
		TreeSet<Gfa> tree = new TreeSet<Gfa>( new SnapshotComparator() );
		for (AbstractDrawableComponent adc : list) {
			if (((Gfa)adc).getGfaHazard().equalsIgnoreCase("FZLVL")) continue;
			if (((Gfa) adc).isFormat()) tree.add( (Gfa) adc );
		}

		if ( tree.isEmpty() ) return;

		/*
		 *  Build FcstHrListPair
		 *   
	     *  Each pair has a list of snapshots and a forecast hour "A-B":
	     *      A - smallest forecast hour of all snapshots in the pair 
	     *      B - largest forecast hour in the pai). 
	     *  Snapshots in each pair has the same hazard type, tag number, and desk.
		 */
		TreeSet<FcstHrListPair> splittedSS = splitSnapshots( tree );
		
		//Get the GFA international bounds
		Coordinate[] intlBnds = GfaClip.getInstance().getFaInternationalBound().getGeometryN(0).getCoordinates();
		ArrayList<Coordinate> intlBndsPts = new ArrayList<Coordinate>();
		for ( Coordinate cc : intlBnds ) {
		   intlBndsPts.add( cc );
		}
		
		/*
		 *  Generate smear from each FcstHrListPair.
		 */  
		ArrayList<ArrayList<Gfa>> listOfLists = new ArrayList<ArrayList<Gfa>>();
		for ( FcstHrListPair pair : splittedSS ) {
			
			// Find the top/bottom, FZL top/bottom, and subtypes for the pair
			HashMap<String, String> values = findGfaTopBots( pair.list );
			String type = findGfaSubTypes( pair.list );

			//Shrink wrap two snapshots with the same fcstHr
			ArrayList<Gfa> toRemove = new ArrayList<Gfa>();
			
			for ( int ii = 0; ii < pair.list.size() - 1; ii++ ) {
				
				Gfa g = pair.list.get( ii );
				Gfa gNext = pair.list.get( ii + 1 );
				
				if ( g.getGfaFcstHr().equals( gNext.getGfaFcstHr() ) ) {
					
					ArrayList<Gfa> l = new ArrayList<Gfa>();
					l.add( g );
					l.add( gNext );

					ArrayList<Coordinate> shrinkWrapped = createModifiedHull( getGeometryFactory(), l );
					
					gNext = new Gfa( PgenUtil.gridToLatlon ( shrinkWrapped ), 
							g.getGfaHazard(), g.getGfaFcstHr(), g.getGfaTag(),
							g.getGfaDesk(), g.getGfaIssueType(), g.getGfaType(),
							g.getGfaValues());
					
					pair.list.remove( ii + 1 );
					pair.list.add( ii + 1, gNext );

					toRemove.add( g );
				}
			}
			
			for ( Gfa remove : toRemove ) {
				pair.list.remove( remove );
			}

			// shrink wrap two snapshots with different fcstHr
			ArrayList<Polygon> toUnite = new ArrayList<Polygon>();
			
			for ( int ii = 0; ii < pair.list.size() - 1; ii++ ) {
				
				Gfa g = pair.list.get( ii );
				Gfa gNext = pair.list.get( ii + 1 );
				
				ArrayList<Gfa> l = new ArrayList<Gfa>();
				l.add( g );
				l.add( gNext );

				ArrayList<Coordinate> shrinkWrapped = createModifiedHull( getGeometryFactory(), l );

				Coordinate[] a = new Coordinate[ shrinkWrapped.size() + 1 ];
				shrinkWrapped.toArray( a );
				a[ a.length - 1 ] = a[ 0 ];
				LinearRing shell = getGeometryFactory().createLinearRing( a );
				Polygon poly = getGeometryFactory().createPolygon( shell, null );
				
				toUnite.add( poly );
			}

			/*
			 *  Perform union
			 *  
			 *  A JTS polygon will have its first/last point the same.
			 *  We remove the last point for snapping.
			 */
			ArrayList<Coordinate> coordinates = new ArrayList<Coordinate>();
			
			if ( toUnite.isEmpty() ) {
				coordinates = pair.list.get( 0 ).getPoints();
			} 
			else {
				Geometry union = toUnite.get( 0 );
				for ( int ii = 1; ii < toUnite.size(); ii++ ) {
					Polygon r = toUnite.get( ii );
					union = union.union( r );
				}
				
				Coordinate[] c = union.getCoordinates();
				coordinates.clear();
				coordinates.addAll( Arrays.asList( c ) );
				coordinates.remove( coordinates.size() - 1 );
				coordinates = PgenUtil.gridToLatlon ( coordinates );
			}

			/*
			 *  Clip against the international bound before snapping - this helps
			 *  to eliminate the difficulty and complexity to find snap points for
			 *  those points outside of the international boundary - snap points are
			 *  very sparse or none when moving away from the international boundary. 
			 */						
			Geometry intlBoundInGrid = GfaClip.getInstance().getFaInternationalBoundInGrid();			
			
			ArrayList<Coordinate> pointsInGrid = PgenUtil.latlonToGrid( coordinates );
			
			Geometry smearPolyInGrid = GfaClip.getInstance().pointsToGeometry( pointsInGrid );
			
			Geometry  clipAgstIntlBnd = null;
			if ( smearPolyInGrid.intersects( intlBoundInGrid ) ) {
				clipAgstIntlBnd = smearPolyInGrid.intersection( intlBoundInGrid );	
			}
			
			if ( clipAgstIntlBnd == null || clipAgstIntlBnd.getNumGeometries() <= 0 ||
					 !GfaClip.getInstance().isBiggerInGrid( clipAgstIntlBnd ) ) {
//				return;
				continue;
			}
						          
			/*
			 *  Snapping (the first point/last point are not the same)
			 *  
			 *  do not clip against the international bound before snapping	- may cause unwanted 
			 *  kinks and hard to keep the bound intact.	
			 */
			ArrayList<Coordinate> snapped = GfaSnap.getInstance().snapPolygon( coordinates );

			// Create a GFA smear
			Gfa smear = (Gfa) pair.list.get(0);
			smear = new Gfa( snapped, smear.getGfaHazard(), pair.fcstHr, smear.getGfaTag(), 
					         smear.getGfaDesk(), smear.getGfaIssueType(), type, values );
			
			if ( pair.hadCancelledRemoved ) {
				smear.setGfaIssueType( "AMD" );
			}
            
			//Clip against FA Regions (for open FZLVLs, clip against FA areas).
			ArrayList<Gfa> clippedWithRegions = GfaClip.getInstance().clipFARegions( smear, pair.original );
			
			if ( clippedWithRegions == null || clippedWithRegions.size() == 0 )  continue;
			
			
			/*
			 * Clipping may result in invalid polygons, when intersection points of the smear with 
			 * the international bounds are snapped....
			 */			
			for ( Gfa gg: clippedWithRegions ) {
				if ( !(gg.toPolygon().isValid()) ) {
					ArrayList<Coordinate> newPts = new ArrayList<Coordinate>( gg.getPoints() );
					for ( int ii = 0; ii < gg.getPoints().size(); ii++ ) {
						int jj = ii+1;
						if ( jj == gg.getPoints().size() ) jj = 0;
						
						double distance =  GfaSnap.getInstance().distance( gg.getPoints().get(ii), gg.getPoints().get(jj) );
						if ( (distance / PgenUtil.NM2M) <= GfaSnap.CLUSTER_DIST ) {
																			 
							if ( intlBndsPts.contains( gg.getPoints().get(ii) ) ) {							
								newPts.remove( gg.getPoints().get( jj ) );
							}
							else {
								newPts.remove( gg.getPoints().get( ii ) );								
							}						
						}						
					}
					
					if ( newPts.size() <  gg.getPoints().size() ) {
						gg.setPointsOnly( newPts );
					}
				}
				
			}
			
			/*
			 *  Add the new smear and its associated snapshots as attributes
			 *  to the list for further processing.
			 */			
			clippedWithRegions.get(0).addAttribute( "TEMP_SMEAR", smear );
			
			for ( Gfa g: clippedWithRegions ) {
				g.addAttribute("TEMP_SMEAR", smear); // just to pass to the next for loop
				
				if( smear.isOutlook() ) {
					g.addAttribute( "OUTLOOKS", clippedWithRegions ); // pass to GfaRules
				} 
				else {
					g.addAttribute( "AIRMETS", clippedWithRegions ); // pass to GfaRules
				}
				
				g.addAttribute( "PAIR", pair );
			}
			
			
			for( Gfa g: pair.original ) {
				if ( smear.isOutlook() ) {
					g.addAttribute( "OUTLOOKS", clippedWithRegions );// pass to GfaRules
				} 
				else {
					g.addAttribute( "AIRMETS", clippedWithRegions );// pass to GfaRules
				}
			}
			
			
			// Temp save, then apply rules after all the smears and outlooks are created 
			listOfLists.add( clippedWithRegions );
			
		}
		
		
		/*
		 *  Apply other GFA rules (area, reduce points, wording ......
		 */
		for ( ArrayList<Gfa> clippedGroup : listOfLists ) {
			
			Gfa smear = ( clippedGroup.get( 0 ).getAttribute( "TEMP_SMEAR", Gfa.class ) );
			
			clippedGroup.get( 0 ).removeAttribute( "TEMP_SMEAR" );
			
			FcstHrListPair pair = clippedGroup.get(0).getAttribute( "PAIR", FcstHrListPair.class );
			
			// Apply rules			
			GfaRules.applyRules( smear, clippedGroup, pair.original );

			// Apply worst case attributes
			GfaWorstAttr.apply( smear, clippedGroup, pair.original, pair.canceled );
			
			// Finally, attach this processed list of GFA back to the input list
			list.addAll( clippedGroup );
			
		}
				
		// Clean up all intermediate attributes from the list.
		clearAttributes( listOfLists );		

		// Set the vor text for each Airmet.
		for ( AbstractDrawableComponent gg : list ) {
			if ( gg instanceof Gfa ) {
				((Gfa)gg).setGfaVorText( Gfa.buildVorText( (Gfa)gg ) );
			}
		}
		
	}

	/**
	 *  A Class to hold a list of snapshots and a forecast hour ("A-B", A - smallest 
	 *  forecast hour of all snapshots in the pair B - largest forecast hour in 
	 *  the pair). Snapshots in each pair has the same hazard type, tag number, and desk.
	 *  
     *  Note: (1) a second list is used for future reference to hold all ORIGINAL 
     *            snapshots that are used to  create the smear.
     *        (2) a third list is used for future references to hold all REMOVED snapshots 
     *            (CANceled snapshots which are not used to generate smears, but they have
     *              the same hazard type and tag/desk as this pair).
	 * 
	 */
	class FcstHrListPair implements Comparable<FcstHrListPair>{
		boolean hadCancelledRemoved = false;
		String fcstHr;
		ArrayList<Gfa> list;
		ArrayList<Gfa> original;  //exlcuding "canceled"
		ArrayList<Gfa> canceled;

		public FcstHrListPair(String fcstHr, ArrayList<Gfa> list, ArrayList<Gfa> canceled ) {
			this.fcstHr = fcstHr;
			this.list = list;
			this.original = new ArrayList<Gfa>();
			this.original.addAll( list );
			this.canceled = new ArrayList<Gfa>();
			
			if ( canceled != null ) {
			    this.canceled.addAll( canceled );
			    if ( canceled.size() > 0 ) {
				    this.hadCancelledRemoved = true;
			    }
			}
		}

		@Override
		public int compareTo(FcstHrListPair o) {
			String[] s = fcstHr.split("-");
			int[] hm0 = Gfa.getHourMinInt(s[0]);
			int[] hm1 = Gfa.getHourMinInt(s[1]);
			boolean isOutlook = hm0[0] >= 6 && hm1[0]>6;

			if(this == o) {
				return 0;
			} else if(isOutlook){
				return 1;
			} else {
				return -1;
			}
		}
		
		public ArrayList<Gfa> getOriginal() {
			return original;
		}

	}

	/**
	 *  Build a tree set of FcstHrListPair from a sorted list of GFA snapshots.
	 *  Each pair has a list of snapshots and a forecast hour ("A-B", A - smallest 
	 *  forecast hour of all snapshots in the pair B - largest forecast hour in 
	 *  the pair). Snapshots in each pair has the same hazard type, tag number, and desk.
	 *  
     *  Note: a second list is used to hold all ORIGINAL snapshots for future references.
	 * 
	 * @param tree
	 * @return
	 */
	private TreeSet<FcstHrListPair> splitSnapshots(TreeSet<Gfa> tree) {
		TreeSet<FcstHrListPair> pairs = new TreeSet<FcstHrListPair>();
		ArrayList<Gfa> list1 = new ArrayList<Gfa>(); // 0-6 smear
		ArrayList<Gfa> list2 = new ArrayList<Gfa>(); // 6-12 outlook

		/*  
		 * First splits a tree set of GFA snapshots into two lists.
		 *  "0-?" for snapshots with forecast hour <= 6
	     *  "6-?" for snapshots with forecast hour >= 6
		 */
		for (Gfa gfa : tree) {
			int [] hm = Gfa.getHourMinInt(gfa.getGfaFcstHr());
			if (hm[0] < 6) {
				list1.add(gfa);
			} else if (hm[0] == 6) {
				list1.add(gfa);
				list2.add(gfa);
			} else {
				list2.add(gfa);
			}
		}

		// Now split list1 and list2 by hazard type, tag, and desk
		HashMap<String, ArrayList<Gfa>> map = new HashMap<String, ArrayList<Gfa>>();
		splitByHazardTag(list1, map);
		splitByHazardTag(list2, map);

		/*
		 *  Create FcstHrListPair for each list of snapshots (key is the forecast hour
		 *  of the group plus hazard type, tag number, and desk.
		 *  
		 *  Note: (1) "6-6" needs special attention
		 *        (2) canceled snapshots needs special rules. 
		 */
		ArrayList<ArrayList<Gfa>> all66 = new ArrayList<ArrayList<Gfa>>();
		for ( String key : map.keySet() ) {
			ArrayList<Gfa> l = map.get( key );
			if ( !l.isEmpty() ) {
				String fcst = determineFcst( l );
				if ("6-6".equals(fcst)) {
					// keep all "6-6", deal with them later
					all66.add( l );
				} 
				else {
					ArrayList<Gfa> canceledSS = findCancelled( l );
					FcstHrListPair pair = new FcstHrListPair( fcst, l, canceledSS );

					pairs.add( pair );
				}
			}
		}

		/*
		 *  Process special case "6-6"
		 */
		for ( ArrayList<Gfa> l66 : all66 ) {
			// should we draw this "6-6" smear? yes, if no other smears/outlooks
			// have these elements
			// let's check only first one
			Gfa gfa = l66.get(0);
			// check all other lists, if they have this gfa, then ignore
			boolean found = false;
			label: for (ArrayList<Gfa> l : map.values()) {
				if (l66 == l || isOutlook(determineFcst(l))) continue;
				for (Gfa g : l) {
					if (g == gfa) {
						found = true;
						break label;
					}
				}
			}
			
			if ( !found ) {
				pairs.add( new FcstHrListPair("6-6", l66, null ) );
				
				for ( String key: map.keySet() ){
					if ( key.endsWith( gfa.getGfaHazard() + gfa.getGfaTag() + gfa.getGfaDesk() )
							&& ( key.contains("-9") || key.contains("-12") ) ) {
						gfa.addAttribute("OTLK_LIST", map.get(key) );
					}
				}
			}
			
		}
		
		return pairs;
	}

	/**
	 * Build a map for a list of GFAs snapshots. The key is the forecast hour of the 
	 * list (("A-B", A and B are the smallest and largest forecast hours of all snapshots 
	 * in the list, respectively) plus a Gfa's hazard type, tag number, and desk. The value
	 * is a new list of snapshots with the same key.
	 *	
	 * @param list
	 * @param map
	 * @return
	 */
	private void splitByHazardTag( ArrayList<Gfa> list, HashMap<String, ArrayList<Gfa> > map ) {
		
		for ( Gfa g : list ) {
			
			String key = determineFcst( list ) + g.getGfaHazard() + g.getGfaTag() + g.getGfaDesk();
			
			if ( map.containsKey( key ) ) {
				if ( !map.get( key ).contains( g ) )  {
					map.get( key ).add( g );
				}
			} 
			else {
				ArrayList<Gfa> l = new ArrayList<Gfa>();
				l.add( g );
				map.put( key, l );
			}
			
		}
	}
    
	/** 
	 *  Make a airmet/outlook forecast hour from a list of snapshots.
	 *  Note: the snapshots are assumed to be sorted by their forecast 
	 *  hours in an ascending order.
	 *  
	 */
	private String determineFcst(ArrayList<Gfa> list1) {
		String str1 = list1.get(0).getGfaFcstHr().split(" ")[0];
		String str2 = list1.get(list1.size() - 1).getGfaFcstHr().split(" ")[0];
		return str1 + "-" + str2;
	}
	

	/** 
	 *  Clear/remove intermediate attributes from a list of GFA lists when those
	 *  attributes are no long needed. 
	 */
	private void clearAttributes(ArrayList<ArrayList<Gfa>> listOfLists) {
		for(ArrayList<Gfa> clipped: listOfLists) {
			for(Gfa g: clipped){
				g.removeAttribute("AIRMETS");
				g.removeAttribute("PAIR");
			}
		}
	}

	/** 
	 *  Check if a forecast hour qualifies as an outlook 
	 *  ("A-B",  A >= 6 and B exists and is assumed >= 6 as well).
	 */
	private boolean isOutlook(String fcst) {
		String[] s = fcst.split("-");
		if (s.length == 2 && s[1].isEmpty()) return false;
		String h = s[0].split(":")[0];
		return Integer.parseInt(h) >= 6;
	}


	/**
	 * Find canceled snapshots that should be removed, but when all of them are canceled,
	 * all of them should be kept.  
	 * 
	 * Note: (1) the snapshots that should be removed are removed from the original list.
	 *       (2) a list of removed "CAN" snapshots are returned in a new list. If all are
	 *           "CAN" snapshots and should be kept, then the returned "CAN" list is empty.
	 * 
	 * see af_useSnapshots in afutils.c
	 * 
	 * @param list
	 */
	private ArrayList<Gfa> findCancelled( ArrayList<Gfa> list ) {
		
		ArrayList<Gfa> cancelled = new ArrayList<Gfa>();
		for ( Gfa gfa : list)  {
			if ( "CAN".equalsIgnoreCase( gfa.getGfaIssueType() ) ) {
				cancelled.add( gfa );
			}
		}
		
		if (  cancelled.size() != 0 ) {
			
			if ( list.size() != cancelled.size() ) {
			    for ( Gfa gfa : cancelled ) {
				    list.remove( gfa );
			    }
			}
			else {
				cancelled.clear();
			}
		}

		return cancelled;
	}

	/*
	 * Create a modified hull for a list of Gfa polygons.
	 */
	private ArrayList<Coordinate> createModifiedHull( GeometryFactory gf, ArrayList<Gfa> list) {
		
		LinearRing[] rings = new LinearRing[ list.size() ];
		int i = 0;
		
		for (Gfa g : list) {
			
			Coordinate[] a = new Coordinate[g.getPoints().size() + 1];
			g.getPoints().toArray(a);
			a[a.length - 1] = a[0];
			
			//Convert to grid coordinates
			a = PgenUtil.latlonToGrid( a );

			LinearRing ring = gf.createLinearRing( a );
			if ( CGAlgorithms.isCCW( a ) ) {
				// counter-clockwise, need to reverse
				LineString ls = (LineString)ring.reverse();
				ring = gf.createLinearRing(ls.getCoordinates());
			}
			
			rings[ i++ ] = ring;
		}

		GeometryCollection poly = new GeometryCollection( rings, gf );
		Geometry hull = poly.convexHull();
		Coordinate[] hullCoordinates = hull.getCoordinates();

		/*
		 * If two consecutive points belong to the same gfa element, then we
		 * need to insert all the points from that gfa into the hullCoordinates
		 * (shrink wrap smear)
		 */
		ArrayList<Coordinate> hullCoordinatesList = shrinkWrapSmear( gf, rings, hullCoordinates );
		
		return hullCoordinatesList; //in grid coordinates.
		
	}
    
	
	/*
	 * Do shrink wrap.
	 * 
	 * The result will be in the same coordinate system as the input's.
	 */
	private ArrayList<Coordinate> shrinkWrapSmear( GeometryFactory gf, LinearRing[] rings,
			Coordinate[] hullCoordinates ) {
		
		int i;
		ArrayList<Coordinate> hullCoordinatesList = new ArrayList<Coordinate>();
		
		for ( i = 0; i < hullCoordinates.length - 1; i++ ) {
			Coordinate c = hullCoordinates[i];
			Coordinate cNext = (i + 1) == hullCoordinates.length - 1 ? hullCoordinates[0]
					: hullCoordinates[i + 1];

			CoordinateArraySequence cas = new CoordinateArraySequence( new Coordinate[] { c } );
			Point p = new Point( cas, gf );
			cas = new CoordinateArraySequence( new Coordinate[] { cNext } );
			Point pNext = new Point( cas, gf );

			if ( !hullCoordinatesList.contains( c ) ) hullCoordinatesList.add( c );

			for ( int j = 0; j < rings.length; j++ ) {
				LinearRing ring = rings[j];
				
				if (ring.contains(p) && ring.contains(pNext)) {
					// belong to the same gfa
					Coordinate[] ringCoordinates = ring.getCoordinates();
					List<Coordinate> rcList = Arrays.asList( ringCoordinates );
					int index = rcList.indexOf(c);
					int indexNext = rcList.indexOf(cNext);
					
					if ( Math.abs( indexNext - index ) <= 1 ) continue;
					
					indexNext = ( indexNext > index ) ? indexNext : indexNext + ringCoordinates.length;
					
					// insert intermediate points
					for ( int ii = index; ii < indexNext; ii++ ) {
						Coordinate toInsert = ringCoordinates[ ii % ringCoordinates.length ];
						if ( !hullCoordinatesList.contains( toInsert ) ) {
							hullCoordinatesList.add( toInsert );
						}
					}
				}
			}
		}
		
		return hullCoordinatesList;
	}
	

	/**
	 *  Create a default GeometryFactory.
	 *  
	 * @return geometryFactory
	 */
	public static GeometryFactory getGeometryFactory() {
		
		if ( geometryFactory == null ) {
			geometryFactory = new GeometryFactory();
		}
		
		return geometryFactory;
	}

	
	/**
	 *  Find the worst case top, bottom, FZL top, and FZL bottom from
	 *  a list of GFA snapshots
	 * 
	 * @param list		list of GFA snapshots
	 * @return
	 */
	public static HashMap<String, String> findGfaTopBots( ArrayList<Gfa> list ) {
		
		Gfa g = (Gfa)list.get( 0 );
        
		/** Need to check the logic **/
		Comparator<String> comp = new Comparator<String>() {
			public int compare( String s1, String s2 ) {
				if ( s1.equals( s2 ) ) {
					return 0;
				} else if ( "SFC".equalsIgnoreCase(s1) && "FZL".equalsIgnoreCase(s2) ) {
					return -1;
				} else if ( "FZL".equalsIgnoreCase(s1) && "SFC".equalsIgnoreCase(s2) ) {
					return 1;
				} else if ( "SFC".equalsIgnoreCase(s1) || "FZL".equalsIgnoreCase(s1) ) {
					return -1;
				} else if ( "SFC".equalsIgnoreCase(s2) || "FZL".equalsIgnoreCase(s2) ) {
					return 1;
				}
				
				return s1.compareTo( s2 );
			}
		};

		// Clone
		HashMap<String, String> values = new HashMap<String, String>();
		
		for ( String key : g.getGfaValues().keySet() ) {
			values.put(key, g.getGfaValues().get(key) );
		}

		/* 
		 * Find top/bottom for the list of GFA snapshots 
		 */
		TreeSet<String> tops = new TreeSet<String>( comp );
		TreeSet<String> bottoms = new TreeSet<String>( comp );
		for ( Gfa gfa : list ) {
			if ( gfa.getGfaTop() != null ) {
				tops.add(gfa.getGfaTop() );
			}
			
			if ( gfa.getGfaBottom() != null ) {
				bottoms.add( gfa.getGfaBottom() );
			}
		}
		
		if ( tops.size() > 0 && bottoms.size() > 0 ) {
			values.put( Gfa.TOP, tops.last() );
			values.put( Gfa.BOTTOM, bottoms.first() );
			values.put( Gfa.TOP_BOTTOM, tops.last() + "/" + bottoms.first() );
		}
		
		
		/*
		 * Find FZL Top/Bottom
		 */
		tops.clear();
		
		// leave the worst case
		while ( bottoms.size() > 0 && "FZL".equals( bottoms.first() ) ) {
			bottoms.remove( bottoms.first() );
		}
		
		for ( Gfa gfa : list ) {
			if ( gfa.getGfaValue( Gfa.FZL_TOP_BOTTOM ) != null && 
				 !gfa.getGfaValue( Gfa.FZL_TOP_BOTTOM).isEmpty() ) {
				
				String[] s = gfa.getGfaValue( Gfa.FZL_TOP_BOTTOM).split( "/" );
				
				tops.add( s[0] );
				bottoms.add( s[1] );
			}
		}
		
		if ( tops.size() > 0 ) {
			values.put( Gfa.FZL_TOP_BOTTOM, tops.last() + "/" + bottoms.first() );
		}

		return values;
	}

	
	/**
	 *  Build a sorted string from all GFA subtypes presented in
	 *  a list of GFA snapshots
	 * 
	 * @param list		list of GFA snapshots
	 * @return
	 */
	public static String findGfaSubTypes( ArrayList<Gfa> list ) {
		
		String cig = "";
		String vis = "";
		
		// A comparator for sorting
		class Comp implements Comparator<String> {
			ArrayList<String> types = new ArrayList<String>();

			public Comp() {
				String[] t = { "CLDS", "PCPN", "BR", "FG", "HZ", "FU", "BLSN" };
				for ( String s : t ) {
					types.add( s );
				}
			}

			public int compare( String s1, String s2 ) {
				if ( s1.equals( s2 ) ) return 0;
				int i1 = types.indexOf( s1 );
				int i2 = types.indexOf( s2 );
				return i1 - i2;
			}
			
		}

		// Find and sort all cig/vis types
		TreeSet<String> visTypes = new TreeSet<String>( new Comp() );
		for ( Gfa gfa : list ) {
			if ( gfa.getGfaType() != null && !gfa.getGfaType().isEmpty() ) {
		          
				String[] s = gfa.getGfaType().replace("/VIS", ":VIS").split(":");
				int ii = 0;
				if ( s[0].startsWith("CIG") ) {
					cig = s[0]; // CIG BLW 010
					ii = 1;
				}

				if ( s.length > ii && !s[ii].isEmpty() ) {
					int last = s[ii].trim().lastIndexOf(" ");
					if (last > -1) {
						// VIS BLW 3SM FG/HZ
						vis = s[ii].substring(0, last); // VIS BLW 3SM
						String[] b = s[ii].substring(last + 1).split("/"); // FG/HZ
						if ( b.length > 0 ) {
							for (String str : b)
								visTypes.add(str);
						}
					}
				}
			    							
			}			
		}							
		
		// Combine all subtypes into a string
		String ret = "";
		if ( !cig.isEmpty() ) ret = cig;
				
		if ( !vis.isEmpty() ) {
			ret += ret.isEmpty() ? (vis + " ") : (":" + vis + " ") ;
		}
		
		for ( String s : visTypes ) {
			ret += s + "/";
		}
		
		if ( ret.endsWith("/") ) ret = ret.substring( 0, ret.length() - 1 );
		
		return ret.replace( ":VIS", "/VIS" );
	}

	
	/**
	 * Comparator, smaller hour first.
	 * 
	 * @author mlaryukhin
	 */
	private class SnapshotComparator implements Comparator<Gfa> {

		@Override
		public int compare(Gfa g1, Gfa g2) {
			if (g1 == null || g2 == null || !g1.isSnapshot() || !g2.isSnapshot()) return -1;

			try {
				String f1 = g1.getGfaFcstHr().split(" ")[0];
				if (f1.contains(":")) f1 = f1.replace(":", ".");
				String f2 = g2.getGfaFcstHr().split(" ")[0];
				if (f2.contains(":")) f2 = f2.replace(":", ".");

				double d1 = Double.parseDouble(f1);
				double d2 = Double.parseDouble(f2);

				if (d1 < d2 || Math.abs(d2 - d1) <= 0.0001) // < or ==
					return -1;
				else
					return 1;
			} catch (RuntimeException e) {
//				logger.error("Comparator problem", e);
				e.printStackTrace();
				return -1;
			}
		}
	}
	
	/**
	 * Check if two Gfa elements have same hazard type and tag/desk
	 * 
	 * @param gfa1
	 * @param gfa2
	 * @return 
	 */
	private boolean isSameHazardAndTag( Gfa gfa1, Gfa gfa2 ) {
        return ( gfa1.getGfaHazard().equals( gfa2.getGfaHazard() )  &&
				 gfa1.getGfaTag().equals( gfa2.getGfaTag() )  &&	
				 gfa1.getGfaDesk().equals( gfa2.getGfaDesk() ) );
	}
		
	/**
	 * Format frzl for the input layer.
	 * @param layer - layer being formatted
	 * @return - list of formatted frzl
	 */
	private List<Gfa> formatFrzl( Layer layer, boolean checkTag ){
		
		List<Gfa> smears = new ArrayList<Gfa>();
		
		DrawableElement de = drawingLayer.getSelectedDE();
		
		for (AbstractDrawableComponent adc : layer.getDrawables()) {
		    
			if ( checkTag ) { 
		    	if ( de != null && !isSameHazardAndTag( ((Gfa)adc), ((Gfa)de) ) ) {
		    		continue;
		    	}
		    }
		    
			if ((adc instanceof Gfa) 
					&& ((Gfa)adc).getGfaHazard().equalsIgnoreCase("FZLVL")
					&& !((Gfa)adc).isSnapshot()){
				
				smears.add((Gfa)adc);
						
			}
		}
		
		if ( smears.isEmpty() ) return smears;
		else return new FrzlFormatter(smears).format();
	}
    
	/**
     * Write out an array of lat/lons to a file
     *  
     * @param
     */
    public static void writePoints ( ArrayList<Coordinate> snp, String filename ){    	
    	
    	Writer output = null;	    	
    	File file = new File( "/export/cdbsrv/jwu/" + filename );
    	try {
    	    FileWriter fw = new FileWriter( file );
	    	output = new BufferedWriter( fw );
		    
	    	for ( Coordinate cc : snp ) {
		    	String c = new String( "<Point Lat=\"" + cc.y + "\" Lon=\" " + cc.x + " \"/>\n");
	    		output.write( c ); 
		    }
	        
	    	output.close();
    	
    	}
    	catch ( IOException e ) {
    		e.printStackTrace();
    	}
    		    	
    }
	
    /*
     * Validate Gfas in all activities and warn for invalid Gfas in the resource.
     */   
    private void validateAllGfas() {
    	
		PgenResource psrc = PgenSession.getInstance().getPgenResource();
		StringBuilder msg = new StringBuilder();
		msg.append( "Warning: The following Gfas are invalid.  Please correct them or they will be " );
		msg.append( "excluded in the FROM action.\n\n" );
        
		int nn = 0;
		for ( Product prd : psrc.getProducts() ) {
			for ( Layer layer : prd.getLayers() ) {
				for ( AbstractDrawableComponent adc : layer.getDrawables() ) {
					if ( adc instanceof Gfa && !((Gfa)adc).isValid() ) {
		                nn++;
						Gfa gg = (Gfa)adc;
						msg.append( prd.getName() + "\t" + prd.getType() + "\t" + layer.getName() + "\t" + 
		                		    gg.getGfaHazard() + "," + gg.getGfaFcstHr() + "," + 
		                		    gg.getGfaTag()+ gg.getGfaDesk()  + "\n" );
					}
				}
			}
			
		}

    	if ( nn > 0 ) {    		
		    MessageDialog confirmDlg = new MessageDialog( 
        		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
        		"Invalid GFA Polygons", null, msg.toString(),
        		MessageDialog.WARNING, new String[]{"OK"}, 0 );
        
    	    confirmDlg.open();
    	}
    }
    


    /*
     * Validate Gfas in current layer and warn for invalid Gfas.
     */   
    private void validateActiveGfas() {

    	Layer curLayer = PgenSession.getInstance().getPgenResource().getActiveLayer();
    	StringBuilder msg = new StringBuilder();
    	msg.append( "Warning: The following Gfas are invalid.  Please correct them or they will be \n" );
    	msg.append( "excluded in the FROM action.\n\n" );

    	int nn = 0;
    	for ( AbstractDrawableComponent adc : curLayer.getDrawables() ) {
    		if ( adc instanceof Gfa && !((Gfa)adc).isValid() ) {
    			nn++;
    			Gfa gg = (Gfa)adc;
    			msg.append( gg.getGfaHazard() + "," + gg.getGfaFcstHr() + "," + 
    					    gg.getGfaTag()+ gg.getGfaDesk()  + "\n" );
    		}
    	}

    	if ( nn > 0 ) {    		
    		MessageDialog confirmDlg = new MessageDialog( 
    				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
    				"Invalid GFA Polygons", null, msg.toString(),
    				MessageDialog.WARNING, new String[]{"OK"}, 0 );

    		confirmDlg.open();
}
    }
}

