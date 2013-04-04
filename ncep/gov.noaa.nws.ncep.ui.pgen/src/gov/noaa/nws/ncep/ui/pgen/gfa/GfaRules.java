/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaRules
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.DVLPG_HR;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.ENDG_HR;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.ISSUE_TIME;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.SNAPSHOT_TYPE;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.UNTIL_TIME;
import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.OUTLOOK_END_TIME;
import static gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool.pad;
import static java.lang.Math.abs;
import static java.lang.Math.ceil;
import static java.lang.Math.floor;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaFormat.FcstHrListPair;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.TreeSet;

//import org.apache.log4j.Logger;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;


/**
 * GFA rules functionality.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 02/11					J. Wu		Added a state into the state list
 * 										as long as the smear touches it.
 * 02/11					J. Wu		Added coastal water and MT_OBSC
 * 										state list check. Great Lakes and
 * 										coastal water are excluded for MT_OBSC.
 * 04/11					J. Wu		Implemented area rules.
 * 04/11					J. Wu		Move MT_OBSC loading to GfaClip and 
 * 										pre-load it when PGEN is activated.
 * 07/11					J. Wu		Remove smears with empty state list.
 * 03/12	    #601		J. Wu		Fixed conditional wording for "contgByd".
 * 06/12	    TTR393		J. Wu		Adjust algorithm in "processMaybe" to speed
 *                                      up processing by 200 times.
 * 11/12	    #909/TTR650	J. Wu		Remove outlooks if "genOlk" is "NO".
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class GfaRules {


	public static final String WORDING = "WORDING";

	/** Logger */
//	private final static Logger logger = Logger.getLogger(GfaRules.class);

	/** Area limit in square nautical miles */
	public static final double AREA_LIMIT = 3000.0;

	/** Tolerance for coordinates to be treated as the same /close points. */
	private static final double PRECISION = 0.000000001;

	
	public enum SnapshotType {
		/* Snapshot intersects FROM/BOUNDED BY line. */
		X,
		/* Snapshot does not intersect FROM/BOUNDED BY line. */
		O
	};
		
	/** XPATH for states in document */
	public static String MTOBSC_XPATH = "/MT_OBSC";
	
	
	/**
	 * Applies all the rules one by one by calling the corresponding method of this class.
	 * 
	 * @param smear		Original smear before clipped - Airmet or Outlook 
	 * @param list		List of GFAs from clipping
	 * @param snapshots	List of original snapshots which were used to create the smear
	 */
	public static void applyRules( Gfa smear, ArrayList<Gfa> clipped, ArrayList<Gfa> snapshots ) {
		
		/*
		 * Pass GFA format structures through area rules to set up area/adjarea info.
		 */
	    areaRules( smear, clipped, snapshots );
      
		/*
		 * Removes outlooks which cover only "6"-hour snapshots.
		 */
		removeOutlooks( clipped, snapshots );

		/*
		 * Reduce points to desired threshold set in prefs.tbl. Check the flag 
		 * before doing the above.
		 */
		GfaReducePoint.getInstance().reducePoints( clipped, snapshots );

		/*
		 * Set wording, area, region, and state list for each format structure.
		 */
		setWording( clipped, snapshots );

	}

	/**
	 * This routine clips the GFA smears against two FA areas in a FA region (the input GFA
	 * smear is assumed to have been clipped against FA regions) and checks the size of the
	 * clipped parts in each FA area to determine the primary area and the possible adjacent 
	 * area they belong to, if the smear intersects both FA areas with an area >= 3K square
	 * nautical miles.      				
	 * 
	 * @param original		Original smear before clipped - Airmet or Outlook 
	 * @param clippedList	List of GFAs from clipping
	 * @param clippedList	List of original snapshots
	 */
	private static void areaRules( Gfa original, ArrayList<Gfa> clippedList,
			                                     ArrayList<Gfa> snapshots ) {
		
		/*
		 *  Toss out all freezing level contours.  The area rules are
		 *  applied to them within af_fzlvl2fmt().
		 */
		if ( original.getGfaHazard().equals( "FZLVL") ) {
		    return;
		}
		
		/*
		 * Clipped against FA areas to update area info
		 */
		for ( Gfa g : clippedList ) {
			findGfaAreaInGrid( g, snapshots );
		}

	}
	
	/**
	 * Removes outlooks which cover only "6"-hour snapshots.
	 * 
	 * @param original
	 * @param list
	 */
	private static void removeOutlooks(ArrayList<Gfa> clipped, ArrayList<Gfa> original) {
		
		ArrayList<Gfa> toRemove = new ArrayList<Gfa>();
		for(Gfa g: clipped) {
			if (!g.isOutlook()) continue;
			boolean intersectsWithAtLeastOneOriginal = false;
			for(Gfa o : original) {
				if("6".equals(o.getGfaFcstHr())) continue; // ignore 6 (see tests 49-50)
				// if g covers o, leave it
				Polygon gP = GfaClip.getInstance().gfaToPolygonInGrid(g);
				Polygon oP = GfaClip.getInstance().gfaToPolygonInGrid(o);
				Geometry intersection = gP.intersection(oP);
				if (intersection instanceof Polygon || intersection instanceof MultiPolygon) {
					intersectsWithAtLeastOneOriginal = true;
				}
			}
			if(!intersectsWithAtLeastOneOriginal) toRemove.add(g);
		}
		clipped.removeAll(toRemove);
	}
	
	
	/**
	 * Compares two Coordinate objects by comparing x and y coordinates. If the coordinates are
	 * close within a precision, returns true, otherwise false.
	 * 
	 * @param c1
	 * 
	 * @param c2
	 * 
	 * @return true is c1 and c2 are very close, false otherwise
	 */
	public static boolean compareCoordinates(Coordinate c1, Coordinate c2) {
		if (abs(c1.x - c2.x) < PRECISION && abs(c1.y - c2.y) < PRECISION) {
			return true;
		}
		return false;
	}

	/**
	 * This routine sets the beginning/ending wording, area, region, and state list for all VALID
	 * GFA format structures. Invalid structures are marked "delete", including those polygons
	 * having less than 3 points (closed) or 2 points (open), and those having no state list.
	 * Outlooks for LLWS, M_FZLVLS, and FZLVL are deleted as well.
	 * 
	 * @param clipped
	 * @param originalSS
	 */
	private static void setWording(ArrayList<Gfa> clipped, ArrayList<Gfa> originalSS ) {
        
		ArrayList<Gfa>	checkStates= new ArrayList<Gfa>();
		for ( Gfa smear : clipped ) {
            
			// Create state list
			updateGfaStates( smear );
			
			// If state is empty, remove this Gfa!
			String stList = smear.getGfaStates();
			if ( stList != null && stList.trim().length() > 1 ) {
				checkStates.add( smear );
			}

		}
		
		/*
		 * Now do conditional wording.
		 */
		ArrayList<Gfa> invalidOtlk = new ArrayList<Gfa>();
		for ( Gfa smear : checkStates ) {

			/* see corresponding method af_getSSAttr in the legacy code 
			 * /nawips/gempak/source/textlib/airmet/afutils.c *
			 * /
						
		    /*
		     *  Find the type of each snapshot and assign to SNAPSHOT_TYPE attribute.
		     *  
		     *  X_SNAPSHOTS - SS intersects with the clipped polygon with an area > 3K.
		     *  O_SNAPSHOTS - SS do not intersect with the clipped polygon
		     */
			assignXorO( originalSS, smear );

			/*
			 * Retrieve each snapshot's forecast time.
			 * 
			 * Round UP - X_SNAPSHOTS for development wording or O_SNAPSHOTS for ending wording
			 * Round Down - X_SNAPSHOTS for ending wording or O_SNAPSHOTS for development wording
			 */
			assignSSForecastTime( originalSS );
			
			assignIssueTime( smear );

			/*
			 * Assign "airmetTag".
			 */
            assignAirmetTag( smear );
			
			/*
			 * Do wording
			 */
			GfaWording wording = new GfaWording();

			// <FROM CONDS DVLPG> wording.
			wording.fromCondsDvlpg = fromCondsDvlpg( smear, originalSS );

			// <FROM CONDS ENDG> wording.
			wording.fromCondsEndg = fromCondsEndg( smear, originalSS );
						
			int[] olkSeq = findOtlkSeq(originalSS);
			
			// GEN_OLK wording
			wording.genOlk = otlkGenWording( originalSS, olkSeq );
			if("MAYBE".equals( wording.genOlk ) ) {
				wording.genOlk = processMAYBE( smear, originalSS );
			}

			/*
			 * Outlooks should be removed if "genOlk" is "NO".
			 */
			if ( smear.isOutlook() && !("YES".equalsIgnoreCase(wording.genOlk)) ) {
				invalidOtlk.add( smear );
			}

			// OTLK CONDS CONTG BYD wording
			// since "smear" element can be either Smear or Outlook, we need an if statement
			if (smear.isAirmet()) { // not for outlooks
				wording.condsContg = contgBydWording( originalSS, olkSeq, smear );
			}
			
			// OTLK CONDS DVLPG wording
			if ("YES".equalsIgnoreCase(wording.genOlk)) {
				wording.otlkCondsDvlpg = otlkDvlpgWording( originalSS, olkSeq );
			}
			
			// OTLK CONDS ENDG wording
			if ("YES".equalsIgnoreCase(wording.genOlk)) {
				wording.otlkCondsEndg = otlkEndgWording( originalSS, olkSeq );
			}
			smear.addAttribute( WORDING, wording );
			
//			logger.trace( smear.toString() );
//			logger.debug( "\n" + wording );
			
			// create beginning and ending 
			parseWording( smear, wording );
			
			clearAttributes( originalSS );

		}
		
		// Return valid Gfa - those with at least one state in it and outlooks with GenOlk is "YES"
		clipped.clear();
		if ( invalidOtlk.size() > 0  ) checkStates.removeAll( invalidOtlk );	    
		clipped.addAll( checkStates );
	}


	/**
	 * Generate the state list for a smear.
	 * 
	 * Note: 1. The state list is not sorted.
	 *       2. If a smear has two FA areas,  the state list is generated but not
	 *          in final order (states in primary FA area precede states in adjacent
	 *          area).  This will be done in GfaGeneate.java instead.
	 * Warning: the validity check of the state bounds should be done when pre-loading
	 *          them (if necessary), not here -  since geometry().isValid() is a quite 
	 *          expensive call.
	 * 
	 * @param smear
	 */
	private static void updateGfaStates( Gfa smear ) {
				
		HashMap<String, Geometry> states = GfaClip.getInstance().getStateBoundsInGrid();
		HashMap<String, Geometry> greatLakes = GfaClip.getInstance().getGreatLakesBoundsInGrid();
		HashMap<String, Geometry> coastalWater = GfaClip.getInstance().getCoastalWaterBoundsInGrid();
		LinkedHashMap<String, Geometry> stateMaps = new LinkedHashMap<String, Geometry>(); // keep insertion order
		stateMaps.putAll( states );

		/*
		 * Find all states that touches the smear.
		 * 
         * If this is a MT_OBSC, only those MT_OBSC states will be
         * tested and added to the state list.
         * 
         */
		List<String>  mtobscsts = GfaClip.getInstance().getMtObscStates();		
		Polygon smearP = GfaClip.getInstance().gfaToPolygonInGrid( smear );
		boolean is_MTOBSC = false;
		if ( smear.getGfaHazard().equalsIgnoreCase( "MT_OBSC" ) ) {
			is_MTOBSC = true;
		}
        		
		int nState = 0;
		for ( String key : stateMaps.keySet() ) {
			Geometry g = stateMaps.get( key) ;
			
			if ( is_MTOBSC && !mtobscsts.contains( key ) ) continue;
                      			
			if ( smearP.intersects( g ) ) {
				nState++;
				updateGfaStatesField( smear, key );				
			}
		}
		
		
		 /**
	     *  Check for the airmet over Great Lakes and coastal waters.  
	     *
	     *  If the state to which those waters belong is not already 
	     *  in the stList, then add it.
	     *
	     *  MT_OBSC hazards can never include any Great Lakes & coastal waters.
	     */
		int nWaters = 0;
		if ( !is_MTOBSC ) {

			for ( String key : greatLakes.keySet() ) {
				Geometry g = greatLakes.get( key );

				if ( !g.intersects( smearP ) ) continue;
				updateGfaStatesField( smear, key );				

			}
		    
			for ( String key : coastalWater.keySet() ) {
				Geometry g = coastalWater.get( key );

				if ( !g.intersects( smearP ) ) continue;
				
				nWaters++;
				updateGfaStatesField( smear, key );				
			}			

		}
		
				
	    /**
	     *  If the airmet covers land and water then append "AND CSTL WTRS" 
	     *  to the state list.  If only water then use just "CSTL WTRS" (no AND).
	     */
	    if ( nWaters > 0 ) {
			
	    	StringBuilder s = new StringBuilder ( nvl( smear.getGfaStates() ) );
            
	    	/*
	    	 * Note - nState should be states on land, not including states from 
	    	 * Great Lakes and Coastal Waters.
	    	 */
	        if ( nState > 0 ) {
		        s.append( " AND" );
	        }

		    s.append( " CSTL WTRS" ); 
			
		    smear.setGfaStates( s.toString() );
	    }
	    					    	    
	}
    

	/**
	 * Updates the states field.
	 * 
	 * @param gfa
	 * @param state
	 * @param polygon
	 */
	private static boolean updateGfaStatesField( Gfa gfa, String state ) {
		boolean added = false;
		String s = nvl( gfa.getGfaStates() );
		if( !s.contains( state ) ) {
			if ( !s.isEmpty() ) s += " ";
			s += state;
			gfa.setGfaStates( s );
			added = true;
		}
		
		return added;
	}
	

	/**
	 * Find the type of each snapshot and assign to SNAPSHOT_TYPE attribute.
	 * 
	 * <pre>
	 * X_SNAPSHOTS - SS intersects with the clipped polygon with an area &gt; 3K. 
	 * O_SNAPSHOTS - SS do not intersect with the clipped polygon
	 * </pre>
	 * 
	 * @param originalSS
	 * @param smear 
	 */
	private static void assignXorO(ArrayList<Gfa> originalSS, Gfa smear) {
//		Polygon smearP = GfaClip.getInstance().gfaToPolygon(smear);
		Polygon smearP = GfaClip.getInstance().gfaToPolygonInGrid( smear );
		boolean allOutlook = true;
		for(Gfa ss: originalSS){ // snapshots
			assignSnapshotType(smearP, ss);
			String fcstHr = ss.getGfaFcstHr();
			if(! ("9".equals(fcstHr) || "12".equals(fcstHr)) ) {
				allOutlook = false;
			}
		}
		
		Gfa first = originalSS.get(0);
		ArrayList<Gfa> outlooks = first.getAttribute("OUTLOOKS", ArrayList.class);
		
		// SMEAR or OUTLOKOS - list of created clipped smears
		// OTLK_LIST - original list of outlook gfa elements (6,9,12)
		if("6".equals(first.getGfaFcstHr()) && !allOutlook 
				&& outlooks != null) {
			// only for smears which have "6" hour snapshots
			ArrayList<Gfa> otkl = first.getAttribute("OTLK_LIST", ArrayList.class); 
			if (otkl == null || otkl.isEmpty()) return;
			smearP = findTheSmear(smearP, outlooks);
			//smearP = GfaClip.gfaToPolygon((Gfa)otkl.get(0).getAttribute("AIRMETS"));
			for(Gfa g: otkl) {
				if(g == originalSS.get(0)) continue;
				assignSnapshotType(smearP, g);
			}
		}
	}

	/**
	 * <pre>
	 * X snapshots - SS intersects with the clipped polygon smearP with an area &gt; 3K. 
	 * O snapshots - SS do not intersect with the clipped polygon
	 * </pre>
	 * 
	 * 
	 * @param smearP <code>Polygon</code> 
	 * @param ss
	 */
	private static void assignSnapshotType(Polygon smearP, Gfa ss) {

		Polygon ssP = GfaClip.getInstance().gfaToPolygonInGrid(ss);
		Geometry intersection = ssP.intersection(smearP);
		
		ss.addAttribute(SNAPSHOT_TYPE, SnapshotType.O);
		
		if( PgenUtil.getSphPolyAreaInGrid( intersection ) > AREA_LIMIT) {
			ss.addAttribute(SNAPSHOT_TYPE, SnapshotType.X);
		} 
		
	}
	
	private static Polygon findTheSmear(Polygon smearP, ArrayList<Gfa> outlooks) {
		for(Gfa g: outlooks) {

			Polygon p = GfaClip.getInstance().gfaToPolygonInGrid(g);
			
			Geometry intersection = p.intersection(smearP);

			double area = PgenUtil.getSphPolyAreaInGrid ( intersection );
			
			if( area > AREA_LIMIT ) {
				return p;
			}
		}
		return smearP;
	}

	/**
	 * Retrieve each snapshot's forecast time.
	 * 
	 * <pre>
	 * Round UP - X_SNAPSHOTS for development wording or O_SNAPSHOTS for ending wording. 
	 * Round Down - X_SNAPSHOTS for ending wording or O_SNAPSHOTS for development wording
	 * </pre>
	 * 
	 * @param originalSS
	 */
	private static void assignSSForecastTime(ArrayList<Gfa> originalSS) {
		for(Gfa ss: originalSS) {
			String fcstHr = ss.getGfaFcstHr();
			int[] hm = Gfa.getHourMinInt(nvl(fcstHr));
			ss.addAttribute("HOUR_INT", hm[0]);
			ss.addAttribute("MIN_INT", hm[1]);
			double hmD = hm[0] + hm[1] / 60.0;
			SnapshotType type = ss.getAttribute(SNAPSHOT_TYPE, SnapshotType.class); 
			if(type == SnapshotType.X){
				ss.addAttribute(DVLPG_HR, (int)ceil(hmD));
				ss.addAttribute(ENDG_HR, (int)floor(hmD));
			} else if (type == SnapshotType.O){
				ss.addAttribute(DVLPG_HR, (int)floor(hmD));
				ss.addAttribute(ENDG_HR, (int)ceil(hmD));
			} else {
				// sanity check - should never reach this place
				// check asssignXorO if this happens
				String err = "Snapshot type must be assigned by this time";
//				logger.error(err);
				throw new IllegalArgumentException(err);
			}
		}
	}
	
	/**
	 * Assigns issue time to the smear.
	 * 
	 * @param smear
	 */
	static void assignIssueTime(Gfa smear) {
		 /*
		 * First determine if the standard issue time should be overridden. An issue time is
		 * overridden if any of the hazards (smears) are of an issuance type that is CAN, COR, NEW,
		 * or AMD.
		 */
		boolean overrideIssueTime = !"NRML".equalsIgnoreCase(smear.getGfaIssueType());

		Calendar localTimeCal = Calendar.getInstance();

		String timeStr = AirmetCycleInfo.getIssueTime();
		Calendar issueTimeCal = Calendar.getInstance();
		int hour = Integer.parseInt(timeStr.substring(0, 2));
		int min = Integer.parseInt(timeStr.substring(2));
		issueTimeCal.set(Calendar.HOUR_OF_DAY, hour);
		issueTimeCal.set(Calendar.MINUTE, min);
		issueTimeCal.set(Calendar.SECOND, 0);

		if (overrideIssueTime) {
			// Compare the local time to the issue time if overrideIssueTm flag is TRUE.
			if (issueTimeCal.before(localTimeCal)) {
				issueTimeCal = localTimeCal;
			} else {
				// set issue time = issue time + 1 min
				issueTimeCal.add(Calendar.MINUTE, 1);
			}
		}

		smear.addAttribute(ISSUE_TIME, issueTimeCal);
		
		Calendar untilTimeCal = AirmetCycleInfo.getUntilTime();		
		smear.addAttribute(UNTIL_TIME, untilTimeCal );
				
		Calendar otlkEndTime = Calendar.getInstance();
		otlkEndTime.set(Calendar.DAY_OF_MONTH, untilTimeCal.get(Calendar.DAY_OF_MONTH));
		otlkEndTime.set(Calendar.HOUR_OF_DAY, untilTimeCal.get(Calendar.HOUR_OF_DAY) + 6 );
		otlkEndTime.set(Calendar.MINUTE, untilTimeCal.get(Calendar.MINUTE) );
		otlkEndTime.set(Calendar.SECOND, untilTimeCal.get(Calendar.SECOND) );

		smear.addAttribute( OUTLOOK_END_TIME, otlkEndTime );
		
	}
	
	/**
	 * Generates "<FROM CONDS DVLPG>" wording.
	 * 
	 * @param smear
	 * @param originalSS
	 * @return
	 */
	private static String fromCondsDvlpg(Gfa smear, ArrayList<Gfa> originalSS) {
		
		int endHour = 6;
	    /*
	     * from the legacy system (af_condsWording method in source/textlib/airmet/afconditions.c):
	     * 
	     *  Wording rules:
	     *  If the first ss in the sequence is an 'X_SNAPSHOTS' {
	     *      If the first ss time is 00, there is no wording.
	     *      If the first ss time is 03 or less, wording is "+00-+TTZ", 
	     *           where 'TT' is the ss time.
	     *      If the first ss time is 06 or less (on-time cycles only), 
	     *           wording is "+03-+TTZ", where 'TT' is the ss time.
	     *  }
	     *   
	     *  If the first ss is an 'O_SNAPSHOTS' {
	     *      Scan the sequence forward from the first ss to find the 
	     *      last 'O' before encountering an 'X' ss.
	     *      The wording is "AFT +TTZ" where 'TT' is this 'O' ss time.
	     *  }
	     */
		String wording = "";
		int xx = 0;
		int yy = 0;
		Gfa ss = originalSS.get(0);
		int dvlpgHr = ss.getAttribute(DVLPG_HR, Integer.class);
		if (ss.getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
			if (dvlpgHr == 0) {
				// no wording
			} else if (dvlpgHr <= 3) {
				wording = "CONDS DVLPG +00-+" + pad(dvlpgHr) + "Z";
				yy = dvlpgHr;
			} else if (dvlpgHr <= 6) {
				wording = "CONDS DVLPG +03-+" + pad(dvlpgHr) + "Z";
				xx = 3;
				yy = dvlpgHr;
			}
		} else {
			int lastO = 0;
			for (int i = 1; i < originalSS.size(); i++) {
				Gfa g = originalSS.get(i);
				dvlpgHr = ss.getAttribute(DVLPG_HR, Integer.class);
				if (g.getAttribute(SNAPSHOT_TYPE) == SnapshotType.X && dvlpgHr <= endHour) {
					lastO = i - 1;
					break;
				}
			}
			dvlpgHr = originalSS.get(lastO).getAttribute(DVLPG_HR, Integer.class);
			wording = "CONDS DVLPG AFT +" + pad(dvlpgHr) + "Z";
			xx = dvlpgHr;
			yy = -1;
		}
		
		if(!PgenCycleTool.isRoutine() && !wording.isEmpty() && xx >= 0 && yy >= 0) {
			
			Calendar cal = smear.getAttribute(ISSUE_TIME, Calendar.class);
			int issueHrMin = cal.get(Calendar.HOUR_OF_DAY) * 100 + cal.get(Calendar.MINUTE);

			int basetime = PgenCycleTool.getCycleHour();

			if ((yy + basetime) * 100 <= issueHrMin) { /* yy in past */
				wording = "";
			} else { /* yy in future */
				if ((xx + basetime) * 100 <= issueHrMin) {/* xx in past */
					wording = "CONDS DVLPG BY +" + pad(yy) + "Z"; // BY +23Z
				} else {
					wording = "CONDS DVLPG +" + pad(xx) + "-+" + pad(yy) + "Z"; // +03-+06Z
				}
			}
		}
		
		return wording;
	}
	
	/**
	 * Generates "<FROM CONDS ENDG>" wording. 
	 * 
	 * @param smear
	 * @param originalSS
	 * @return
	 */
	private static String fromCondsEndg(Gfa smear, ArrayList<Gfa> originalSS) {
		
		int endHour = 6;
	    /*
	     * from the legacy system (af_fromEndgWording)
	     * 
	     * Wording Rules:
	     * 
	     * If last ss in the sequence is an 'X_SNAPSHOTS' {
	     *     If the last ss time is the last possible time (06 for on-time cycles 
	     *           or 03 for off-time cycles), wording is determined by the outlook
	     *           component snapshots rules. See the 'Outlook BOUNDED BY lines' 
	     *           section below for "CONDS CONTG BYD +06Z" wording 
	     *           (or "CONDS CONTG BYD +03Z" for cycles 00, 06, 12 and 18).
	     *     If the last ss time is less than 03, wording is "+TT-+03Z", 
	     *            where 'TT' is the ss time.
	     *     If the last ss time is less than 06 (on-time cycles only), 
	     *             wording is "+TT-+06Z", where 'TT' is the ss time.
	     * }
	     * 
	     * If last ss is an 'O_SNAPSHOT' {
	     *     Scan the sequence backward from the last ss to find the first 'O' 
	     *     after encountering an 'X' ss. The wording is "BY +TTZ" where 'TT' 
	     *     is this 'O' ss time.
	     * }
	     * 
	     */
		String wording = "";
		int xx = -1;
		int yy = -1;
		int lastSSIndex = originalSS.size() - 1;
		for (int i = originalSS.size() - 1; i >= 0; i--) {
			lastSSIndex = i;
			int hr = originalSS.get(lastSSIndex).getAttribute(ENDG_HR, Integer.class);
			if(hr <= endHour) break;
		}
		Gfa lastSS = originalSS.get(lastSSIndex);
		int lastEndgHr = lastSS.getAttribute(ENDG_HR, Integer.class);
		String lastFcstHr = lastSS.getGfaFcstHr();
		int[] hm = Gfa.getHourMinInt(lastFcstHr);

		if (lastSS.getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
			
			if(hm[0] == endHour && hm[1] == 0) {
				/* Handled separately by af_contgBydWording() */
			} else if(lastEndgHr < 3){
				// sprintf(wording, "CONDS ENDG +0%d-+03Z", last_ss_hr)
				wording = "CONDS ENDG +" + pad(lastEndgHr) + "-+03Z";
				xx = lastEndgHr;
				yy=3;
			} else if (lastEndgHr <= 6) {
				// sprintf(wording, "CONDS ENDG +0%d-+06Z", last_ss_hr);
				wording = "CONDS ENDG +" + pad(lastEndgHr) + "-+06Z";
				xx = lastEndgHr;
				yy = 6;
			}
		} else {
			int firstO = lastSSIndex;
			
			ArrayList<Gfa> otlkList = lastSS.getAttribute("OTLK_LIST", ArrayList.class);
			if("6".equals(originalSS.get(originalSS.size()-1).getGfaFcstHr()) && otlkList != null) {
				lastSSIndex = otlkList.size()-1;
				lastSS = otlkList.get(lastSSIndex);
				for (int jj = otlkList.size() - 2; jj >= 0; jj--) {
					lastSS = otlkList.get(jj);
					if(lastSS.getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
						firstO = jj + 1;
						break;
					}
				}
				lastEndgHr = Integer.parseInt(otlkList.get(firstO).getGfaFcstHr());
			} else {
				for (int jj = lastSSIndex-1; jj >= 0; jj--) {
					lastSS = originalSS.get(jj);
					if(lastSS.getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
						firstO = jj + 1;
						break;
					}
				}
				lastEndgHr = originalSS.get(firstO).getAttribute(ENDG_HR, Integer.class);
			}
			wording = "CONDS ENDG BY +" + pad(lastEndgHr) + "Z";
		}
		
		
		if (!PgenCycleTool.isRoutine() && !wording.isEmpty() && xx >= 0 && yy >= 0) {

			Calendar cal = smear.getAttribute(ISSUE_TIME, Calendar.class);
			int issueHrMin = cal.get(Calendar.HOUR_OF_DAY) * 100 + cal.get(Calendar.MINUTE);

			int basetime = PgenCycleTool.getCycleHour();

			if ((yy + basetime) * 100 <= issueHrMin) { /* yy in past */
				wording = "CONDS HV ENDED";
			} else { /* yy in future */
				if ((xx + basetime) * 100 <= issueHrMin) {/* xx in past */
					wording = "CONDS ENDG BY +" + pad(yy) + "Z"; // BY +23Z
				} else {
					wording = "CONDS ENDG +" + pad(xx) + "-+" + pad(yy) + "Z"; // +03-+06Z
				}
			}
		}
		
		return wording;
	}

	private static int[] findOtlkSeq(ArrayList<Gfa> originalSS) {
		int startHour = 6; // ontime 
		
		int[] olkSeq = new int[3];
		for(int i=0; i<3; i++) {
			olkSeq[i] = -1;
		}
		
		for(int ii=0; ii< originalSS.size(); ii++) {
			String fcstHr = originalSS.get(ii).getGfaFcstHr();
			int[] hm = Gfa.getHourMinInt(nvl(fcstHr));
			if (hm[1] != 0) continue;

			if (hm[0] == startHour) {
				olkSeq[0] = ii;
			} else if (hm[0] == (startHour + 3)) {
				olkSeq[1] = ii;
			} else if (hm[0] == (startHour + 6)) {
				olkSeq[2] = ii;
			}
		}
		return olkSeq;
	}

	/**
	 * Generates <genOlk> wording
	 * 
	 * @param smear
	 * @param originalSS
	 * @return
	 */
	private static String otlkGenWording(ArrayList<Gfa> originalSS, int[] olkSeq) {
//		  af_otlkGenWording(ss_attr, olk_seq, tmpstr);
		
	    /*
	     *  Wording Rules:
	     *  Assume there is at least one SS in the sequence. 
	     *  1SS - 06 ss (03 for off-time cycles)
	     *  2SS - 09 ss (06 for off-time cycles)
	     *  3SS - 12 ss (09 for off-time cycles)
	     *   
	     *  If 1SS does not exist or exists as an O_SNAPSHOTS {
	     *      <GEN OTLK> is "YES"
	     *  }
	     *   
	     *  If both 1SS and 2SS exist as X_SNAPSHOTSs {
	     *      <GEN OTLK> is "MAYBE"
	     *  }
	     *
	     *  If 1SS & 3SS exist X_SNAPSHOTSs {
	     *      <GEN OTLK> is "MAYBE"  (X-O-X case and X---X case)
	     *  }
	     *   
	     *  All other cases - <GEN OTLK> is "NO" 
	     */        
		
		String genOlk = "NO";
		if (olkSeq[0] >= 0 || olkSeq[1] >= 0 || olkSeq[2] >= 0) {
			if (olkSeq[0] < 0
					|| (olkSeq[0] >= 0 && originalSS.get(olkSeq[0]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.O)) {
				genOlk = "YES";
			} else if ((olkSeq[1] >= 0 && originalSS.get(olkSeq[1]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) || 
					(olkSeq[2] >= 0 && originalSS.get(olkSeq[2]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X)) {
				genOlk = "MAYBE";
			}
		}
		
		return genOlk;
	}

	private static String processMAYBE(Gfa outlook, ArrayList<Gfa> originalSS) {
		// corresponds to afconditions.af_processMAYBE
		
		Polygon outlookP = GfaClip.getInstance().gfaToPolygon(outlook);
		HashMap<String, Geometry> states = GfaClip.getInstance().getStateBounds();

		Gfa g6 = null; 
		for(Gfa g: originalSS) {
			updateGfaStates(g);
			if("6".equals(g.getGfaFcstHr())) g6 = g;
		}
		
		// smears list is not null here, because MAYBE can only be called 
		// for outlooks containing "6" and "9" hour snapshots.
		// and at least one "6-6" airmet exists, g6 is not be null 
		ArrayList<Gfa> airmets = g6.getAttribute("AIRMETS", ArrayList.class); // from 0 to 6 hours
		
		// loop through airmets and find the one which overlaps with the outlook we process
		// then the overlapping one is the one we will use to apply rules.
		TreeSet<String> statesInAirmet = new TreeSet<String>();
		Gfa airmet = null;
		for(Gfa g: airmets){
			Polygon gP = GfaClip.getInstance().gfaToPolygon(g);
			Geometry intersection = outlookP.intersection(gP);
			if (intersection instanceof Polygon || intersection instanceof MultiPolygon) {
				// states
				String [] statesArray = nvl(g.getGfaStates()).split(" ");
				for(String s: statesArray) {
					statesInAirmet.add(s.trim());
				}
				airmet = g;
				break;
			}
		}
		
	    /*
	     *  Rule 1: For states in the outlook's state list but not listed in the
	     *  airmet's state list, check the sizes of the intersection areas
	     *  between the outlook with those states.  If there is at least
	     *  one of them is greater than or equal to 3K, set the flag to True 
	     *  to issue the outlook.  
	     *  
	     *  Rule 2: For states both in the outlook's state list and the airmet's state
	     *  list, check the in-state outlook generation ratio with the threshold
	     *  set in the prefs.tbl.  If it's greater than the threshold, issue the
	     *  outlook. 
	     *     
	     *  Both checks should be done for the state bound,  the Great Lakes
	     *  bound, and the coastal water bound.
	     */
		String[] statesInOutlook = nvl(outlook.getGfaStates()).split(" ");
		double gfaOtlkgenRatio = GfaInfo.getGfaOtlkgenRatio();
		Polygon airmetP = GfaClip.getInstance().gfaToPolygon(airmet);
		for(String stateStr: statesInOutlook) {
			if(stateStr.isEmpty()) continue;
			Geometry stateP = states.get(stateStr);
			
			if(stateP == null) continue;

			if(statesInAirmet.contains(stateStr)) {
				// rule 2
				// 
				// the formula from tt 8.106 takes precedence over the earlier one 
				// 
				// ratio = (Outlook Area - Intersect Area)/ State Area
				// 
				//  June, 2012 - adjust algorithm to find the ratio due to high-resolution state bounds.
				//
				//  Prevoius - Intersection Area = (A^S) ^ (O^S)
				//  Now      - Intersection Area = (A^O)^S
				//
				// 
				Geometry a = airmetP.intersection( outlookP );
				if ( a == null ) continue;
				
				Geometry o = outlookP.intersection(stateP);
				
				Geometry i = a.intersection( stateP );
				if ( i == null ) continue;
				
				double oArea = PgenUtil.getSphPolyArea(o);
				double iArea = PgenUtil.getSphPolyArea(i);
				
				double ratio = 0.0;
				if ( (oArea - iArea) > 0.0 ) {
				double sArea = PgenUtil.getSphPolyArea(stateP);
				     ratio = (oArea - iArea)/sArea;
				}
				
				if (ratio >= gfaOtlkgenRatio) return "YES";
				
			} else {
				// rule 1
				Geometry intersection = outlookP.intersection(stateP);
				double area = PgenUtil.getSphPolyArea(intersection );
				if(area > AREA_LIMIT) {
					// stop here, yes we need to issue the outlook  
					return "YES";
				}
			}
		}
		
		return "NO";
	}
	
	/**
	 * OTLK CONDS CONTG BYD wording
	 * 
	 * @param origSS
	 * @param olkSeq
	 * @param smr
	 * @return
	 */
	private static String contgBydWording(ArrayList<Gfa> origSS, int[] olkSeqIn, Gfa smr ) {
	    
	    /*
	     * Wording Rules:
	     *
	     *  Proceed only if the 06 ss exists ( 03 ss for off-time cycles ) 
	     *  as an "X_SNAPSHOTS". 
	     *
	     *  If the last ss in the sequence is an 'X' {
	     *      If its time is 12, wording - "CONDS CONTG BYD +0NZ THRU +12Z".
	     *      If its time is 09, wording - "CONDS CONTG BYD +0NZ ENDG +09-+12Z".
	     *      If its time is 06, wording - "CONDS CONTG BYD +0NZ ENDG +06-+09Z".
	     *      If its time is 03, wording - "CONDS CONTG BYD +0NZ ENDG +03-+06Z".
	     *  }
	     *
	     *  If the last ss in the sequence is an 'O' {
	     *	    Scan the sequence backward from the last ss to find the first 'O' 
	     *      after encountering an 'X' ss. 
	     *      
	     *      If this 'O' ss time is 12, wording - "CONDS CONTG BYD +0NZ ENDG BY +12Z".
	     *      If this 'O' ss time is 09, wording - "CONDS CONTG BYD +0NZ ENDG BY +09Z".
	     *  }    
	     *      
	     *  Where "N" is "6" for on-time cycle and "3" for off-time cycle. 
	     *     
	     */
		
	    // Proceed only if the 06 ss exists as an "X_SNAPSHOTS"
		boolean ss06Exists = false;
		for (Gfa g : origSS) {
			int[] hm = Gfa.getHourMinInt(g.getGfaFcstHr());
			if (hm[0] == 6 && hm[1] == 0 && g.getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
				ss06Exists = true;
				break;
			}
		}
		if (!ss06Exists) return "";
        
		// Make a copy, since we need to adjust it here.
		int[] olkSeq = new int[ olkSeqIn.length ];
		for ( int ii = 0; ii < olkSeqIn.length; ii++ ) {
			olkSeq[ ii ] = olkSeqIn[ ii ];
		}

		String wording = "";
		int lastSS;
		if (olkSeq[2] >= 0)
			lastSS = 2;
		else if (olkSeq[1] >= 0)
			lastSS = 1;
		else
			lastSS = 0; 
        
		ArrayList<Gfa> originalSS = new ArrayList<Gfa>(); 
		originalSS.addAll( origSS );
		
		boolean isX = originalSS.get(olkSeq[lastSS]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X;
		ArrayList<Gfa> otlkList = originalSS.get(olkSeq[lastSS]).getAttribute("OTLK_LIST", ArrayList.class);

		// now test if OTLK_LIST list has 
		if(lastSS == 0 && otlkList != null) {
			for(Gfa g: otlkList) {
				if(g.getAttribute(SNAPSHOT_TYPE) == SnapshotType.O && !"6".equals(g.getGfaFcstHr())) {
					isX = false;
					break;
				}
			}
		}
        
        /*
         *  Find the types of outlook snapshots embedded in the airmet and adjust the 
         *  type of outlook snapshots
         */		
		boolean adjEmbedOtlk = false;
		ArrayList<Gfa> embedOtlks = originalSS.get(olkSeq[lastSS]).getAttribute( "OUTLOOKS", ArrayList.class);		
		if ( embedOtlks != null && embedOtlks.size() > 0 ) {
			
			/*
			 * Find the outlook originated from the same snapshots and clipped to
			 * the same region
			 */
			Gfa otlkInSameRegion = null;
			for ( Gfa gg : embedOtlks ) {
				if ( gg.getAttribute( "FA_REGION").equals( smr.getAttribute( "FA_REGION") ) ) {
					otlkInSameRegion = gg;
					break;
				}
			}
			
			//Adjust the type
			FcstHrListPair p = embedOtlks.get(0).getAttribute( "PAIR", FcstHrListPair.class );
			for ( Gfa gg : p.getOriginal() ) {
				if ( !originalSS.contains( gg ) ) {
					Gfa  temp = gg.copy();
					temp.addAttribute( SNAPSHOT_TYPE, SnapshotType.O );
					
					if ( otlkInSameRegion != null ) {
					    Polygon otlkPoly = GfaClip.getInstance().gfaToPolygonInGrid( otlkInSameRegion );
					    assignSnapshotType( otlkPoly, temp );
					}

					originalSS.add( temp );
				}
			}
						
			adjEmbedOtlk = true;
		}
		
		if ( adjEmbedOtlk ) {
			olkSeq = findOtlkSeq( originalSS );
			if (olkSeq[2] >= 0)
				lastSS = 2;
			else if (olkSeq[1] >= 0)
				lastSS = 1;
			else
				lastSS = 0; 
			
			isX = originalSS.get( olkSeq[lastSS] ).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X;
		}
 		
		/*
		 * wording....
		 */		
		if ( isX ) {

			String hourMinStr = originalSS.get(olkSeq[lastSS]).getGfaFcstHr();
			int [] hm = Gfa.getHourMinInt(hourMinStr);
			String lastXinOtlk= "";
			if(otlkList != null && !otlkList.isEmpty() 
					&& otlkList.get(otlkList.size() -1).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
				lastXinOtlk = otlkList.get(otlkList.size()-1).getGfaFcstHr();
			}
			
			if(hm[0] == 12 || "12".equals(lastXinOtlk)) {
				wording = "CONDS CONTG BYD +06Z THRU +12Z";
			} else if (hm[0] == 9 || "9".equals(lastXinOtlk)) {
				wording = "CONDS CONTG BYD +06Z ENDG +09-+12Z";
			} else if (hm[0] == 6 ) {
				wording = "CONDS CONTG BYD +06Z ENDG +06-+09Z";
			}
		} 
		else {
			int firstO = lastSS;
			int firstX = -1;

			for (int ii = lastSS; ii >= 0; ii--) {
				if (olkSeq[ii] >= 0
						&& originalSS.get(olkSeq[ii]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
					firstX = ii;
					break;
				}
			}

			if (firstX >= 0) {
				for (int ii = firstX + 1; ii <= lastSS; ii++) {
					if (olkSeq[ii] >= 0
							&& originalSS.get(olkSeq[ii]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.O) {
						firstO = ii;
						break;
					}
				}
			}

			String hourMinStr = originalSS.get(olkSeq[firstO]).getGfaFcstHr();
			int[] hm = Gfa.getHourMinInt(hourMinStr);
			String firstOinOtlk= "";
			if(otlkList != null && !otlkList.isEmpty()) {
				for(Gfa el: otlkList) {
					if(el.getAttribute(SNAPSHOT_TYPE) == SnapshotType.O) {
						firstOinOtlk = el.getGfaFcstHr();
						break;
					}
				}
			}

			if (hm[0] == 12 || "12".equals(firstOinOtlk)) {
				wording = "CONDS CONTG BYD +06Z ENDG BY +12Z";
			} else if (hm[0] == 9 || "9".equals(firstOinOtlk)) {
				wording = "CONDS CONTG BYD +06Z ENDG BY +09Z";
			} else if (hm[0] == 6) {
				wording = "CONDS CONTG BYD +03Z ENDG BY +06Z";
			}
		}

		return wording;
	}
	
	/** 
	 * OTLK CONDS DVLPG wording
	 * 
	 * @param smear
	 * @param originalSS
	 * @param olkSeq
	 * @return
	 */
	private static String otlkDvlpgWording(ArrayList<Gfa> originalSS, int[] olkSeq) {
		// afconditions.af_otlkDvlpgWording
		
	    /*
	     * Wording Rules:
	     * 
	     * If the first ss in the sequence is an 'X' {
	     *     If first ss time is 06, there is no wording.
	     *     If first ss time is 09, wording is "+06-+09Z".
	     *     If first ss time is 12, wording is "+09-+12Z".
	     * }
	     * If the first ss is an 'O' {
	     *     Scan the sequence forward from the first ss to find the last 'O' 
	     *     before encountering an 'X' ss. The wording is "AFT +TTZ" where 'TT' 
	     *     is this 'O' ss time.
	     * } 
	     *
	     */   
		int firstSS = -1;
		for (int ii = 0; ii < 3; ii++) {
			if (olkSeq[ii] >= 0) {
				firstSS = ii;
				break;
			}
		}
		
		String hourMinStr = originalSS.get(olkSeq[firstSS]).getGfaFcstHr();
		int[] hm = Gfa.getHourMinInt(hourMinStr);
		
		String wording = "";

		if (originalSS.get(olkSeq[firstSS]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {

			if (firstSS == olkSeq[0]) {
				/* No wording */
			} else if (hm[0] == 6) {
				wording = "CONDS DVLPG +03-+06Z";
			} else if (hm[0] == 9) {
				wording = "CONDS DVLPG +06-+09Z";
			} else if (hm[0] == 12) {
				wording = "CONDS DVLPG +09-+12Z";
			}
		} else {
			int lastO = firstSS;
			for (int ii = 0; ii < 2; ii++) {
				if (lastO == olkSeq[ii]
						&& olkSeq[ii + 1] >= 0
						&& originalSS.get(olkSeq[ii + 1]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.O) {
					lastO = olkSeq[ii + 1];
				}
			}
			hourMinStr = originalSS.get(olkSeq[lastO]).getGfaFcstHr();
			hm = Gfa.getHourMinInt(hourMinStr);

			wording = "CONDS DVLPG AFT +" + pad(hm[0]) + "Z";
		}
		return wording;
	}
	
	/**
	 * OTLK CONDS ENDG wording
	 * @param originalSS
	 * @param olkSeq
	 * @return
	 */
	private static String otlkEndgWording(ArrayList<Gfa> originalSS, int[] olkSeq) {
		// afconditions.af_otlkEndgWording
		
	    /*
	     *  Wording Rules:
	     *  
	     *  If last ss in the sequence is an 'X' {
	     *      If its time is 12 (09 for off-time), wording is "CONTG THRU +12Z" 
	     *           ("CONTG THRU +09Z" for off-time cycle).
	     *      If its time is 09, wording is "ENDG +09-+12Z" for on-time cycle and 
	     *           "CONTG THRU +09Z" for off-time cycle.
	     *      If its time is 06, wording is "ENDG +03-+06Z" for off-time cycle.
	     *  }
	     * 
	     *  If last ss is an 'O' {
	     *      Scan the sequence backward from the last ss to find the first 'O' 
	     *      after encountering an 'X' ss. The wording is "ENDG BY +TTZ" 
	     *      where 'TT' is this ss time.
	     *      
	     *      If this time is 12 (09 for off-time cycle).
	     *      If this time is 09 (06 for off-time cycle), no wording.
	     *  }
	     *
	     */
		int lastSS;
		if (olkSeq[2] >= 0)
			lastSS = 2;
		else if (olkSeq[1] >= 0)
			lastSS = 1;
		else
			lastSS = 0;

		String hourMinStr = originalSS.get(olkSeq[lastSS]).getGfaFcstHr();
		int[] hm = Gfa.getHourMinInt(hourMinStr);

		String wording = "";

		if (originalSS.get(olkSeq[lastSS]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {

			if (hm[0] == 12) {
				wording = "CONDS CONTG THRU +12Z";
			} else if (hm[0] == 9) {
				wording = "CONDS ENDG +09-+12Z";
			} else if (hm[0] == 6) {
				// no wording for on-time
			}
		} else {
			int firstO = lastSS;
			int firstX = -1;

			for (int ii = lastSS; ii >= 0; ii--) {
				if (olkSeq[ii] >= 0
						&& originalSS.get(olkSeq[ii]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.X) {
					firstX = ii;
					break;
				}
			}

			if (firstX >= 0) {
				for (int ii = firstX + 1; ii <= lastSS; ii++) {
					if (olkSeq[ii] >= 0
							&& originalSS.get(olkSeq[ii]).getAttribute(SNAPSHOT_TYPE) == SnapshotType.O) {
						firstO = ii;
						break;
					}
				}
			}

			hourMinStr = originalSS.get(olkSeq[firstO]).getGfaFcstHr();
			hm = Gfa.getHourMinInt(hourMinStr);

			if (hm[0] == 12) {
				wording = "CONDS ENDG BY +12Z";
			} else if (hm[0] == 9) {
				// no wording for on-time
			}
		}

		return wording;
	}
	
	/**
	 * This routine gets the starting and ending wordings from the format string.
	 * 
	 * @param smear
	 * @param wording
	 */
	private static void parseWording(Gfa smear, GfaWording wording) {
		
		String beginWording = "";
		String endWording = "";
		// For smear wording
		if (smear.isAirmet()) {
			//Get developing wording
			
			beginWording = wording.fromCondsDvlpg;
			
			if(!wording.condsContg.isEmpty()) {
				if(beginWording.isEmpty()) {
					beginWording += wording.condsContg;
				} else {
					beginWording += ". " + wording.condsContg;
				}
			}
			endWording = wording.fromCondsEndg;

		} else { //For outlook wording
			
			beginWording = wording.otlkCondsDvlpg;
			
			if(!wording.condsContg.isEmpty()){
				if(beginWording.isEmpty()) {
					beginWording += wording.condsContg;
				} else {
					beginWording += ". " + wording.condsContg;
				}
			}
			
			if(!wording.otlkCondsEndg.isEmpty()) {
				endWording = wording.otlkCondsEndg; 
			}
		}
		
		smear.setGfaBeginning(beginWording);
		smear.setGfaEnding(endWording);

		replacePlusWithCycle(smear);
	}

	/**
	 * Replaces all occurrences of substrings like +ii in beginning and ending words with the
	 * strings calculated as a sum of ii and a cycle. For example, "CONDS ENDG +09-+12Z" becomes
	 * "CONDS ENDG 05-08Z" if the gfa cycle is 20.
	 * 
	 * @param gfa
	 */
	public static void replacePlusWithCycle(Gfa gfa) {
		int cycle = gfa.getGfaCycleHour();

		String b = gfa.getGfaBeginning();
		b = replacePlusWithCycle(b, cycle);
		gfa.setGfaBeginning(b);

		String e = gfa.getGfaEnding();
		e = replacePlusWithCycle(e, cycle);
		gfa.setGfaEnding(e);
	}

	/**
	 * Replaces all occurrences of substrings like +ii with the string calculated as a sum of ii +
	 * cycle. For example, CONDS ENDG +09-+12Z becomes CONDS ENDG 05-08Z if cycle is 20.
	 * 
	 * @param b
	 * @param cycle
	 * @return
	 */
	public static String replacePlusWithCycle(String b, int cycle) {
		int i;
		while ((i = b.indexOf("+")) > -1) {
			String toReplace = b.substring(i, i + 3);
			String hour = toReplace.substring(1);
			int h = Integer.parseInt(hour);
			hour = pad((h + cycle) % 24);
			b = b.replace(toReplace, hour);
		}
		return b;
	}

	/**
	 * Deletes all the parameters added in this class to avoid confusion in the future.
	 * 
	 * @param originalSS
	 */
	private static void clearAttributes(ArrayList<Gfa> originalSS) {
		for (Gfa ss : originalSS) {
			ss.removeAttribute(SNAPSHOT_TYPE);
			ss.removeAttribute(DVLPG_HR);
			ss.removeAttribute(ENDG_HR);
		}
	}
	
	private static String nvl(String value) {
		return value == null ? "" : value;
	}
	
		
	/**
	 * This routine clips ONE GFA smear against two FA areas in a FA region (the input GFA
	 * smear is assumed to have been clipped against FA regions) and checks the size of the
	 * clipped parts in each FA area to determine the primary area and the possible adjacent 
	 * area they belong to. Additional Airmet/Outlook may need to be created when generating
	 * text product for such an smear (see GfaGenerate.java)	                				
	 * 
	 * @param current			GFA to be processed
	 * @param clippedlist		List of GFA snapshots from clipping
	 */
	private static void findGfaAreaInGrid( Gfa current, ArrayList<Gfa> snapShots ) {
								
		/*
		 *  Clip against the FA areas
		 *  
		 *  1. Clip against the FA areas and compute the size of the intersection 
		 *     in each FA area.
		 *  2. Find the area that the smear has the largest intersection
		 *  3. Find the area that the smear has the second largest intersection, if any.
		 *  4. Apply the area rules: 
		 *     a. The primary "area" is the area that the smear intersects with a larger size. 
		 *     b. When both areas >= 3K, both areas are considered as primary areas and 
		 *        one additional AIRMET should be created (in GfaGenerate.java)
		 *     c. the part in an FA area must also intersect with one of the snapshots
		 *        with an area >= 3K, if there are associated snapshots.
		 */
		HashMap<String, Geometry> areaBnds = GfaClip.getInstance().getFaAreaBoundsInGrid();
		HashMap<String, Double> interSizes = new HashMap<String, Double>();
		HashMap<String, Boolean> interWithSS = new HashMap<String, Boolean>();				
		
		Polygon clipPoly = GfaClip.getInstance().gfaToPolygonInGrid( current );

		for ( String areaName : areaBnds.keySet() ) {
			
			Geometry g = areaBnds.get( areaName );

			if ( clipPoly.intersects( g ) ) {

				Geometry interAA = clipPoly.intersection( g );				
				
				double ss = PgenUtil.getSphPolyAreaInGrid( interAA );	
				
				interSizes.put( areaName, ss );
				              
				boolean interSSBig = false;
				if ( snapShots == null || snapShots.size() <= 0 ) {
					interSSBig = true;
				}
				else { 
				    for ( Gfa gfa : snapShots ) {

					    Polygon p = GfaClip.getInstance().gfaToPolygonInGrid( gfa );
					    //Note: interAA might be an GeometryCollection and thus
					    //      will be illegal parameter for intersects().
					    
					    double narea = 0.0;
					    for ( int nn = 0; nn < interAA.getNumGeometries(); nn++ ) {
						    if ( p.intersects( interAA.getGeometryN( nn ) ) ) {										
					            Geometry in_1 = p.intersection( interAA.getGeometryN( nn ) );
						        narea += PgenUtil.getSphPolyAreaInGrid( in_1 );
						    }
					    }
						
						if ( narea >= AREA_LIMIT ) {
							interSSBig = true;
							break;
					    }
					}
				}
				
				interWithSS.put( areaName, interSSBig );
			}			
		}
				        
		// Find the primary and the secondary area
		String primaryArea = null;
		double biggest = 0.0;
		for ( String areaName : interSizes.keySet() ) {
			if ( interSizes.get( areaName ) > biggest ) {
				primaryArea = new String( areaName );
				biggest = interSizes.get( areaName );
			}			
		}
		
		String secondArea = null;
		double second = 0.0;
		for ( String areaName : interSizes.keySet() ) {
			if ( !areaName.equals( primaryArea ) &&
				 interSizes.get( areaName ) > second ) {
				
				secondArea = areaName;
				second = interSizes.get( areaName );
			}			
		}
        		
		// Set up "area" info in the smear.
		StringBuilder  gfaArea = new StringBuilder();
		if ( primaryArea != null && interWithSS.get( primaryArea ) ) {
						
			gfaArea.append( primaryArea );
			
			if ( secondArea != null && interSizes.get( secondArea ) >= AREA_LIMIT &&				
				interWithSS.get( secondArea ) ) {

				gfaArea.append( "-" + secondArea );			        
			}		
		}
				
	    current.setGfaArea( gfaArea.toString() );	
				
	}
	
	/**
	 * Assigns airmetTag to the smear.
	 * 
	 * @param smear
	 */
	static void assignAirmetTag(Gfa smear) {		
        
		/*
		 * Note, if necessary, this could be controlled as a preference?
		 */
		boolean addAirmetTag = true;
		String haz = smear.getGfaHazard();
		if ( addAirmetTag && !("FZLVL".equals( haz ) ) && !("M_FZLVL".equals( haz ) )) {
			String prefix = "";
			if ( haz.equals( "TURB-HI") ) {
				prefix = "H";
			}
			else if ( haz.equals( "TURB-LO") ) {
				prefix = "L";
			}
			
			String airmetTag = new String ( prefix + smear.getGfaTag()+smear.getGfaDesk() );
			smear.setGfaValue( Gfa.AIRMET_TAG, airmetTag );
			
		}

	}
	
}
