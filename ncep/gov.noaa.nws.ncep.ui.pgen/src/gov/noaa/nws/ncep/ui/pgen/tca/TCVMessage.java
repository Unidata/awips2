/*
 * TCVMessage
 * 
 * Date created 03 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.tca.TCVEvent.TCVEventType;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class is used to create a Tropical Cyclone VTEC (TCV) message.
 * The format of the message is described in NWSI 10-601, Section 1.8
 * @ http://www.nws.noaa.gov/directives.
 * 
 * The content of the message is obtained from one or two TCAElement objects, 
 * which contain cyclone advisory watch/warning information about the current and/or 
 * previous advisory.  These TCAElement(s) are passed in through the constructor.
 * @author sgilbert
 *
 */
public class TCVMessage {

	private static final String TIME_FORMAT = "hmm a ";
	private static final String DATE_FORMAT = " E MMM d yyyy";
	//private static final String DATE_FORMAT = "hmm a 'XXX' E MMM d yyyy";
	private static final String BREAKPOINT_FORMAT = "%-36s%.2fN %.2fW";
	private static final String TPC = "NWS TPC/NATIONAL HURRICANE CENTER MIAMI FL";
	private static final String CPHC ="NWS CENTRAL PACIFIC HURRICANE CENTER";
	private static final String HEADER = " WATCH/WARNING BREAKPOINTS/ADVISORY NUMBER ";
	private static final String HEADER_INT = " WATCH/WARNING BREAKPOINTS/INTERMEDIATE ADVISORY NUMBER ";
	private static final String TEST_STRING = "...THIS IS ONLY A TEST...";
	public static final String ACTION_NEW = "NEW";
	public static final String ACTION_CONT = "CON";
	public static final String ACTION_CANCEL = "CAN";
	private static final HashMap<String,String> prBreakpoints = new HashMap<String,String>();
	
	private String issuingCenter;
	
	private String stormName;
	private int stormNumber;
	private String advisoryNumber;
	private String basin;
	private Calendar issueTime;
	private String stormType;
	private String timeZone;
	private String productClass;
	
	private Calendar purgeTime;
	private int seqNumber;
	
	//list of UGC Group events that make up the TCV message;
	private List<TCVEvent> events;
	
	/*
	 * This hashmap is used to associate a given advisory to a particular
	 * status, such as NEW, CONtinued, or CANcelled.
	 */
	private HashMap<TropicalCycloneAdvisory,String> statusMap;
		
	static {
		prBreakpoints.put("PRZ008", "Puerto_Rico_Northwest");
		prBreakpoints.put("PRZ002", "Puerto_Rico_Northeast");
		prBreakpoints.put("PRZ003", "Puerto_Rico_Southeast");
		prBreakpoints.put("PRZ011", "Puerto_Rico_Southwest");
	}
	
	/**
	 * Constructor specifying TCAElement for the current advisory.  This is
	 * used when there is no previous advisory, and all watch warnings are assumed
	 * to be "NEW".
	 * @param center id of the issuing office
	 * @param tca TCAElement containing current advisory information
	 */
	public TCVMessage( String center, TCAElement tca ) {
		
		saveStormInfo(center, tca);
		events = createNewEvents(tca);
	
	}

	/**
	 * Constructor used to specify the current and previous TCAElements.  The previous element
	 * is used to determine whether watch/warnings are NEW, CONtinued or CANcelled, 
	 * @param center id of the issuing office
	 * @param prev TCAElement containing previous advisory information
	 * @param tca TCAElement containing current advisory information
	 */
	public TCVMessage( String center, TCAElement prev, TCAElement tca ) {
		
		saveStormInfo(center, tca);
		events = createEvents(prev, tca);
	
	}

	/*
	 * Save current storm and advisory information
	 */
	private void saveStormInfo(String center, TCAElement tca) {
		
		issuingCenter = center;
		
		stormName = tca.getStormName().toUpperCase();
		stormNumber = tca.getStormNumber();
		advisoryNumber = tca.getAdvisoryNumber().toUpperCase();
		basin = Basin.getBasinAbbrev( tca.getBasin() ).toUpperCase();
		issueTime = tca.getAdvisoryTime();
		stormType = tca.getStormType().toUpperCase();
		timeZone = tca.getTimeZone();
		
		productClass = convertStatus(tca.getIssueStatus());
		
		purgeTime = calculatePurgeTime(tca.getAdvisoryTime());
		seqNumber = generateETN();
		
		// create map
		statusMap = new HashMap<TropicalCycloneAdvisory,String>();
	}

	/*
	 * Add all watch/warnings to the status map with a status of "NEW"
	 */
	private List<TCVEvent> createNewEvents(TCAElement tca) {

		for ( TropicalCycloneAdvisory tcadv : tca.getAdvisories() ) {
			
			//  If segment does not contain any zones, it does not get included
			//  in the TCV.  move on to next...
			if ( tcadv.getSegment().getZones() ==  null || 
				 tcadv.getSegment().getZones().isEmpty() ) continue;

			statusMap.put(tcadv, ACTION_NEW);
			
		}
		
		return advisoriesToEvents();
	}

	/*
	 * Sift through watch/warnings of both TCAElements to determine which advisories 
	 * are NEW, CANcelled, or CONtinued from the previous advisory
	 */
	private List<TCVEvent> createEvents(TCAElement prev, TCAElement tca) {
		
		/*
		 * Loop through advisories in previous TCAElement
		 */
		for ( TropicalCycloneAdvisory padv : prev.getAdvisories() ) {
			
			// If advisory contains no zones, move on to the next; this one not included in VTEC
			if ( padv.getSegment().getZones() ==  null || 
				 padv.getSegment().getZones().isEmpty() ) continue;
			
			if ( padv.getSegment() instanceof IslandBreakpoint ||
				 padv.getSegment() instanceof WaterBreakpoint ) {
				
				/*
				 * If same watch/warning is in current advisory, identify as CONtinued.
				 * Otherwise, set as CANCelled
				 */
				String action = ACTION_CANCEL;
				for ( TropicalCycloneAdvisory a : tca.getAdvisories() ) {
					if ( padv.equals(a) ) {
						action = ACTION_CONT;
						break;
					}
				}
				statusMap.put(padv, action);

			}
			else if ( padv.getSegment() instanceof BreakpointPair ) {
				
				/*
				 * If watch/warning segment overlaps any current segments, 
				 * split up into segments with different statuses.
				 * Otherwise, mark previous segment as CANceled 
				 */
				HashMap<TropicalCycloneAdvisory,String> split = null;
				for ( TropicalCycloneAdvisory a : tca.getAdvisories() ) {
					if ( padv.overlaps(a) ) {
						split = AdvisoryUtils.createSegmentMap(padv, a);
						statusMap.putAll(split);
					}
				}
				if ( split == null ) {
					statusMap.put(padv, ACTION_CANCEL);
				}
				
			}
			
		}
		
		/*
		 * Now go through advisories in current TCAElement
		 */
		for ( TropicalCycloneAdvisory adv : tca.getAdvisories() ) {
			 
			// If advisory contains no zones, move on to the next;  this one not included in VTEC
			if ( adv.getSegment().getZones() ==  null || 
			     adv.getSegment().getZones().isEmpty() ) continue;
			
			if ( adv.getSegment() instanceof IslandBreakpoint ||
				 adv.getSegment() instanceof WaterBreakpoint ) {
				
				/*
				 * if current watch/warning is in status map, ignore it.
				 * Otherwise, mark it as NEW.
				 */
				String action = ACTION_NEW;
				for ( TropicalCycloneAdvisory t : statusMap.keySet() ) {
					if ( adv.equals(t) ) {
						action = null;
						break;
					}
				}
				
				if ( action != null ) statusMap.put(adv, action);
			
			}
			else if ( adv.getSegment() instanceof BreakpointPair ) {
				
				/*
				 * If watch/warning segment overlaps on that is already in the status map,
				 * ignore it - it has already been considered.
				 * Otherwise, set segment as NEW.
				 */
				String action = ACTION_NEW;
				for ( TropicalCycloneAdvisory t : statusMap.keySet() ) {
					if ( adv.overlaps(t) ) {
						action = null;
						break;
					}
				}
				if ( action != null ) statusMap.put(adv, action);
				
			}
			
		}
		
		return advisoriesToEvents();
	}

	/*
	 * converts a list of TropicalCycloneAdvisory objects to a list of
	 * TCVEvent objects that better represent the contents of a TCV message
	 */
	private List<TCVEvent> advisoriesToEvents() {
		
		List<TCVEvent> eventList = new ArrayList<TCVEvent>();
		
		//  First determine whether any advisories need to be further segmented.
		segmentAdvisories();
		
		for ( TropicalCycloneAdvisory tcadv : statusMap.keySet() ) {
			
			// If advisory contains no zones, move on to the next;  this one not included in VTEC
			if ( tcadv.getSegment().getZones() ==  null || 
			     tcadv.getSegment().getZones().isEmpty() ) continue;
			
			// special treatment for US Border points and Puerto Rico.
			TropicalCycloneAdvisory adv = preprocessAdvisory(tcadv);
			
			TCVEventType type = TCVEventType.LIST;
			if ( adv.getSegment() instanceof BreakpointPair ) type = TCVEventType.SEGMENT;
			TCVEvent event = new TCVEvent(type);
			
			//  create new VTEC object associated with this event
			TVtecObject vtec = new TVtecObject( productClass, statusMap.get(tcadv), issuingCenter,
					convertSeverity(adv.getSeverity()),
					convertAdvisoryType(adv.getAdvisoryType()),
					seqNumber, issueTime, null);
			
			//  Add appropriate info to the Event
			event.addVtecLine(vtec);
			event.setBreakpoints( adv.getSegment().getBreakpoints() );
			event.addZones(adv.getSegment().getZones());
			event.setPurgeTime( purgeTime );
			
			//eventList.add(event);
			addToEventList(event, eventList);
		}
		
		return eventList;
	}

	/*
	 * performs special checks when adding a new event to the list of events.
	 * Some new ones may be included in existing events.
	 */
	private void addToEventList(TCVEvent event, List<TCVEvent> eventList) {

		switch ( event.getEvenType() ) {
		
		case LIST:
			addListEvent(event, eventList);
			break;
			
		case SEGMENT:
			addSegmentEvent(event, eventList);
			break;
		}
		
	}

	/*
	 * checks to see if this breakpoint can be added to an existing event
	 * with the same VTEC priority and same breakpoint state
	 */
	private void addListEvent(TCVEvent event, List<TCVEvent> eventList) {
		
		Breakpoint thisbkpt = event.getBreakpoints().get(0);
		
		for ( TCVEvent tcev : eventList ) {
			if ( tcev.getEvenType() != TCVEventType.LIST ) continue;
			Breakpoint bkpt2 = tcev.getBreakpoints().get(0);
			if ( event.equals(tcev) && 
				 thisbkpt.getState().equals(bkpt2.getState()) ) {
				tcev.addBreakpoint(thisbkpt);
				tcev.addZones( event.getUgc().getZones());
				return;
			}
		}
			
		eventList.add(event);
	}

	/*
	 * checks to see if this breakpoint pair can be added to an existing
	 * event with the same breakpoint pair
	 */
	private void addSegmentEvent(TCVEvent event, List<TCVEvent> eventList) {

		List<Breakpoint> pair1 = event.getBreakpoints();

		for ( TCVEvent tcev : eventList ) {
			if ( tcev.getEvenType() != TCVEventType.SEGMENT ) continue;
			
			/*
			 * if find an event with the same breakpoint pair, just add new vtec line to 
			 * existing event.
			 */
			List<Breakpoint> pair2 = tcev.getBreakpoints();
			if ( pair1.get(0).equals( pair2.get(0) ) &&
				 pair1.get(1).equals( pair2.get(1) ) ) {
				tcev.addVtecLine( event.getVtecLines().get(0) );
				return;
			}
		}
		
		eventList.add(event);
		
	}

	/*
	 * Makes sure that breakpoint segments are divided into one or more segments,
	 * so that they can be easily grouped together by UGC segment when creating Events
	 */
	private void segmentAdvisories() {

		Set<Breakpoint> bkptSet = new HashSet<Breakpoint>();
	
		/*
		 * loop through all advisories to accumulate a set of all coastal breakpoints
		 */
		for ( TropicalCycloneAdvisory tcadv : statusMap.keySet() ) {
			if ( tcadv.getSegment() instanceof BreakpointPair ) {
				bkptSet.add( tcadv.getSegment().getBreakpoints().get(0) );
				bkptSet.add( tcadv.getSegment().getBreakpoints().get(1) );
			}
		}
		
		/*
		 * loop through advisories again and segment it, if necessary,
		 * based on the set of breakpoints found above.
		 */
		HashMap<TropicalCycloneAdvisory,String> newmap = new HashMap<TropicalCycloneAdvisory,String>();
 		for ( TropicalCycloneAdvisory tcadv : statusMap.keySet() ) {
			if ( tcadv.getSegment() instanceof BreakpointPair ) {
				List<TropicalCycloneAdvisory> advlist = AdvisoryUtils.segmentAdvisory( tcadv, bkptSet );
				if ( ! advlist.isEmpty() ) {
					String status = statusMap.get(tcadv);
					
					for ( TropicalCycloneAdvisory tadv : advlist ) {
						newmap.put(tadv, status);
					}
				}
				else {
					newmap.put(tcadv, statusMap.get(tcadv));
				}
			}
			else {
				newmap.put(tcadv, statusMap.get(tcadv));
			}
		}
 		statusMap = newmap;

	}

	/*
	 * Checks for special changes that need to be made to the advisory.
	 * Special checks are made for watch/warnings that span US border and
	 * for Puerto Rico
	 */
	private TropicalCycloneAdvisory preprocessAdvisory(	TropicalCycloneAdvisory adv ) {
		
		/*
		 * if advisory crosses US/MEX or US/CAN border, reset border breakpoint
		 */
		if ( advisoryCrossesUSBorder(adv) ) {
			return setBorder(adv);
		}
		
		/*
		 * Puerto Rico breakpoints are treated specially
		 */
		if ( advisoryInPuertoRico(adv) ) {
			return createPRBreakpoint(adv);
		}
		
		return adv;
	}

	/*
	 * Watch/warnings along the coast of Puerto Rico need to be represented in the 
	 * TCV message quite differently than they are selected by the forecaster.  This method
	 * modifies the Puerto Rico watches/warnings so that they will be properly
	 * displayed in the TCV. 
	 */
	private TropicalCycloneAdvisory createPRBreakpoint(TropicalCycloneAdvisory adv) {
		
		TropicalCycloneAdvisory newadv = new TropicalCycloneAdvisory();
		newadv.setAdvisoryType( adv.getAdvisoryType() );
		newadv.setGeographyType( adv.getGeographyType() );
		newadv.setSeverity( adv.getSeverity() );
		
		BPGeography segment = adv.getSegment();
		BreakpointList bl = new BreakpointList();
		bl.setPaths( segment.getPaths() );
		bl.setZones( segment.getZones() );
		
		Breakpoint bkpt1 = segment.getBreakpoints().get(0);
		Breakpoint bkpt2 = segment.getBreakpoints().get(1);

		/*
		 * If the whole island is under a watch/warning, set one 
		 * breakpoint indicating that.
		 */
		if ( bkpt1.equals( bkpt2 ) ) {
			Breakpoint prall = new Breakpoint();
			prall.setName("PUERTO-RICO-ALL");
			prall.setLocation(new Coordinate(-66.45,18.20) );
			prall.setCountry("US");
			prall.setState("PR");
			prall.setOfficial(true);
			bl.addBreakpoint(prall);
		}
		else {
			/*
			 * Select the specific quadrants that are included in the watch/warning.
			 */
			BreakpointManager bm = BreakpointManager.getInstance();
			for ( String key : prBreakpoints.keySet() ) {
				if ( segment.getZones().contains(key) ) {
					bl.addBreakpoint( bm.getBreakpoint( prBreakpoints.get(key) ) );
				}
			}
		}
		
		newadv.setSegment(bl);
		return newadv;
	}

	/*
	 * Determines whether the given advisory if for a watch/warning for Puerto Rico
	 */
	private boolean advisoryInPuertoRico(TropicalCycloneAdvisory adv) {
		
		if (adv.getSegment().getBreakpoints().get(0).getState().equals("PR") )
			return true;
		else
			return false;
	}

	/*
	 * Modifies an advisory by replacing the non US breakpoint with the appropriate
	 * US breakpoint for the US border.  This method is used so that no non-US
	 * breakpoints appear in the TCV message.
	 */
	private TropicalCycloneAdvisory setBorder(TropicalCycloneAdvisory adv) {

		BreakpointManager bm = BreakpointManager.getInstance();
		
		TropicalCycloneAdvisory newadv = new TropicalCycloneAdvisory();
		newadv.setAdvisoryType( adv.getAdvisoryType() );
		newadv.setGeographyType( adv.getGeographyType() );
		newadv.setSeverity( adv.getSeverity() );
		
		Breakpoint bkpt1 = adv.getSegment().getBreakpoints().get(0);
		Breakpoint bkpt2 = adv.getSegment().getBreakpoints().get(1);
		Breakpoint border = bm.findBorderPoint(bkpt1,bkpt2);
		
		BPGeography bp;
		if ( bkpt1.getCountry().equals("US") )
			bp = bm.getBreakpointPair(bkpt1, border);
		else
			bp = bm.getBreakpointPair(border, bkpt2);
		
		newadv.setSegment(bp);
		
		return newadv;
	}

	/*
	 * Determines whether a watch/warning segment crossed the US/MX or US/CN border.
	 */
	private boolean advisoryCrossesUSBorder(TropicalCycloneAdvisory adv) {

		boolean retval = false;
		
		if ( adv.getSegment() instanceof BreakpointPair ) {
			String country1 = adv.getSegment().getBreakpoints().get(0).getCountry();
			String country2 = adv.getSegment().getBreakpoints().get(1).getCountry();
			
			if ( country1.equals("US") && !country2.equals("US") ) 
				retval=true;
			else if ( !country1.equals("US") && country2.equals("US") ) 
				retval=true;
		}
		
		return retval;
	}

	/*
	 * Calculate the proper purge time to use in the UGC lines of the TCV message
	 */
	private Calendar calculatePurgeTime(Calendar advisoryTime) {

		Calendar purgeTime = Calendar.getInstance(advisoryTime.getTimeZone());
		purgeTime.setTimeInMillis(advisoryTime.getTimeInMillis());
		purgeTime.add(Calendar.HOUR_OF_DAY, 6);
		
		int hour = purgeTime.get(Calendar.HOUR_OF_DAY);
		if ( hour > 3 && hour < 9 ) {
			purgeTime.set(Calendar.HOUR_OF_DAY, 3);
		}
		else if ( hour > 9 && hour < 15 ) {
			purgeTime.set(Calendar.HOUR_OF_DAY, 9);
		}
		else if ( hour > 15 && hour < 21 ) {
			purgeTime.set(Calendar.HOUR_OF_DAY, 15);
		}
		else if ( hour > 21 ) {
			purgeTime.set(Calendar.HOUR_OF_DAY, 21);
		}
		else if ( hour < 3 ) {
			purgeTime.setTimeInMillis(advisoryTime.getTimeInMillis());
			purgeTime.set(Calendar.HOUR_OF_DAY, 21);
		}
		
		return purgeTime;
	}

	/*
	 * generates the Event Tracking Number (ETN) to be used in the VTEC strings
	 */
	private Integer generateETN() {
		
		int etn = (Basin.getBasinNumber(basin) * 1000 ) + stormNumber;
		return new Integer(etn);
	}

	/*
	 * Determines the appropriate Significance to use in the VTEC strings
	 */
	private String convertAdvisoryType(String advisoryType) {
		String s = new String("X");
		
		if ( advisoryType.equals("Watch") ) {
			s = new String("A");
		}
		else if ( advisoryType.equals("Warning") ) {
			s = new String("W");
		}
		
		return s;
	}

	/*
	 * Determines the appropriate Phenomena to use in the VTEC strings
	 */
	private String convertSeverity(String severity) {
		String pp = new String("XX");
		
		if ( severity.equals("Hurricane") ) {
			pp = new String("HU");
		}
		else if ( severity.equals("Tropical Storm") ) {
			pp = new String("TR");
		}
		
		return pp;
	}

	/*
	 * Determines the appropriate Product Class to use in the VTEC strings
	 */
	private String convertStatus(String issueStatus) {
		
		String k = new String("T");
		
		if ( issueStatus.equals("Operational") ) {
			k = new String("O");
		}
		else if ( issueStatus.equals("Experimental") ) {
			k = new String("E");
		}
		else if ( issueStatus.equals("Experimental Operational") ) {
			k = new String("X");
		}
		
		return k;
	}

	/**
	 * Generates the Tropical Cyclone VTEC (TCV) message.
	 * @return the Tropical Cyclone VTEC (TCV) message
	 */
	public String createText() {

		String localTime = createLocalTimeString();
		
		String center = TPC;
		if ( PgenUtil.getCurrentOffice().equalsIgnoreCase("PHFO") ||
				 PgenUtil.getCurrentOffice().equalsIgnoreCase("HFO")) center = CPHC;
		
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);

		pw.println();
		if ( StormAdvisoryNumber.isIntermediate(advisoryNumber) ) {
			pw.println(stormName + HEADER_INT + advisoryNumber);
		}
		else {
			pw.println(stormName + HEADER + advisoryNumber);
		}
		
		if ( productClass.equals("T") ) {
			pw.format("TEST...%s %s%02d%4d...TEST\n", center, basin, stormNumber, issueTime.get(Calendar.YEAR) );
		}
		else {
			pw.format("%s %s%02d%4d\n", center, basin, stormNumber, issueTime.get(Calendar.YEAR) );
		}
		pw.println(localTime);
		
		if ( productClass.equals("T") ) {
			pw.println();
			pw.println(TEST_STRING);
		}
		
		pw.println();
		pw.println("."+stormType+" "+stormName);
		pw.println();
		
		Collections.sort(events);
		
		for ( TCVEvent event : events ) {
			pw.println(event.getUgc().createUGCString());
			for ( TVtecObject vtec : event.getVtecLines() ) {
				pw.println(vtec.getVtecString());
			}
			pw.println(localTime);
			pw.println("");
			for ( Breakpoint bkpt : event.getBreakpoints() ) {
				pw.println( formatBreakpoint(bkpt) );
			}
			pw.println("");
			pw.println("$$");
			pw.println("");
		}
		
		String wfoList = generateWFOList();
		pw.format("ATTN...WFO...%s", wfoList);
		
		return sw.toString();
	}

	/*
	 * Determines which WFOs have forecast zones included in the TCV Message, 
	 * and then formats that list for inclusion in the TCV.
	 */
	private String generateWFOList() {

		BreakpointManager bm = BreakpointManager.getInstance();
		TreeSet<String> wfos = new TreeSet<String>();
		StringBuilder sb = new StringBuilder();

		for ( TCVEvent event : events ) {
			for (String zone : event.getUgc().getZones() ) {
				if ( bm.getCWA(zone) != null )  
					wfos.add( bm.getCWA(zone) );
				else
					System.out.println("Could not find WFO associated with zone: "+zone);
			}
		}
		
		for ( String wfo : wfos ) {
			sb.append(wfo);
			sb.append("...");
		}
		
		return sb.toString();
	}

	/*
	 * Format the Advisory issuance time for the TCV message for the local
	 * time zone.
	 */
	private String createLocalTimeString() {

		String localFormat =  TIME_FORMAT + "'" + timeZone + "'" + DATE_FORMAT;
		SimpleDateFormat sdf = new SimpleDateFormat(localFormat);
		sdf.setTimeZone(createLocalTimeZone());
		String time = sdf.format(issueTime.getTime()).toUpperCase();
		
		return time;
	}

	/*
	 * Returns the proper Time Zone for the current advisory
	 */
	private TimeZone createLocalTimeZone() {
		TimeZone tz;
		
		if ( timeZone.equalsIgnoreCase("AST") ) {
			tz = TimeZone.getTimeZone("GMT-4");
		}
		else if ( timeZone.equalsIgnoreCase("EDT") ) {
			tz = TimeZone.getTimeZone("GMT-4");
		}
		else if ( timeZone.equalsIgnoreCase("EST") ) {
			tz = TimeZone.getTimeZone("GMT-5");
		}
		else if ( timeZone.equalsIgnoreCase("CDT") ) {
			tz = TimeZone.getTimeZone("GMT-5");
		}
		else if ( timeZone.equalsIgnoreCase("CST") ) {
			tz = TimeZone.getTimeZone("GMT-6");
		}
		else if ( timeZone.equalsIgnoreCase("PDT") ) {
			tz = TimeZone.getTimeZone("GMT-7");
		}
		else if ( timeZone.equalsIgnoreCase("PST") ) {
			tz = TimeZone.getTimeZone("GMT-8");
		}
		else {
			tz = TimeZone.getTimeZone("GMT");
		}
		//System.out.println("FOUNDTIMEZONE "+tz.toString());
		
		return tz;
	}

	/*
	 * Formats the given breakpoint for a TCV message
	 */
	private String formatBreakpoint(Breakpoint bkpt) {
		
		double lat = bkpt.getLocation().y;
		double lon = bkpt.getLocation().x;
		String state = bkpt.getState();
		
		StringBuilder name = new StringBuilder( bkpt.getName().replace('_', '-').toUpperCase() );
		if ( (state!=null) && !state.isEmpty() && !state.equals("--") && !state.equals("PR") ) {
			name.append("-"+state.toUpperCase());
		}
		
		String bkptLine = String.format(BREAKPOINT_FORMAT, name.toString(), lat, Math.abs(lon) );
		return bkptLine;
	}

	
}
