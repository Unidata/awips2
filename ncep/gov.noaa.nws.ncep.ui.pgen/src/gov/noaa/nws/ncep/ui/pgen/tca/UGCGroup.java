/*
 * UGCGroup
 * 
 * Date created 05 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.List;

/**
 * Represents a Universal Geographic Code (UGC) group consisting of a set of
 * NWS forecast zones and a purge time. 
 * @author sgilbert
 *
 */
public class UGCGroup {

	private static final String ZONE_NUM_FMT = "%03d-";
	private static final String DATE_TIME_FMT = "%02d%02d00-";
	private static final int MAX_LINE_LENGTH = 66;
	private static final char DASH = '-';
	private static final char NEW_LINE = '\n';
	
	private Calendar purgeTime;
	
	/*
	 * Stores zones by State abbreviation and a set of zone numbers for that state.
	 * A TreeSet is used to maintain numeric order of zone numbers.
	 * A TreeMap is used to maintain alphabetic order of state abbreviations.
	 */
	private TreeMap<String,TreeSet<Integer>> zoneGroups;
	
	public UGCGroup() {
		zoneGroups = new TreeMap<String,TreeSet<Integer>>();
	}

	/**
	 * @return the purgeTime
	 */
	public Calendar getPurgeTime() {
		return purgeTime;
	}

	/**
	 * @param purgeTime the purgeTime to set
	 */
	public void setPurgeTime(Calendar purgeTime) {
		this.purgeTime = purgeTime;
	}

	/**
	 * Adds another forecast zone to this UGC group.  Zone should be in
	 * SSZNNN format, where 'SS' 2 char abbreviation of the state, and 'NNN' is
	 * the three digit forecast zone.
	 * @param zone
	 */
	public void addZone(String zone) {
		
		//  separate zone string into state and zone number
		Zone z = new Zone(zone);
		String state = z.getState();
		Integer num = new Integer(z.getNumber());
		
		/*
		 * Add zone number to appropriate state key.  
		 */
		if ( zoneGroups.containsKey(state) ) {
			zoneGroups.get(state).add(num);
		}
		else {
			/*
			 * create a new key for this new state.
			 */
			TreeSet<Integer> newgroup = new TreeSet<Integer>();
			newgroup.add(num);
			zoneGroups.put(state, newgroup);
		}
		
	}
	
	/**
	 * Adds a list of forecast zones to this UGC group.  Zone should be in
	 * SSZNNN format, where 'SS' 2 char abbreviation of the state, and 'NNN' is
	 * the three digit forecast zone.
	 * @param zone
	 */
	public void addZones(List<String> zones) {
		for ( String zone : zones ) {
			this.addZone(zone);
		}
	}
	
	/**
	 * Formats the list Zones and purge time into a UGC Element string
	 * @return
	 */
	public String createUGCString() {
		StringBuilder sb = new StringBuilder();
		
		for ( String state : zoneGroups.keySet() ) {
			sb.append(state+"Z");
			for ( Integer num : zoneGroups.get(state) ) {
				sb.append( String.format(ZONE_NUM_FMT, num.intValue()) );
			}
		}
		sb.append( String.format(DATE_TIME_FMT, purgeTime.get(Calendar.DAY_OF_MONTH),
				                                purgeTime.get(Calendar.HOUR_OF_DAY) ) );
		
		/*
		 * break string up into multiple lines if > than 66 characters.
		 * Each line must end with a "-".
		 */
		int index = MAX_LINE_LENGTH - 1;
		while ( index < sb.length() ) {
			int idx = index;  
			boolean done = false;
			do {
				idx--;
				if ( sb.charAt(idx) == DASH ) {
					sb.insert(idx+1, NEW_LINE);
					done = true;
				}
			} while ( ! done );
			index = idx + MAX_LINE_LENGTH;
		}
		
		return sb.toString();
	}
	
	/**
	 * Gets the list of forecast Zones.  Zones will be returned in
	 * SSZNNN format, where 'SS' 2 char abbreviation of the state, and 'NNN' is
	 * the three digit forecast zone.
	 * @return
	 */
	public List<String> getZones() {
	
		ArrayList<String> lst = new ArrayList<String>();
		
		for ( String state : zoneGroups.keySet() ) {
			for ( Integer num : zoneGroups.get(state) ) {
				StringBuilder sb = new StringBuilder(state+"Z");
				sb.append( String.format("%03d", num.intValue()) );
				lst.add(sb.toString());
			}
		}
		
		return lst;
	}
	
}
