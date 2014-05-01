/*
 * TCVEvent
 * 
 * Date created 03 NOVEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tca;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;

/**
 * This class represents an Event group in a Tropical Cyclone VTEC (TCV) message.
 * The event group consists of a UGC line, one or more VTEC lines, and one or more 
 * tropical cyclone breakpoints.
 * @author sgilbert
 *
 */
public class TCVEvent implements Comparable<TCVEvent> {

	public static enum TCVEventType  { LIST, SEGMENT };
	
	private TCVEventType evenType;
	private UGCGroup ugc;
	private List<TVtecObject> vtecLines;
	private List<Breakpoint> breakpoints;

	/**
	 * @param evenType
	 */
	public TCVEvent(TCVEventType evenType) {
		super();
		this.evenType = evenType;
		ugc = new UGCGroup();
		vtecLines = new ArrayList<TVtecObject>();
		breakpoints =  new ArrayList<Breakpoint>();
	}
	
	
 	
	/**
	 * @return the evenType
	 */
	public TCVEventType getEvenType() {
		return evenType;
	}



	/**
	 * @return the vtecLines
	 */
	public List<TVtecObject> getVtecLines() {
		return vtecLines;
	}



	/**
	 * @return the breakpoints
	 */
	public List<Breakpoint> getBreakpoints() {
		return breakpoints;
	}



	public void addVtecLine( TVtecObject vtec ) {
		vtecLines.add(vtec);
		Collections.sort(vtecLines);
	}



	/**
	 * @return the ugc
	 */
	public UGCGroup getUgc() {
		return ugc;
	}

	/**
	 * @param breakpoints the breakpoints to set
	 */
	public void setBreakpoints(List<Breakpoint> breakpoints) {
		this.breakpoints = breakpoints;
	}
	
	public void addZones( List<String> zones) {
		ugc.addZones(zones);
	}
	
	public void setPurgeTime(Calendar time) {
		ugc.setPurgeTime(time);
	}

	/**
	 * The ordering of two event groups is defined the same as the ordering of 
	 * the highest priority VTEC line of each. 
	 */
	@Override
	public int compareTo(TCVEvent o) {
		
		TVtecObject thisone = this.getVtecLines().get(0);
		TVtecObject thatone = o.getVtecLines().get(0);

		return thisone.compareTo(thatone);
	}



	/* 
	 * two Events are considered equal if their leading VTEC lines have the same priority
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {

		if ( obj instanceof TCVEvent ) {
			TVtecObject thisone = this.getVtecLines().get(0);
			TCVEvent o = (TCVEvent)obj;
			TVtecObject thatone = o.getVtecLines().get(0);
			return thisone.equals(thatone);
		}
		else
			return false;
	}



	public void addBreakpoint( Breakpoint bkpt) {
		breakpoints.add(bkpt);
		}
	
	
}
