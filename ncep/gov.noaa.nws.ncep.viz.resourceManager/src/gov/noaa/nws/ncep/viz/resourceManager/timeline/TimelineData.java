/*
 * Classname
 * 
 * Date created (as DD MONTH YYYY)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.resourceManager.timeline;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * This class maintains an ordered map of data times, where each time
 * is mapped to a Boolean value indicating whether that time is "selected" from 
 * the overall list.
 * 
 * @author sgilbert
 *
 */
public class TimelineData {

	private TreeMap<Calendar,Boolean> times;

	/**
	 * Initializes all available time to non-selected status
	 * @param timeList list of available data times
	 */
	TimelineData( List<Calendar> timeList ) {

		times = new TreeMap<Calendar,Boolean>();
		for ( Calendar cal : timeList ) {
				times.put(cal, false);
		}

	}
	
	/**
	 * checks if the list of available times is empty
	 * @return
	 */
	public boolean isEmpty() {
		return times.isEmpty();
	}

	/**
	 * Returns the first available data time in the list
	 * @return
	 */
	public Calendar getFirstTime() {
		return times.firstKey();
	}
	
	/**
	 * Returns the last available data time in the list
	 * @return
	 */
	public Calendar getLastTime() {
		return times.lastKey();
	}

	/**
	 * Returns a set of all the available data times
	 * @return
	 */
	public Set<Calendar> getTimes() {
		return times.keySet();
	}
	
	/**
	 * Returns a list of "selected" data times from the list of available times 
	 * @return
	 */
	public List<Calendar> getSelectedTimes() {
		ArrayList<Calendar> list = new ArrayList<Calendar>();
		for ( Calendar cal : times.keySet() ) {
			if ( times.get(cal) ) list.add(cal);
		}
		return list;
	}

	/**
	 * Checks if the specified time is selected
	 */
	public boolean isSelected(Calendar cal) {
		return times.get(cal);
	}
	
	/**
	 * Returns the total number of minutes between the first and last available
	 * data times
	 * @return
	 */
	public int getTotalMinutes() {
		Calendar first = getFirstTime();
		Calendar last = getLastTime();
		long timeLength = last.getTimeInMillis() - first.getTimeInMillis();
		return (int)(timeLength/60000);
	}

	/**
	 * Returns the total number of milliseconds between the first and last available
	 * data times
	 * @return
	 */
	public long getTotalMillis() {
		Calendar first = getFirstTime();
		Calendar last = getLastTime();
		return last.getTimeInMillis() - first.getTimeInMillis();
	}

	/*
	public void selectFirst(int num) {
		
		int j=0;
		Calendar cal = times.firstKey();
		while ( cal != null ) {
			if ( j++ < num ) times.put(cal, true);
			else times.put(cal, false); 
			
			cal = times.higherKey( cal );
		}
	}
	
	public void selectLast(int num) {

		int j=0;
		Calendar cal = times.lastKey();
		while ( cal != null ) {
			if ( j++ < num ) times.put(cal, true);
			else times.put(cal, false); 
			
			cal = times.lowerKey( cal );
		}
		
	}
	*/

	/**
	 * Returns the first selected time in the list
	 */
	public Calendar getFirstSelected() {
		
		Map.Entry<Calendar, Boolean> entry = times.firstEntry();
		while ( entry != null ) {
			if ( entry.getValue() ) return entry.getKey();
			entry = times.higherEntry( entry.getKey() );
		}
		return null;
	}

	/**
	 * Returns the last selected time in the list
	 * @return
	 */
	public Calendar getLastSelected() {
		
		Map.Entry<Calendar, Boolean> entry = times.lastEntry();
		while ( entry != null ) {
			if ( entry.getValue() ) return entry.getKey();
			entry = times.lowerEntry( entry.getKey() );
		}
		return null;
	}

	/**
	 * Returns the available time just before the specified time
	 * @param cal
	 * @return
	 */
	public Calendar getPreviousTime(Calendar cal) {
		return times.lowerKey(cal);
	}

	/**
	 * Returns the available time just after the specified time
	 * @param cal
	 * @return
	 */
	public Calendar getNextTime(Calendar cal) {
		return times.higherKey(cal);
	}

	/**
	 * Toggles the "select" indicator for the given time
	 * @param cal
	 */
	public void toggle(Calendar cal) {
		if ( times.containsKey(cal) ) {
			times.put( cal, !times.get(cal) );
		}
	}

	/**
	 * Returns the number of selected data times
	 * @return
	 */
	public int numSelected() {
		int num = 0;
		for ( Boolean val : times.values() ) {
			if ( val ) num++;
		}
		return num;
	}

	/**
	 * Marks the given time as selected
	 * @param cal
	 */
	public void select(Calendar cal) {
		if ( times.containsKey(cal) ) times.put(cal, true);
	}

	/**
	 * Marks the given time as not selected
	 * @param cal
	 */
	public void deselect(Calendar cal) {
		if ( times.containsKey(cal) ) times.put(cal, false);
	}

	/**
	 * Returns the number of available times in the list
	 * @return
	 */
	public int getSize() {
		return times.size();
	}

	/**
	 * Adds or removes times from the end of the selected list until the specified 
	 * number of times (numFrames) is selected.  The skip factor is used to skip 
	 * a specific number of times between each selected time.
	 * @param numFrames
	 * @param skip
	 */
	public void appendTimes(int numFrames, int skip) {
		
		if ( numFrames > numSelected() ) {
			if ( numSelected() == 0 ) select(getFirstTime());
			addToLast( numFrames, skip );
			if ( numFrames > numSelected() ) {
				addToFirst( numFrames, skip );
			}
		}
		else if ( numFrames < numSelected() ) {
			removeFromLast( numFrames, skip);
		}
		
	}

	/**
	 * Adds times to the end of the selected list until the specified 
	 * number of times (numFrames) is selected.  The skip factor is used to skip 
	 * a specific number of times between each selected time.
	 * @param numFrames
	 * @param skip
	 */
	private void addToLast(int numFrames, int skip) {

		while ( numSelected() < numFrames ) {
			
			Calendar cal = getLastSelected();
			for ( int j=0; j <= skip; j++ ) {
				cal = getNextTime(cal);
				if ( cal == null) return;
			}
			select(cal);
			
		}
	
	}

	/**
	 * Removes times from the end of the selected list until the specified 
	 * number of times (numFrames) is selected.  The skip factor is used to skip 
	 * a specific number of times between each selected time.
	 * @param numFrames
	 * @param skip
	 */
	private void removeFromLast(int numFrames, int skip) {

		int count = numSelected();
		while ( count > numFrames ) {
			Calendar cal = getLastSelected();
			deselect(cal);
			count = numSelected();
		}
	}

	/**
	 * Adds or removes times from the beginning of the selected list until the specified 
	 * number of times (numFrames) is selected.  The skip factor is used to skip 
	 * a specific number of times between each selected time.
	 * @param numFrames
	 * @param skip
	 */
	public void prependTimes(int numFrames, int skip) {

		if ( numFrames > numSelected() ) {
			if ( numSelected() == 0 ) select(getLastTime());
			addToFirst( numFrames, skip );
			if ( numFrames > numSelected() ) {
				addToLast( numFrames, skip );
			}
		}
		else if ( numFrames < numSelected() ) {
			removeFromFirst( numFrames, skip);
		}
		
	}

	/**
	 * Adds times to the beginning of the selected list until the specified 
	 * number of times (numFrames) is selected.  The skip factor is used to skip 
	 * a specific number of times between each selected time.
	 * @param numFrames
	 * @param skip
	 */
	private void addToFirst(int numFrames, int skip) {

		while ( numSelected() < numFrames ) {
			
			Calendar cal = getFirstSelected();
			for ( int j=0; j <= skip; j++ ) {
				cal = getPreviousTime(cal);
				if ( cal == null) return;
			}
			select(cal);
			
		}
	}

	/**
	 * Removes times from the beginning of the selected list until the specified 
	 * number of times (numFrames) is selected.  The skip factor is used to skip 
	 * a specific number of times between each selected time.
	 * @param numFrames
	 * @param skip
	 */
	private void removeFromFirst(int numFrames, int skip) {
		
		int count = numSelected();
		while ( count > numFrames ) {
			Calendar cal = getFirstSelected();
			deselect(cal);
			count = numSelected();
		}
	}

	/**
	 * Updates the selected times in the given time range.  The skip factor is used to skip 
	 * a specific number of times between each selected time.
	 * @param numFrames
	 * @param skip
	 */
	public void updateRange(Calendar first, Calendar second, int skip) {

		int j=0, loc1=0, loc2=0;
		
		deselectAll();
		
		for ( Calendar cal : times.keySet() ) {
			if ( cal.equals(first) ) loc1 = j;
			if ( cal.equals(second) ) loc2 = j;
			j++;
		}
		
		int avail = Math.abs(loc2 -loc1) + 1;
		int num = avail / (skip+1);
		if ( num * (skip + 1) < avail ) num++;

		if ( first.before(second) ) {
			select(first);
			addToLast( num, skip);
		}
		else if ( first.after(second) ) {
			select(first);
			addToFirst( num, skip);
		}
		else {
			select(first);
		}
		
	}

	/**
	 * Marks all times in the list as not selected
	 */
	public void deselectAll() {
		for ( Calendar cal : times.keySet() ) {
			deselect(cal);
		}
	}

}
