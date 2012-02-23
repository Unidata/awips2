/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

import java.util.Date;
import java.util.TreeSet;

/**
 * @author sgilbert
 *
 */
public class StormTrack {

	private StormIdentifier stormId;
	
	private TreeSet<StormLocation> track;

	/**
	 * @param model
	 * @param reftime
	 * @param cycloneNum
	 */
	public StormTrack(String model, Date reftime, String cycloneNum) {
		this( new StormIdentifier(model, reftime, cycloneNum) );
	}
	
	/**
	 * @param stormId
	 */
	public StormTrack( StormIdentifier stormId ) {
		this.stormId = stormId;
		this.track = new TreeSet<StormLocation>();
	}

	public void addStormLocation(StormLocation sloc) {
		track.add(sloc);
	}

	/**
	 * @return the stormId
	 */
	public StormIdentifier getStormId() {
		return stormId;
	}

	/**
	 * @return the track
	 */
	public TreeSet<StormLocation> getTrack() {
		return track;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return stormId.toString();
	}
	
}
