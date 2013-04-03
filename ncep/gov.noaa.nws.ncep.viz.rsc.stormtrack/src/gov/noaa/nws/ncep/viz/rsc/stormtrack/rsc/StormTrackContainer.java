/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.stormtrack.rsc;

import gov.noaa.nws.ncep.common.dataplugin.stormtrack.StormTrackRecord;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author sgilbert
 *
 */
public class StormTrackContainer {

//	private Map<StormIdentifier, StormTrack> modelMap;
	private Map<String, Map<StormIdentifier, StormTrack> >  modelMap;

	/**
	 * 
	 */
	public StormTrackContainer() {
		modelMap = new HashMap<String, Map<StormIdentifier, StormTrack> >();
	}
	
	public Collection<StormTrack> getStormTracksByModel(String modelName) {
		if( modelMap.containsKey( modelName ) ) {
		return modelMap.get(modelName).values();
	}
		else {
			return new ArrayList<StormTrack>();
		}
	}
	
	public void addStormRecord(StormTrackRecord record) {
		Map<StormIdentifier, StormTrack> tracks;
		
		/*
		 * find or create the set of StormTracks for this model
		 */
		String modelName = record.getModel();
		if ( modelName == null ) return;
		
		if ( modelMap.containsKey(modelName) ) {
			tracks = modelMap.get(modelName);
		}
		else {
			tracks = new HashMap<StormIdentifier, StormTrack>();
			modelMap.put(modelName, tracks);
		}
		
		/*
		 * find or create the track for the current storm/cyclone
		 */
		StormTrack thisTrack;
		StormIdentifier stormId = new StormIdentifier(modelName, record.getDataTime().getRefTime(), 
				                       record.getCycloneNum());
		if ( tracks.containsKey(stormId) ) {
			thisTrack = tracks.get(stormId);
		}
		else {
			thisTrack = new StormTrack(stormId);
			tracks.put(stormId, thisTrack);
		}
		
		/*
		 * Add this storm location to the Track
		 */
		StormLocation sloc = new StormLocation(record.getClat(), record.getClon(), 
		                                       record.getFcstHour(), record.getMslp(),
		                                       record.getWindMax() );
		thisTrack.addStormLocation(sloc);
	}
	
	public Set<String> getModels() {
		return modelMap.keySet();
	}
	
	public void clear() {
		for (Map<StormIdentifier, StormTrack> map : modelMap.values() ) {
			map.clear();
		}
		modelMap.clear();
	}
	
}
