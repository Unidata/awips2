package gov.noaa.nws.ncep.viz.rsc.ffa.util;

import java.util.HashMap;
import java.util.Map;

public class AwwImmediateCauseUtil {
	private static Map<String, String> immediateCauseDescMap = new HashMap<String, String>(15); 
	
	static {
		initMap(); 
	}
	
	private static void initMap() {
		immediateCauseDescMap.put("ER", "Excessive Rainfall"); 
		immediateCauseDescMap.put("SM", "Snowmelt"); 
		immediateCauseDescMap.put("RS", "Rain and Snowmelt"); 
		immediateCauseDescMap.put("DM", "Dam or Levee Failure"); 
		immediateCauseDescMap.put("IJ", "Ice Jam"); 
		immediateCauseDescMap.put("GO", "Glacier-Dammed Lake Outburst"); 
		immediateCauseDescMap.put("IC", "Rain and/or Snowmelt and/or Ice Jam"); 
		immediateCauseDescMap.put("FS", "Upstream Flooding plus Storm Surge"); 
		immediateCauseDescMap.put("FT", "Upstream Flooding plus Tidal Effects"); 
		immediateCauseDescMap.put("ET", "Elevated Upstream Flow plus Tidal Effects"); 
		immediateCauseDescMap.put("WT", "Wind and/or Tidal Effects"); 
		immediateCauseDescMap.put("DR", "Upstream Dam or Reservoir Release"); 
		immediateCauseDescMap.put("MC", "Other Multiple Causes"); 
		immediateCauseDescMap.put("OT", "Other Effects"); 
		immediateCauseDescMap.put("UU", "Unknown"); 
	}

	public static String getImmediateCauseDesc(String immediateCauseAbbreviation) {
		String desc = null; 
		if(immediateCauseAbbreviation != null)
			desc = immediateCauseDescMap.get(immediateCauseAbbreviation); 
		if(desc == null)
			desc = ""; 
		return desc; 
	}
}
