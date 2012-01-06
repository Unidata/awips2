package gov.noaa.nws.ncep.viz.rsc.ffa.util;

public class FFAConstant {
	/*
	 * UGC String patterns
	 */
	public final static String UGC_COUNTY_PATTERN = "C"; 

	public final static String UGC_ZONE_PATTERN = "Z"; 

	public final static String UGC_ALL_STATE_ZONE_PATTERN = "Z000"; 

	public final static String[] UGC_GREAT_LAKES_STRING_PATTERN_ARRAY = {"LSZ000", "LMZ000", 
		"LHZ000", "LEZ000", "LOZ000"}; 
	
	
	/*
	 * UGC pattern int indicators
	 */
	public final static int UGC_PATTERN_UNKNOWN = 0;  
	
	public final static int UGC_COUNTY_INDICATOR = 10; 

	public final static int UGC_ZONE_INDICATOR = 11; 

	public final static int UGC_ALL_STATE_ZONE_INDICATOR = 12; 

	public final static int UGC_GREAT_LAKE_INDICATOR = 13;  
	
	public final static int UGC_NEW_PATTERN_INDICATOR = 14;  
	
	/*
	 * FFA AWW report type
	 */
	public final static String FLASH_FLOOD_ADVISORY = "FLASH_FLOOD_ADVISORY"; 
	
	public final static String FLASH_FLOOD_WARNING = "FLASH_FLOOD_WARNING"; 
	
	public final static String FLASH_FLOOD_WATCH = "FLASH_FLOOD_WATCH"; 
	
	public final static String FLASH_FLOOD_STATEMENT = "FLASH_FLOOD_STATEMENT"; 
	
	
	/*
	 * DB names
	 */
	public final static String NCEP_DB_NAME = "ncep"; 

	public final static String MAPS_DB_NAME = "maps"; 

	/*
	 * Miscellaneous string values
	 */
	public final static String UNDERSTORE = "_"; 
	
	public final static String HYPHEN = "-"; 
	
	/*
	 * Symbol type values
	 */
	public final static String FILLED_DIAMOND_SYMBOL = "FILLED_DIAMOND"; 
	
	/*
	 * Zone range string markers
	 */
	public final static String ALL_STATES_MARKER = "ALLSTATES"; 
	
	public final static String COUNTY_MARKER = "COUNTY"; 
	
	public final static String ZONE_MARKER = "ZONE"; 
	
	public final static String GREAT_LAKES_MARKER = "GREATLAKES"; 
	

	
}
