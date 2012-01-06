package gov.noaa.nws.ncep.common.log.impl;

public enum NcepLogCategory {

	EDEX_INGEST_LOGGER_CATEGORY  ("ingest"), 
	
	EDEX_INGEST_GRIB_LOGGER_CATEGORY  ("ingestGrib"), 
	
	EDEX_REQUEST_LOGGER_CATEGORY  ("request"), 

	NCEP_CAVE_LOGGER_CATEGORY  ("ncep-cave"); 


	private String category; 
	
	private NcepLogCategory(String categoryString) {
		this.category = categoryString; 
	}
	
	public String getCategory() {
		return category; 
	}
}
