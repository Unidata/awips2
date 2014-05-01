package gov.noaa.nws.ncep.edex.common.stationTables;

public interface IStationField {

	public static enum StationField {
		STID,	// station id
		STNM,   // station number
		NAME,	// station name
		ST,		// state
		CO,		// country
		//LAT,	// latitude
		//LON,	// longitude
		//ELV,	// elevation
		//PRI,	// priority
		WFO,	// WFO
		LOC		// location
	}

}
