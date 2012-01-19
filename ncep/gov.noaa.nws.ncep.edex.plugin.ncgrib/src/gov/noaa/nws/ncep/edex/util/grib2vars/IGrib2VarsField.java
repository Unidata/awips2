package gov.noaa.nws.ncep.edex.util.grib2vars;

public interface IGrib2VarsField {

	public static enum Grib2VarsField {
		G2VARSID,	// g2vars id
		DISCIPLINE,   // The discipline
		CATEGORY,	// The category
		PID,		// The parameter id
		PDT,		// The product definition template
		NAME,	// description of this g2vars
		UNITS,	// units
		GNAM,	// gnam
		SCALE,	// scale
		MISSING		// The missing value
	}

}
