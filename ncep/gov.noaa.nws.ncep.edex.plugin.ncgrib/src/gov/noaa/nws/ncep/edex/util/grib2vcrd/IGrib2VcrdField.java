package gov.noaa.nws.ncep.edex.util.grib2vcrd;

public interface IGrib2VcrdField {

	public static enum Grib2VcrdField {
		G2VCRDID,	// g2vars id
		VCRDID1,   // The vertical coordination 1
		VCRDID2,	// The vertical coordination 2
		NAME,	// description of this g2vars
		UNITS,	// units
		GNAM,	// gnam
		SCALE,	// scale
	}

}
