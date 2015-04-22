package gov.noaa.nws.ncep.edex.plugin.aww.util;

public class DirectionStringToDegreeConverter {
	public static final String[] DIRECT_ARRAY = new String[]{	"N","NNE","NE","ENE","E","ESE","SE","SSE",
		"S","SSW","SW","WSW","W","WNW","NW","NNW"};

	public static double getDegreeDecimal(String directionString){
		if("N".equalsIgnoreCase(directionString)) 		return 0;
		if("NNE".equalsIgnoreCase(directionString))  	return 22.5;
		if("NE".equalsIgnoreCase(directionString))  	return 45;
		if("ENE".equalsIgnoreCase(directionString)) 	return 67.5;
		if("E".equalsIgnoreCase(directionString))  		return 90;
		if("ESE".equalsIgnoreCase(directionString)) 	return 112.5;
		if("SE".equalsIgnoreCase(directionString)) 		return 135;
		if("SSE".equalsIgnoreCase(directionString)) 	return 157.5;
		if("S".equalsIgnoreCase(directionString)) 		return 0;
		if("SSW".equalsIgnoreCase(directionString))		return -157.5;
		if("SW".equalsIgnoreCase(directionString))		return -135;
		if("WSW".equalsIgnoreCase(directionString))		return -112.5;
		if("W".equalsIgnoreCase(directionString)) 		return -90;
		if("WNW".equalsIgnoreCase(directionString)) 	return -67.5;
		if("NW".equalsIgnoreCase(directionString)) 		return -45;
		if("NNW".equalsIgnoreCase(directionString)) 	return -22.5;
		else return 0;   //if nothing is matched, return 0. 
	}
}
