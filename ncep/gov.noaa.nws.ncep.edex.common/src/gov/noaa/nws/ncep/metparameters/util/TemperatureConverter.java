package gov.noaa.nws.ncep.metparameters.util;

public class TemperatureConverter {
	public static double convertToCelsiusFromFahrenheit(double degreeInF) {
		double degreeInC = (degreeInF - 32)*5/9; 
		return degreeInC; 
	}
	
	public static double convertToFahrenheitFromKelvin(double degreeInK) {
		double degreeInF = (degreeInK - 273.15)*9/5 + 32; 
		return degreeInF; 
	}

	public static double convertToKelvinFromFahrenheit(double degreeInF) {
		double degreeInK = (degreeInF - 32)*5/9 + 273.15; 
		return degreeInK; 
	}

	public static double convertToKelvinFromCelsius(double degreeInC) {
		double degreeInK = degreeInC + 273.15; 
		return degreeInK; 
	}

	public static double convertToFahrenheitFromCelsius(double degreeInC) {
		double degreeInF = (degreeInC *9) / 5 + 32; 
		return degreeInF; 
	}

}
