package gov.noaa.nws.ncep.metParameters.parameterConversion;


import gov.noaa.nws.ncep.metParameters.quantity.RateOfChangeInTemperatureWithHeight;
import gov.noaa.nws.ncep.metParameters.quantity.RateOfChangeInTemperatureWithPressure;

import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.unit.NonSI;
import javax.measure.unit.ProductUnit;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
/**
 * @author archana
 *
 */
public class NcUnits {
	public static final Unit<?> GRAMS_PER_KILOGRAM           = SI.GRAM.divide(SI.KILOGRAM) ; 
	public static final Unit<Length> HUNDREDS_OF_FEET       =  NonSI.FOOT.times( 100 );
	public static Unit<?> INCHES_PER_THREE_HOURS            = NonSI.INCH.divide(NonSI.HOUR.times( 3 ) );
	public static final Unit<?> JOULES_PER_KILOGRAM           = SI.JOULE.divide(SI.KILOGRAM) ;
	//public static final Unit<VolumetricDensity> KILOGRAM_PER_METER_CUBE = VolumetricDensity.UNIT;
	public static final Unit<Pressure> MILLIBAR         = SI.HECTO( SI.PASCAL );
	public static final Unit<?> FREQUENCY_SQUARED       = SI.HERTZ.times(SI.HERTZ); 
	//TODO Rename CELSIUS_PER_KILOMETER to TEMPERATURE_PER_UNIT_HEIGHT ??

	public static final Unit<RateOfChangeInTemperatureWithHeight> CELSIUS_PER_KILOMETER = 
		new ProductUnit<RateOfChangeInTemperatureWithHeight>( SI.CELSIUS.divide( SI.KILOMETER ) );
	public static final Unit<RateOfChangeInTemperatureWithPressure> KELVIN_PER_MILLIBAR = 
		new ProductUnit<RateOfChangeInTemperatureWithPressure>( SI.KELVIN.divide( NcUnits.MILLIBAR ) );

	public static void register(){
		if( UnitFormat.getUCUMInstance().isValidIdentifier( "100*inHg") ) {
			System.out.println("100*inHg is invalid");
		}
		UnitFormat.getUCUMInstance().label( NonSI.INCH_OF_MERCURY.divide(100), "centi_inHg" );
		UnitFormat.getUCUMInstance().label( HUNDREDS_OF_FEET, "hecto_ft" );
//		UnitFormat.getUCUMInstance().label( GRAMS_PER_KILOGRAM, "g/kg" );
//		UnitFormat.getUCUMInstance().label( JOULES_PER_KILOGRAM, "J/kg" );
		UnitFormat.getUCUMInstance().label( MILLIBAR, "mb" );
//		UnitFormat.getUCUMInstance().label( CELSIUS_PER_KILOMETER, "℃/km" );
//		UnitFormat.getUCUMInstance().label( KELVIN_PER_MILLIBAR, "K/mb" );
		
		//                UnitFormat.getUCUMInstance().label( KILOGRAM_PER_METER_CUBE, "kg/m³" );
//		UnitFormat.getUCUMInstance().label( INCHES_PER_THREE_HOURS, "in/hr*3" );
//		UnitFormat.getUCUMInstance().label( FREQUENCY_SQUARED, "Hz^2" ); //not sure if this will parse...
		
		// aliases to make editing the plotParameter.xml files easier (ie no non-ascii chars)
	    UnitFormat.getUCUMInstance().label( NonSI.DEGREE_ANGLE, "degree" );
	    

	}
}