package gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion;


import gov.noaa.nws.ncep.edex.common.metparameters.quantity.AmountOfPrecipitation;
import gov.noaa.nws.ncep.edex.common.metparameters.quantity.HeatFlux;
import gov.noaa.nws.ncep.edex.common.metparameters.quantity.PotentialForCyclonicUpdraftRotation;
import gov.noaa.nws.ncep.edex.common.metparameters.quantity.RateOfChangeInPressureWithTime;
import gov.noaa.nws.ncep.edex.common.metparameters.quantity.RateOfChangeInTemperatureWithHeight;
import gov.noaa.nws.ncep.edex.common.metparameters.quantity.RateOfChangeInTemperatureWithPressure;
import gov.noaa.nws.ncep.edex.common.metparameters.quantity.TemperatureTendency;

import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.unit.NonSI;
import javax.measure.unit.ProductUnit;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
/**
 * @author archana
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcUnits implements ISerializableObject{
	/**
	 * No-arguments constructor
	 */
	public NcUnits(){
		
	}
	
	@DynamicSerializeElement
	public static final Unit<AmountOfPrecipitation> KG_PER_METER_SQ  = new ProductUnit<AmountOfPrecipitation>(SI.KILOGRAM.divide(( SI.METER.pow( 2 ) )) ) ;
	
	@DynamicSerializeElement
	public static final Unit<?> GRAMS_PER_KILOGRAM           = SI.GRAM.divide(SI.KILOGRAM) ; 
	
	@DynamicSerializeElement
	public static final Unit<Length> HUNDREDS_OF_FEET       =  NonSI.FOOT.times( 100 );
	
	@DynamicSerializeElement
	public static Unit<?> INCHES_PER_THREE_HOURS            = NonSI.INCH.divide(NonSI.HOUR.times( 3 ) );
	
	@DynamicSerializeElement
	public static Unit<?> INCHES_PER_HOUR            = NonSI.INCH.divide(NonSI.HOUR );

	@DynamicSerializeElement
	public static Unit<PotentialForCyclonicUpdraftRotation> METER_SQUARE_PER_SECOND_SQUARE = 
		new ProductUnit<PotentialForCyclonicUpdraftRotation> ( ( SI.METER.pow( 2 ) ).divide( ( SI.SECOND.pow( 2 ) ) ));
	
	@DynamicSerializeElement
	public static final Unit<?> JOULES_PER_KILOGRAM           = SI.JOULE.divide(SI.KILOGRAM) ;
	//public static final Unit<VolumetricDensity> KILOGRAM_PER_METER_CUBE = VolumetricDensity.UNIT;
	
	@DynamicSerializeElement
	public static final Unit<Pressure> MILLIBAR         = SI.HECTO( SI.PASCAL );
	
	@DynamicSerializeElement
	public static final Unit<?> FREQUENCY_SQUARED       = SI.HERTZ.times(SI.HERTZ); 

	
	//TODO Rename CELSIUS_PER_KILOMETER to TEMPERATURE_PER_UNIT_HEIGHT ??
	@DynamicSerializeElement
	public static final Unit<RateOfChangeInTemperatureWithHeight> CELSIUS_PER_KILOMETER = 
		new ProductUnit<RateOfChangeInTemperatureWithHeight>( SI.CELSIUS.divide( SI.KILOMETER ) );
	
	@DynamicSerializeElement
	public static final Unit<RateOfChangeInTemperatureWithPressure> KELVIN_PER_MILLIBAR = 
		new ProductUnit<RateOfChangeInTemperatureWithPressure>( SI.KELVIN.divide( NcUnits.MILLIBAR ) );
    
	@DynamicSerializeElement
	public static final Unit<RateOfChangeInPressureWithTime> PASCALS_PER_SEC = 
		new ProductUnit<RateOfChangeInPressureWithTime>( SI.PASCAL.divide( SI.SECOND ) );
	
	@DynamicSerializeElement
	public static final Unit<TemperatureTendency> KELVIN_PER_DAY = 
		new ProductUnit<TemperatureTendency>( SI.KELVIN.divide( NonSI.DAY ) );

	@DynamicSerializeElement
	public static final Unit<HeatFlux> WATT_PER_SECOND_SQUARE 
	= new ProductUnit<HeatFlux>(SI.WATT.divide(SI.SECOND.times( 2 ) ) );
	
	public static void register(){
		if( UnitFormat.getUCUMInstance().isValidIdentifier( "100*inHg") ) {
			System.out.println("100*inHg is invalid");
		}

		UnitFormat.getUCUMInstance().label( NonSI.INCH_OF_MERCURY.divide(100), "hecto_inHg" );
		UnitFormat.getUCUMInstance().label( HUNDREDS_OF_FEET, "hecto_ft" );

		UnitFormat.getUCUMInstance().label( MILLIBAR, "mb" );
		
		UnitFormat.getUCUMInstance().label( NonSI.FAHRENHEIT, "Fahrenheit" );
		UnitFormat.getUCUMInstance().label( NonSI.INCH_OF_MERCURY.divide(100), "hecto_inHg" );
		UnitFormat.getUCUMInstance().label( HUNDREDS_OF_FEET, "hecto_ft" );
	    UnitFormat.getUCUMInstance().label( NonSI.DEGREE_ANGLE, "degree" );
//		UnitFormat.getUCUMInstance().label( GRAMS_PER_KILOGRAM, "g/kg" );
	    UnitFormat.getUCUMInstance().label( PASCALS_PER_SEC, "pascals_per_sec" );
	    UnitFormat.getUCUMInstance().label( KELVIN_PER_DAY, "kelvin_per_day" );
		UnitFormat.getUCUMInstance().label( JOULES_PER_KILOGRAM, "joules_per_kg" );
		UnitFormat.getUCUMInstance().label( INCHES_PER_HOUR, "inches_per_hour" );
		UnitFormat.getUCUMInstance().label( KG_PER_METER_SQ, "kg_per_meter_squared" );
//		UnitFormat.getUCUMInstance().label( CELSIUS_PER_KILOMETER, "℃/km" );
//		UnitFormat.getUCUMInstance().label( KELVIN_PER_MILLIBAR, "K/mb" );
		
//      UnitFormat.getUCUMInstance().label( KILOGRAM_PER_METER_CUBE, "kg/m³" );
//		UnitFormat.getUCUMInstance().label( INCHES_PER_THREE_HOURS, "in/hr*3" );
//		UnitFormat.getUCUMInstance().label( FREQUENCY_SQUARED, "Hz^2" ); //not sure if this will parse...
		
		// aliases to make editing the plotParameter.xml files easier (ie no non-ascii chars)

	    

	}
}