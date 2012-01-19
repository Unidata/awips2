/*
 * Created on Jul 2, 2003
 *
 * 
 */
package ohd.hseb.measurement;
/**
 * @author gobsc
 *
 * This class is basically an enum for the possible types of
 * MeasuringUnits.  distance, volume, flow
 */
public class MeasuringUnitType
{
	  	
	 public static final MeasuringUnitType length = new MeasuringUnitType("length");
	 public static final MeasuringUnitType discharge = new MeasuringUnitType("discharge");
	 public static final MeasuringUnitType volume = new MeasuringUnitType("volume");
     public static final MeasuringUnitType elapsedTime = new MeasuringUnitType("elapsedTime");
     public static final MeasuringUnitType temperature = new MeasuringUnitType("temperature");
     public static final MeasuringUnitType speed = new MeasuringUnitType("speed");
     public static final MeasuringUnitType direction = new MeasuringUnitType("direction");
     
     public static final MeasuringUnitType unitless = new MeasuringUnitType("unitless");
     
     
	 private String _name = null;

     //private constructor
     private MeasuringUnitType(String typeName)
     {
     	_name = typeName;
     }
     
     public String getName()
     {
     	return _name;
     }
     
     public String toString()
     {
          return _name;	
     
     } //toString
    
}
