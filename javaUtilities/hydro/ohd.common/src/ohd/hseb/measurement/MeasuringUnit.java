/*
 * Created on Jul 2, 2003
 *
 * To change the template for this generated file go to
 * 
 */
package ohd.hseb.measurement;

import java.util.*;

/**
 * @author gobsc
 *
 * This is basically an enum of all possible units of measure, but it is also
 * smart enough to help handle unit conversions.
 */
public class MeasuringUnit
{
	
    //conversion factor map
    private static Map _conversionFactorMap = new HashMap();	
 
 
    //conversion standard map
    private static Map _conversionStandardMap= new HashMap();
 
    // Note: the standard unit for a particular MeasuringUnitType, MUST be the first
    //one of that type created here. Otherwise, a circular lookup dependency will be created when
	//when the code tries to set up Maps used to convert first to the standard unit (which wouldn't exist yet)
	// and then to the desired unit.
	// In the private constructor, having a conversion factor of 1.0 is used to identify the unit
	// as the standard of that MeasuringUnitType.
   
   
    //discharge types
    public static final MeasuringUnit cfs = new MeasuringUnit("cfs", MeasuringUnitType.discharge, 1.0);
    public static final MeasuringUnit kcfs = new MeasuringUnit("kcfs", MeasuringUnitType.discharge, 1000.0);
    public static final MeasuringUnit cms = new MeasuringUnit("cms", MeasuringUnitType.discharge, 35.314666572  );
    
    //length types
    public static final MeasuringUnit meters = new MeasuringUnit("meters", MeasuringUnitType.length, 1.0);
    public static final MeasuringUnit cm = new MeasuringUnit("cm", MeasuringUnitType.length, 0.01);
    public static final MeasuringUnit mm = new MeasuringUnit("mm", MeasuringUnitType.length, 0.001);
    public static final MeasuringUnit inches = new MeasuringUnit("inches", MeasuringUnitType.length, 0.0254);
    public static final MeasuringUnit feet = new MeasuringUnit("feet", MeasuringUnitType.length, 0.3048);
	
    //relative time types
    public static final MeasuringUnit hours = new MeasuringUnit("hours", MeasuringUnitType.elapsedTime, 1.0);
    public static final MeasuringUnit days = new MeasuringUnit("days", MeasuringUnitType.elapsedTime, 24.0);
 
    //temperature types
    // This implementation does NOT work right for Fahrenheit to/from Celsius conversion because
    // one needs to add or subtract 32 to convert. Address this later, when needed.
    // Consider adding a larger, unit conversion class
    public static final MeasuringUnit degreesFahrenheit = new MeasuringUnit("degrees F", MeasuringUnitType.temperature, 1.0);
    public static final MeasuringUnit degreesCelsius = 
                      new MeasuringUnit("degrees C", MeasuringUnitType.temperature, 0);
    
    
    //speed types
    public static final MeasuringUnit mph = new MeasuringUnit("mph", MeasuringUnitType.speed, 1.0);
    public static final MeasuringUnit knots = new MeasuringUnit("knots", MeasuringUnitType.speed, 1.150779448 );
    public static final MeasuringUnit kph = new MeasuringUnit("kph", MeasuringUnitType.speed, 0.621371192);

    //direction
    public static final MeasuringUnit degrees = new MeasuringUnit("degrees", MeasuringUnitType.elapsedTime, 1.0);
    
    
    
    //unitless
    public static final MeasuringUnit unitless = 
        new MeasuringUnit("unitless", MeasuringUnitType.unitless, 1.0);

	
   // member variables
   private MeasuringUnitType _type;	
   private String _name;
 
   //note, we simply need to convert to a standard and convert from the standard,
   // so that we don't have (N*N) -N conversions, just 2*N conversions
 
  // --------------------------------------------------------------------
   private MeasuringUnit(String unitName,
           MeasuringUnitType type,
           double conversionFactor)
   {
       _name = unitName;
       _type = type;


       String toStandardKey = null;
       String fromStandardKey = null;

       if (conversionFactor == 1.0) 
       { 
           // this is the standard unit
           _conversionStandardMap.put(type, this);

           toStandardKey = getConversionKey(unitName,unitName);
           fromStandardKey = getConversionKey(unitName, unitName);

           addConversionFactor(toStandardKey, conversionFactor);       	
           addConversionFactor(fromStandardKey, 1.0/conversionFactor);  
       }
       else
       {
           MeasuringUnit standardUnit = getStandardUnitForType(type); 
           toStandardKey = getConversionKey(unitName,standardUnit.getName());
           fromStandardKey = getConversionKey(standardUnit.getName(), unitName);


           addConversionFactor(toStandardKey, conversionFactor);       	
           addConversionFactor(fromStandardKey, 1.0/conversionFactor);  
       }

   } //end constructor


   public String getName()
   {
      return _name;
   }
   
   public MeasuringUnitType getType()
   {
   	  return _type;
   }
   
   public String toString()
   {
       return _name;
   }

   public static double  getConversionFactor(MeasuringUnit fromUnit, 
										  MeasuringUnit toUnit)
   {
   	       MeasuringUnit standardUnit = getStandardUnitForType(fromUnit.getType());
   	       double conversionFactor = 1.0;
   	
   	       if (fromUnit == toUnit)
   	       {
   	       	   conversionFactor = 1.0;       	
   	       }
   	       else
   	       {
   	
		       String conversionKey1 = getConversionKey(fromUnit, standardUnit);
		       Double conversionFactor1 = (Double)_conversionFactorMap.get(conversionKey1);
		   
		       String conversionKey2 = getConversionKey(standardUnit, toUnit);
		       Double conversionFactor2 = (Double)_conversionFactorMap.get(conversionKey2);
			   conversionFactor = conversionFactor1.doubleValue() * 
			                      conversionFactor2.doubleValue();   
			                      
   	       }
   	       
		   return conversionFactor;
   } //end getConversionFactor
   
   protected static MeasuringUnit getStandardUnitForType(MeasuringUnitType type)
   {
        MeasuringUnit unit = (MeasuringUnit) _conversionStandardMap.get(type);
        
        return unit;	
   	
   } //end getStandardUnitForType
   
   protected static MeasuringUnit getStandardUnitForUnit(MeasuringUnit unit)
   {
       	MeasuringUnitType type = unit.getType();
        MeasuringUnit standardUnit = (MeasuringUnit) _conversionStandardMap.get(type);
        
        return standardUnit;	
   	
   } //end getStandardUnitForType


 /*  
   private static void addConversionFactor(MeasuringUnit fromUnit, 
										MeasuringUnit toUnit,
										double conversionFactor)
   {
		String conversionKey = getConversionKey(fromUnit, toUnit);
		addConversionFactor(conversionKey, conversionFactor);
		     
		return;       
   }
 */
   
   private static void addConversionFactor(String conversionKey, double conversionFactor)
   {
       _conversionFactorMap.put(conversionKey, new Double(conversionFactor));	
   	
   	   return;
   }
 
 
   private static String getConversionKey(MeasuringUnit fromUnit, MeasuringUnit toUnit)
   {
	   return getConversionKey(fromUnit.getName(), toUnit.getName());
   }
  
   private static String getConversionKey(String fromUnitName, String toUnitName)
   {  
   		 return fromUnitName + "|" + toUnitName; 
   }

} //end MeasuringUnit
