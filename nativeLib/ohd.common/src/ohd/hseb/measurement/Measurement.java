/*
 * Created on Jul 2, 2003
 *
 *  This class is a fundamental class to be used as the basis
 *  for time series and other measurements.
 */

package ohd.hseb.measurement;

//import java.text.*;
import java.util.*;

/**
 * @author Chip Gobs
 *
 *
 */
public class Measurement
{
    
     
    private MeasuringUnit _unit;
    private double _value;
     
    private boolean _interpolated = false;
    private boolean _extrapolated = false;
    private boolean _defaulted = false;
    
    private boolean _isMissing = false;
    
    private double _missingValue = -999.0;

    //conversion for degrees fahrenheit and celsius
    private static final double _f_to_c_factor = 5.0/9.0;
    private static final double _c_to_f_factor = 9.0/5.0;
  
    
    //consider a separate valid time class
    
   
	//---------------------------------------------------------------
    
    public Measurement(double value, MeasuringUnit unit)
    {
    	_value = value;
		_unit = unit;
        
        _isMissing = false;
		
		if (_unit == null)
		{
			throw new Error("ERROR: Measurement.Measurement() MeasuringUnit may not be null");	
		}
		
    	return;
    }

	//---------------------------------------------------------------
    
    public Measurement(Measurement measurement)
    {
        _value = measurement.getValue();
        _unit = measurement.getUnit();	
        
        _isMissing = measurement.isMissing();
    
    	return;
    }
 
 
   //--------------------------------------------------------------- 
    public boolean equals(Object object)
    {
        boolean result = false;
        
        if (object instanceof Measurement)
        {
            Measurement m = (Measurement) object;
            result = this.equals(m);
        }
        
        return result;

    }
    //--------------------------------------------------------------- 

   public boolean equals(Measurement measurement)
   {
       boolean result = false;
       
       if (this._unit != measurement.getUnit())
       {
           measurement = this.getCopy(this._unit);
       }
       
       if (this.getValue() == measurement.getValue())
       {
           result = true;    
       }
       
       return result;

   }

   //--------------------------------------------------------------- 
    public static Measurement getConvertedCopy(Measurement origMeasurement, MeasuringUnit toUnit)
    {
        Measurement newMeasurement = null;
    
        if (origMeasurement != null)
        {
            newMeasurement = new Measurement(origMeasurement);    
            newMeasurement.convert(toUnit);
        }
        return newMeasurement;
    }
   
    //--------------------------------------------------------------- 
  
    public Measurement getCopy()
    {
        return getCopy(_unit);
    }
    //--------------------------------------------------------------- 
  
    public Measurement getCopy(MeasuringUnit unit)
    {
        return Measurement.getConvertedCopy(this, unit);
    }
    //---------------------------------------------------------------
 
    public MeasuringUnit getUnit()
    {
       return _unit;
    }

	//---------------------------------------------------------------
  
    public double getValue()
    {  
        if (_isMissing)
        {
            _value = _missingValue;    
        }
        
    	return _value;
    }
 
    //---------------------------------------------------------------   
    public double getValue(MeasuringUnit unit)
    {
       double value = _value;
        
       if (_isMissing)
       {
          _value = _missingValue; 
          value = _value;    
       }   
       
       else if (_unit != unit)
       {
           double factor =  MeasuringUnit.getConversionFactor(_unit, unit);
           value = _value * factor;    
       } 
       
       return value;
      
    }

	//---------------------------------------------------------------
	public void setValue(double value)
	{  
	   _value = value;
	}

	//---------------------------------------------------------------
    public void addMeasurement(Measurement measurement)
    {
        measurement.convert(_unit);
        
        this.setValue(this.getValue() + measurement.getValue());
        
    }
	
	//---------------------------------------------------------------
	  
    
    protected void setUnit(MeasuringUnit unit)
    {
       _unit = unit;	
    }
    
  
    //---------------------------------------------------------------
   
    
  //  public void convert(MeasuringUnit toUnit) throws 
  //			MeasuringUnitConversionException
    protected void convert(MeasuringUnit toUnit)
    {
        String header = "Measurement.convert(): ";
        
    	MeasuringUnit fromUnit = this.getUnit();
    	if (fromUnit.getType() == toUnit.getType())
    	{
    		if (fromUnit == toUnit) //do nothing
    		{
    			// no conversion needed
    		}
            else if (fromUnit.getType() == MeasuringUnitType.temperature)
            {
                if (fromUnit.equals(MeasuringUnit.degreesFahrenheit) && 
                        (toUnit.equals(MeasuringUnit.degreesCelsius) ) )
                {
                    _value = fahrenHeightToCelsius(_value);
                    _unit = toUnit;
                }
                else if (fromUnit.equals(MeasuringUnit.degreesCelsius) && 
                        (toUnit.equals(MeasuringUnit.degreesFahrenheit) ) )
                {
                    _value = celsiusToFahrenheit(_value);
                    _unit = toUnit;
                }
            }
    		else // multiplicative conversion
    		{
    		   double factor = MeasuringUnit.getConversionFactor(fromUnit, toUnit);
               
              // System.out.println(header + " from " + _value + " in " + fromUnit + 
              //                      " to " + (_value * factor) + " in " + toUnit );
                
               
    		   _value = _value * factor;
    		   _unit = toUnit;
    		   
    		}
    	}
    	else //can't convert these types
    	{
			//throw new MeasuringUnitConversionException(fromUnit, toUnit);
			throw new Error("Bogus Unit conversion attempt from " +
							 fromUnit.getName() + 
							 " to " + toUnit.getName() );
    	}
    } //end convert

	//---------------------------------------------------------------
    private double fahrenHeightToCelsius(double origValue)
    {
        double newValue = _f_to_c_factor * ( origValue - 32);
               
        return newValue;
    }
    
    private double celsiusToFahrenheit(double origValue)
    {
        double newValue = (_c_to_f_factor *  origValue) + 32;
        
        return newValue;
    }
 
    //---------------------------------------------------------------

  
    public String toString()
	{
         String string = null;
            
         if (! _isMissing)   
         {
             string = _value + " in " + _unit.getName();
         }
         else
         {
             string = "MISSING";
         }
         
         return string; 
    }     	

   
	//---------------------------------------------------------------
  
	public void setIsInterpolated(boolean interpolated) {
		this._interpolated = interpolated;
	}
	//---------------------------------------------------------------
  
	public boolean isInterpolated()
	{
		return _interpolated;
	}

	//---------------------------------------------------------------
  
	public void setIsExtrapolated(boolean extrapolated)
	{
		this._extrapolated = extrapolated;
	}

	//---------------------------------------------------------------
  
	public boolean isExtrapolated()
	{
		return _extrapolated;
	}

	//---------------------------------------------------------------
  
	public void setIsDefaulted(boolean defaulted) 
	{
		this._defaulted = defaulted;
	}

	//---------------------------------------------------------------
  
	public boolean isDefaulted() 
	{
		return _defaulted;
	}
	
    //---------------------------------------------------------------

    public void setIsMissing(boolean isMissing)
    {
        _isMissing = isMissing;
        
        _value = _missingValue;
    }

    //---------------------------------------------------------------
    public boolean isMissing()
    {
        return _isMissing;
    }
    
    //---------------------------------------------------------------

    public int hashCode()
    {
        int value = 0;
        
        MeasuringUnit standardUnit = MeasuringUnit.getStandardUnitForUnit(this.getUnit()); 
        value = (int) ( this.getValue(standardUnit));
        
        return value;
    }
	//---------------------------------------------------------------
  
    public static void main (String[] args) throws Exception
    {
        AbsTimeMeasurement m1 = new AbsTimeMeasurement(3.0, 
                                    new Date().getTime(),
                                    MeasuringUnit.feet);

        System.out.println(m1);

        AbsTimeMeasurement m2 = AbsTimeMeasurement.getConvertedCopy(m1, MeasuringUnit.inches);

        System.out.println(m2);

    } //end main

    
} //end class Measurement
