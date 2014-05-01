/*
 * Created on Aug 25, 2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package ohd.hseb.measurement;

import java.text.*;
import java.util.*;

/**
 * @author GobsC
 *
 * 
 */
public class AbsTimeMeasurement extends Measurement
{
	private long _time; // the time the measurement was taken
  
  // *****************************************************************************
    public AbsTimeMeasurement(Measurement measurement,  long timeInMillis)
    {
         super(measurement);
         _time = timeInMillis;
        
         return;
    }
    
//  *****************************************************************************
  
    public AbsTimeMeasurement(double value, long timeInMillis, MeasuringUnit unit)
    {
    	super(value, unit);
        _time = timeInMillis;
        
        return;
    }
    
	// *****************************************************************************
  
	public AbsTimeMeasurement(AbsTimeMeasurement measurement)
	{
		  super(measurement.getValue(), measurement.getUnit());
		  _time = measurement.getTime();
    
		  return;
	}
	//---------------------------------------------------------------
 
	public long getTime()  
	{
	    return _time;
	}
	
	public void setTime(long timeInMillis)
	{
		_time = timeInMillis;	
	}
	//---------------------------------------------------------------
  
	private String formatTime(long time)
	{
	   
	   SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss.SSS");
	   formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
	   return formatter.format(new Date(time)) + " UTC";
	}
	

	//---------------------------------------------------------------
    public static AbsTimeMeasurement getConvertedCopy(AbsTimeMeasurement origMeasurement,
                                                      MeasuringUnit toUnit)
    {
          
          AbsTimeMeasurement newMeasurement = new AbsTimeMeasurement(origMeasurement);    
        
          newMeasurement.convert(toUnit);
        
          return newMeasurement;
    }
    
//  ---------------------------------------------------------------
 	
	public static AbsTimeMeasurement interpolate(AbsTimeMeasurement m1,
	AbsTimeMeasurement m2, 
											 long desiredTime)
	{
		   long t1 = m1.getTime();
		   long t2 = m2.getTime();
    	
		   double value1 = m1.getValue();
		   double value2 = m2.getValue();
    	
		   double slope = (value2 - value1) / (t2 - t1);
		   double intercept = (value1) - (slope*t1);
    	
    	
		   double value = (slope*desiredTime) + intercept; 
		   AbsTimeMeasurement measurement = new AbsTimeMeasurement(value, desiredTime, m1.getUnit());
        
		   measurement.setIsInterpolated(true);	
        	
		   return measurement;
	}

	//---------------------------------------------------------------
	
	
	public boolean equals(Object object)
    {
	    boolean result = false;
	    
	    if (object instanceof AbsTimeMeasurement)
	    {
	        AbsTimeMeasurement m = (AbsTimeMeasurement) object;
	        result = equals(m);
	    }
	    
	    return result;
	    
    }
	
	//---------------------------------------------------------------

	 
    public boolean equals(AbsTimeMeasurement measurement)
      {
          boolean result = false;
       
          if (this.getUnit() != measurement.getUnit())
          {
              measurement = AbsTimeMeasurement.getConvertedCopy(measurement, this.getUnit());
          }
       
          
          if  (
              (this.getValue() == measurement.getValue()) && 
              (this.getTime() == measurement.getTime())
              )
          {
              result = true;    
          }
       
          return result;
       
      }
      
    //  ---------------------------------------------------------------
    public int hashCode()
    {
        int value = 0;
        
        MeasuringUnit standardUnit = MeasuringUnit.getStandardUnitForUnit(this.getUnit()); 
        value = (int) ( this.getTime() +  
                		this.getValue(standardUnit));
        
        return value;
    }
    //  ---------------------------------------------------------------
    
	public String toString()
	{
		return getValue() + " in " + getUnit().getName() + " at " + formatTime(_time);
	}  
}
