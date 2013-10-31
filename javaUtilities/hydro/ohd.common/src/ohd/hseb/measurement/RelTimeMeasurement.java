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
 * This class represents a Measurement at a time relative to some
 * externally defined time.
 */
public class RelTimeMeasurement extends Measurement
{
	private long _relTimeInMillis;  //time after or before another base time, defined elsewhere
  
	//---------------------------------------------------------------
  
    public RelTimeMeasurement(double value, long relTimeInMillis, MeasuringUnit unit)
    {
        super(value, unit);
        _relTimeInMillis = relTimeInMillis;	
    }
    
	//---------------------------------------------------------------
	
	public RelTimeMeasurement(RelTimeMeasurement measurement)
	{
		  super(measurement.getValue(), measurement.getUnit());
		  _relTimeInMillis = measurement.getRelativeTime();	
	}
    
	//---------------------------------------------------------------
    public Measurement getConvertedCopy(MeasuringUnit toUnit)
    {
          
           RelTimeMeasurement newMeasurement = new RelTimeMeasurement(this);    
        
           newMeasurement.convert(toUnit);
        
           return newMeasurement;
    }
   //  ---------------------------------------------------------------
    
	public long getRelativeTime()  
	{
		return _relTimeInMillis;
	}
	
	//---------------------------------------------------------------
	private String formatTime(long time)
	{
	   SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss.SSS");
	   return formatter.format(new Date(time));
	}
	//---------------------------------------------------------------
 
	public String toString()
	{
		return getValue() + " in " + getUnit().getName() + " at " + formatTime(_relTimeInMillis);
	}  
    	
}
