/*
 * Created on Sep 26, 2003
 *
 * 
 */
package ohd.hseb.measurement;

import java.text.*;
import java.util.*;

/**
 * @author Chip Gobs
 *
 * This class handles the abstraction of
 * a measurement that is taken over a period of time,
 * such as 1-hour precipitation totals. 
 *
 */
public class TimeIntervalMeasurement extends AbsTimeMeasurement
{

	private static final long MILLIS_PER_HOUR = 1000 * 60 * 60;

  	private int _intervalInHours = -1;
  	
  
  //---------------------------------------------------------------------
  
	public TimeIntervalMeasurement(double value,
								   long endTimeInMillis, 
								   int intervalInHours,
								   MeasuringUnit unit)
	{
		super(value, endTimeInMillis, unit);
		
        setIntervalInHours(intervalInHours);
        
		return;
	}
    
	//---------------------------------------------------------------------
  
	public TimeIntervalMeasurement(TimeIntervalMeasurement measurement)
	{
		  super(measurement.getValue(),
		  		 measurement.getTime(),
		  		   measurement.getUnit());
		 
    	  setIntervalInHours(measurement.getIntervalInHours());
		  return;
	}
	//---------------------------------------------------------------
    public static TimeIntervalMeasurement getConvertedCopy(
                        TimeIntervalMeasurement origMeasurement,
                        MeasuringUnit toUnit)
    {
          TimeIntervalMeasurement newMeasurement = 
                    new TimeIntervalMeasurement(origMeasurement);    
        
          newMeasurement.convert(toUnit);
        
          return newMeasurement;
    }
    //---------------------------------------------------------------
   
    //  ---------------------------------------------------------------
  
	public long getStartTime()  
	{
		return getTime() - (_intervalInHours * MILLIS_PER_HOUR);
	}
	
	//---------------------------------------------------------------
	
	private void setIntervalInHours(int intervalInHours)
	{
		_intervalInHours = intervalInHours;
	}
	
//	---------------------------------------------------------------

	public long getEndTime()  
	{
		return getTime();
	}
	//---------------------------------------------------------------
/*	
	private void setEndTime(long endTime)
	{
		setTime(endTime);
	}
*/	
	//---------------------------------------------------------------
	
	public int getIntervalInHours()  
	{
		return _intervalInHours;
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
		
		String outString = getValue() + " in " + getUnit().getName() + " ending at " + 
							formatTime(getEndTime()) + 
							" over " + _intervalInHours + " hours. ";
		
		return outString;
	}  
	//---------------------------------------------------------------------
 
} //end class TimeIntervalMeasurement


