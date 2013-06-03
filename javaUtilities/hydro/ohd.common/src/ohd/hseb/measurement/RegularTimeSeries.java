/*
 * Created on Oct 15, 2003
 *
 * 
 * 
 */
package ohd.hseb.measurement;

import java.text.NumberFormat;
import java.text.*;
import java.util.*;
import ohd.hseb.util.*;
import ohd.hseb.db.*;

/**
 * @author Chip Gobs
 *
 * 
 */
public class RegularTimeSeries
{
    private static final double _missingValue = -999.0;
    private static final double _otherMissingValue = -9999.0;
    private static final long _millisPerHour = 1000 * 60 * 60;  
    
    private static final String _missingString = "MSG";
    
	private String _name = null;
	private long _startTime = 0;
	private long _endTime = 0;
	private int _intervalInHours = 1;
    private long _shiftedTimeInMillis = 0;
	private long _intervalInMillis = _intervalInHours * _millisPerHour;
	private List _measurementList = new ArrayList();
	private MeasuringUnit _measuringUnit = null;
    
   

// -----------------------------------------------------------------------------
    
	public RegularTimeSeries(RegularTimeSeries originalTimeSeries)
	{

	    this(originalTimeSeries._startTime,
             originalTimeSeries._endTime,
             originalTimeSeries._intervalInHours,
             originalTimeSeries._measuringUnit, -1.0);    

        _shiftedTimeInMillis = originalTimeSeries._shiftedTimeInMillis;
        
        List origMeasurementList = originalTimeSeries._measurementList;
        
        for (int i = 0; i < origMeasurementList.size(); i++)
        {
            Measurement origMeasurement = (Measurement) originalTimeSeries._measurementList.get(i);
            Measurement measurement = new Measurement(origMeasurement);
            
            setMeasurementByIndex(measurement, i);
        }
        
        return;
	}     
    
//   -----------------------------------------------------------------------------

   public RegularTimeSeries(long startTime,
                              long endTime,
                              int intervalInHours,
                              MeasuringUnit unit)
   {
       
       // defaults to initialValue = 0.0;
       this(startTime, endTime, intervalInHours, unit, 0.0);    
       
   }                           
    
//  -----------------------------------------------------------------------------
   
    public RegularTimeSeries(long startTime,
							long endTime,
							int intervalInHours,
							MeasuringUnit unit,
                            double initialValue)
	{
        //System.out.println("RegularTimeSeries c-tor");
		_intervalInHours = intervalInHours;
		_intervalInMillis = _intervalInHours * _millisPerHour;
		
		_startTime = TimeHelper.truncateTimeInMillisToNearestHour(startTime,
		                                                       _intervalInHours);
		                                                       
		_endTime = TimeHelper.truncateTimeInMillisToNearestHour(endTime,
		                                                     _intervalInHours);
                                                             
                                                             
       // System.out.println( "RegularTimeSeries ctor _startTime =  " + DbTimeHelper.getDateTimeStringFromLongTime(_startTime));
       // System.out.println( "RegularTimeSeries ctor _endTime =  " + DbTimeHelper.getDateTimeStringFromLongTime(_endTime));
      
		
		
		_measuringUnit = unit;
		
		long numberOfIntervals = (_endTime - _startTime) / _intervalInMillis;
		numberOfIntervals++; //if endTime == startTime, that is 1 interval
		
		//long currentTime = startTime;
		for (int i = 0; i < numberOfIntervals; i++)
		{	
		    Measurement newMeasurement = new Measurement(initialValue, unit);
			_measurementList.add(i, newMeasurement);
		}
		
	} //end RegularTimeSeries() 

//  ---------------------------------------------------------------------------
    public void shiftStartEndTimeHours(int hoursShift)
    {
        
         long timeDiff = hoursShift * _millisPerHour;
  
        _startTime += timeDiff;
        _endTime += timeDiff;
        
        _shiftedTimeInMillis = timeDiff;
        
        return;
    }
    
//  ---------------------------------------------------------------------------
 
    public static RegularTimeSeries add(RegularTimeSeries ts1, RegularTimeSeries ts2)
    {
        long startTime1 = ts1.getStartTime();
        long startTime2 = ts2.getStartTime();
       
        long endTime1 = ts1.getEndTime();
        long endTime2 = ts2.getEndTime();
        
        int intervalInHours  = ts1.getIntervalInHours();
        long intervalInMillis = intervalInHours * _millisPerHour;
        
        MeasuringUnit unit = ts1.getMeasuringUnit();
        ts2.convert(unit);
      
        long startTime = Math.min(startTime1, startTime2);
        long endTime = Math.max(endTime1, endTime2);
        
        // fit the 2 time series to each other
        ts1.stretchTimeSeries(startTime, endTime, 0);
        ts2.stretchTimeSeries(startTime, endTime, 0);
    
       
        //create the new time series
        RegularTimeSeries newTs = new RegularTimeSeries(startTime, endTime,
                                                         intervalInHours,
                                                         unit);   
                
                
                                                         
        for (long time = startTime; time <= endTime; time += intervalInMillis)
        {
            Measurement m1 = ts1.getMeasurementByTime(time);
            Measurement m2 = ts2.getMeasurementByTime(time);
            
            double value1 = m1.getValue();
            double value2 = m2.getValue();
            
            if ( (! m1.isMissing()) && (!m2.isMissing()) )
            {
                double value = value1 + value2;
                Measurement newMeasurement = new Measurement (value, unit);
                newTs.setMeasurementByTime(newMeasurement, time);
            }
            else
            {
                Measurement newMeasurement = new Measurement (0.0, unit);
                newMeasurement.setIsMissing(true);
                newTs.setMeasurementByTime(newMeasurement, time);    
            }
        }                                                  
                                                            
        return newTs;
    }
//  ---------------------------------------------------------------------------
    public RegularTimeSeries copy()
    {
        RegularTimeSeries newTs = 
             new RegularTimeSeries(this.getStartTime(),
                                   this.getEndTime(),
                                   this.getIntervalInHours(),
                                   this.getMeasuringUnit());
                                   
        for (int i=0; i < this._measurementList.size(); i++)
        {
             Measurement m = (Measurement) this._measurementList.get(i);
             newTs.setMeasurementByIndex( m, i);    
            
        }
        
        return newTs;
            
    }
    
//  ---------------------------------------------------------------------------

        
    public void convert(MeasuringUnit newUnit)
    {
        String header = "RegularTimeSeries.convert(): ";
       // System.out.println(header + " ts name = " + _name);
      //  trace();
        
        List newMeasurementList = new ArrayList();
        
        if (_measuringUnit != newUnit)
        {
            for (int i = 0; i < _measurementList.size(); i++)
            {
                Measurement origMeasurement = (Measurement) _measurementList.get(i); 
                Measurement convertedMeasurement = 
                            Measurement.getConvertedCopy(origMeasurement, newUnit); 
                            
                newMeasurementList.add(convertedMeasurement);             
              
            }    
            
            _measuringUnit = newUnit;
            _measurementList = newMeasurementList;
        }    
        
    }
//  -----------------------------------------------------------------------------
    public void stretchTimeSeries(long newStartTime, long endStartTime, double fillInValue)
    {
        prependTimeSeries(newStartTime, fillInValue);
        extendTimeSeries(endStartTime, fillInValue);    
        
        return;
    }

//  -----------------------------------------------------------------------------
    public void trimTimeSeriesAtStart(long newStartTime)
    {
        // round off the newEndTime to the nearest hour
       // if it is not already rounded, then add an hour to the rounded result
       long newRoundedStartTime = TimeHelper.truncateTimeInMillisToNearestHour(newStartTime, 1);
    
       if (newRoundedStartTime != newStartTime)
       {
           newRoundedStartTime += _millisPerHour;
       }
    
    
       // remove measurements from the beginning of the list
       if (newRoundedStartTime > _startTime)
       {
           long hoursDiff = (newRoundedStartTime - _startTime) / _millisPerHour;
           for (int i = 0; i < hoursDiff; i++)
           {   
               //remove from the beginning of the list
               _measurementList.remove(0);
           }   
            
           _startTime = newRoundedStartTime;
       }   
       
       return;
    }
    
//  -----------------------------------------------------------------------------
    public void prependTimeSeries(long newStartTime)
    {
        prependTimeSeries(newStartTime, 0.0);
           
    }

    public void prependTimeSeries(long newStartTime, double fillInValue)
    {
        // round off the newEndTime to the nearest hour
        // if it is not already rounded, then add an hour to the rounded result
        long roundedStartTime = TimeHelper.truncateTimeInMillisToNearestHour(newStartTime, 1);
    
        if (roundedStartTime != newStartTime)
        {
            roundedStartTime += _millisPerHour;
        }
    
    
        // add measurements to the beginning of the list
        if (roundedStartTime < _startTime)
        {
            long hoursDiff = (_startTime - roundedStartTime) / _millisPerHour;
            for (int i = 0; i < hoursDiff; i++)
            {   
                Measurement newMeasurement = new Measurement(fillInValue, _measuringUnit);
        
                //add to the beginning of the list
                _measurementList.add(0, newMeasurement);
            }   
            
            _startTime = roundedStartTime;
        }   
       
   } //end prependTimeSeries()
 
// ---------------------------------------------------------------------------
    public void extendTimeSeries(long newEndTime)
    {
         extendTimeSeries(newEndTime, 0.0);
    }    
   
    public void extendTimeSeries(long newEndTime, double fillInValue)
    {
        
        // round off the newEndTime to the nearest hour
        // if it is not already rounded, then add an hour to the rounded result
        long roundedEndTime = TimeHelper.truncateTimeInMillisToNearestHour(newEndTime, 1);
        
        if (roundedEndTime != newEndTime)
        {
            roundedEndTime+= _millisPerHour;
        }
        
    
        // add measurements to the end of the list
        if (roundedEndTime > _endTime)
        {
            long hoursDiff = (roundedEndTime - _endTime) / _millisPerHour;
            for (int i = 0; i < hoursDiff; i++)
            {   
                Measurement newMeasurement = new Measurement(fillInValue, _measuringUnit);
                
                //add to the end of the list
                _measurementList.add(newMeasurement);
            }   
            
            _endTime = roundedEndTime;
        }   
        
      
        
    } //end extendTimeSeries()
//	---------------------------------------------------------------------------
	public void setName(String name)
	{
		_name = name;
	}
//	---------------------------------------------------------------------------

	public String getName()
	{
		return _name;
	}
//	---------------------------------------------------------------------------
 
	public int getMeasurementCount()
	{
		return _measurementList.size();
	}

//	--------------------------------------------------------    
	
	public Measurement getMeasurementByIndex(int index)
	{
       /* 
        if (index >= _measurementList.size())
        {
            
        }
       */
        
	    return (Measurement)_measurementList.get(index);
	}
//  -------------------------------------------------------- 
    
    public double getMeasurementValueByIndex(int index, MeasuringUnit unit)
    {
        double value = _missingValue;
        
        Measurement m = getMeasurementByIndex(index);
        if (m != null)
        {
            m = m.getCopy(unit);
            value = m.getValue();
        }
        else
        {
            System.err.println("RegularTimeSeries.getMeasurementValueByIndex(): no measurement for index = " + index);   
        }
        
        return value;
    }   
        
//	-------------------------------------------------------- 
    public double getMeasurementValueByTime(long time, MeasuringUnit unit)
    {
        double value = _missingValue;
        
        Measurement m = getMeasurementByTime(time);
       
        if (m != null)
        {
            m = m.getCopy(unit);
            value = m.getValue();    
        }
        else
        {
            System.err.println("RegularTimeSeries.getMeasurementValueByTime(): no measurement for time = " + 
                    DbTimeHelper.getDateTimeStringFromLongTime(time));     
        }
        return value;
    }
        
//  -------------------------------------------------------- 
   
    public long getMeasurementTimeByIndex(int index)
    {
        long newTime = _startTime + (index * _intervalInMillis);
        
        return newTime;	
    }
    
//	-------------------------------------------------------- 
	
	private int getMeasurementIndexByTime(long time)
	{
		int index = -1;
		
		String header = "RegularTimeSeries.getMeasurementIndexByTime()";
		
		
		//System.out.println(header + "time 1 = " + time);
		
        //System.out.println(header + "orig time = " + DbTimeHelper.getDateTimeStringFromLongTime(time));
        
		time = TimeHelper.truncateTimeInMillisToNearestHour(time, _intervalInHours) + _shiftedTimeInMillis;	
		
		//System.out.println(header + "adjusted time = " + DbTimeHelper.getDateTimeStringFromLongTime(time));
		
		//System.out.println(header + "startTime = " + DbTimeHelper.getDateTimeStringFromLongTime(_startTime));
		//System.out.println(header + "endTime = " + DbTimeHelper.getDateTimeStringFromLongTime(_endTime));
	//	
		if ( (time >= _startTime) &&
		     (time <= _endTime) )
		{
			long intervalsDifference = (time - _startTime) / _intervalInMillis;
			index = (int) intervalsDifference;
            
            /*
            if (index >= _measurementList.size())
            {
                System.out.println("Problem is here");
                System.out.println("Time Series name = " + getName());    
            }
            */
			
		}
		
		//System.out.println(header + "index = " + index);
		
		return index;
	}

//	-------------------------------------------------------- 
   
	private void setMeasurementByIndex(Measurement measurement, int index)
	{
        Measurement newMeasurement = 
                     Measurement.getConvertedCopy(measurement,
                                                  _measuringUnit);
		_measurementList.remove(index);
		_measurementList.add(index, newMeasurement);
	    
		return;
	}

//	--------------------------------------------------------
    
	public void setMeasurementByTime(Measurement measurement, long time)
	{
        
        if (time > _endTime)
        {
           // System.out.println("RegularTimeSeries.setMeasurementByTime(): Extending the timeSeries");
            extendTimeSeries(time);   
        }
        else if (time < _startTime)
        {
           // System.out.println("RegularTimeSeries.setMeasurementByTime(): Prepending the timeSeries");   
            prependTimeSeries(time);
        }
        
		int index = getMeasurementIndexByTime(time);
		
	//	System.out.println("RegularTimeSeries.setMeasurementByTime(): time = " + 
	//						DbTimeHelper.getDateTimeStringFromLongTime(time) + " index = " + index);
		
		if (index != -1)
		{
		    setMeasurementByIndex(measurement, index);	
		}
		
		return;
	} 
//  -------------------------------------------------------- 
	/*
    private void setMeasurementValueByIndex(double value, int index)
    {
     //   System.out.println(" ** RegularTimeSeries.setMeasurementValueByIndex()");
        
        Measurement measurement = getMeasurementByIndex(index);
        measurement.setValue(value);        
    }
    */
//  -------------------------------------------------------- 
	/*
    private void setMeasurementValueByTime(double value, long time)
    {
       // System.out.println(" ** RegularTimeSeries.setMeasurementValueByTime()");
        Measurement measurement = getMeasurementByTime(time);
        measurement.setValue(value);        
    }
    */  
//	-------------------------------------------------------- 
 
	public Measurement getMeasurementByTime(long time)
	{
	   Measurement measurement = null;
	   int index = -1;
	  
	   index = getMeasurementIndexByTime(time);
	   
	   if (index != -1)
	   {
	   	    measurement = getMeasurementByIndex(index);
	   }
	        
	   return measurement;
	}
	 
	public AbsTimeMeasurement getAbsTimeMeasurementByTime(long time)
    {
        Measurement measurement = null;
        AbsTimeMeasurement absTimeMeasurement = null;
        
       
        measurement = getMeasurementByTime(time);
        if (measurement != null)
        {
            absTimeMeasurement = new AbsTimeMeasurement(measurement, time);
        }

        return absTimeMeasurement;
    }

//	-------------------------------------------------------- 

	public AbsTimeMeasurement getAbsTimeMeasurementByIndex(int index)
	{
        Measurement measurement =  (Measurement)_measurementList.get(index);
        long time = getMeasurementTimeByIndex(index);
        
        AbsTimeMeasurement absTimeMeasurement = new AbsTimeMeasurement(measurement, time);
        
        return absTimeMeasurement;
        
	}
	
//	-------------------------------------------------------- 
	
	public RegularTimeSeries getSubTimeSeries(long startTime, long endTime)
	{
	    /**
	     * Return the time series that is within this time window, inclusively. 
	     */
	    RegularTimeSeries newTs = null;
	 
	    newTs = new RegularTimeSeries(startTime,
	            					  endTime,
	            					  this.getIntervalInHours(),
	            					  this.getMeasuringUnit() );
	    
	    boolean firstTime = true;
	    for (int i= 0; i < this.getMeasurementCount(); i++)
	    {
	        AbsTimeMeasurement m = this.getAbsTimeMeasurementByIndex(i);
	        
	        //nulls are not possible, so don't check for them
	        
	        long mTime = m.getTime();
	        if ( (mTime >= startTime) && (mTime <= endTime) )
	        {
	            if (firstTime)
	            {
	                newTs = new RegularTimeSeries(mTime,
      					  mTime,
      					  this.getIntervalInHours(),
      					  this.getMeasuringUnit() );  
	                firstTime = false;
	                
	            }
	            newTs.setMeasurementByTime(m, mTime);
	        }
	    }
	    	    
	    return newTs;
	    
	}
// --------------------------------------------------------    
/*	
	private void setStartTime(long startTime)
	{
	    _startTime = startTime;
	}
*/	
//----------------------------------------------------------

	public long getStartTime()
	{
	    return _startTime;
	}
//--------------------------------------------------------
/*
	private void setEndTime(long endTime)
	{
	    _endTime = endTime;
	}
*/
//--------------------------------------------------------

	public long getEndTime()
	{
	    return _endTime;
	}

//	--------------------------------------------------------


	public MeasuringUnit getMeasuringUnit()
	{
	    return _measuringUnit;
	}
	
//	--------------------------------------------------------

/*
	public Measurement getMinMeasurement()
	{
	    Measurement measurement = null;
		Measurement minMeasurement = null;
   
		if (_measurementList.size() > 0)
		{
		    minMeasurement = (Measurement) _measurementList.get(0);
		}
    
		for (int i = 1; i < _measurementList.size(); i++)
		{
		    measurement = (Measurement) _measurementList.get(i);
	     
			if (measurement.getValue() < minMeasurement.getValue())
			{
			    minMeasurement = measurement;
			}
		}
    
        if (minMeasurement != null)
        {
            minMeasurement = new Measurement(minMeasurement);
        }
        
		return minMeasurement;	
	}
	
//---------------------------------------------------------------------
  
	public Measurement getMaxMeasurement()
	{
        String header = "RegularTimeSeries.getMaxMeasurement(): " +
                        "where name = " + _name;
      
       // System.out.println(header + "current time series is " + this._name);
        
        
		Measurement measurement = null;
		Measurement maxMeasurement = null;

        MeasuringUnit checkingUnit = this.getMeasuringUnit();

		double value = 0;

		if (_measurementList.size() > 0)
		{
			maxMeasurement = (Measurement) _measurementList.get(0);
		}

        
		for (int i = 1; i < _measurementList.size(); i++)
		{
           
            
			measurement = (Measurement) _measurementList.get(i);
			value = measurement.getValue();

            if   (! measurement.getUnit().getName().equals(checkingUnit.getName()))
                 
            {
               System.out.println(header + " ******* HEY, the expected unit was " + checkingUnit +
                               " and the received unit was " + measurement.getUnit() + " and the value was " + value);      
            }
        

			if (value > maxMeasurement.getValue())
			{
                //System.out.println(header + "value for maxMeasurement so far = " + value);
				maxMeasurement = measurement;
			}
		} //end for
        
        if (maxMeasurement != null)
        {
            maxMeasurement = new Measurement(maxMeasurement);
        }
        
	    return maxMeasurement;	
    }
 */   
    
//---------------------------------------------------------------------

	public AbsTimeMeasurement getMinMeasurement(long startTime, long endTime)
	{
	    AbsTimeMeasurement measurement = null;

	    AbsTimeMeasurement minMeasurement = null;

	    for (int i = 0; i < _measurementList.size(); i++)
	    {
	        measurement = getAbsTimeMeasurementByIndex(i);
	        long time = measurement.getTime();

	        if ( (time >= startTime) && (time <= endTime) )
	        {
	            if ( (measurement.getValue() != _missingValue)  &&
	                    (measurement.getValue() != _otherMissingValue) &&

	                    ( (minMeasurement == null)  ||
	                            (measurement.getValue() < minMeasurement.getValue())
	                    )
	            )
	            {
	                minMeasurement = measurement;
	            }
	        }
	        else if (time > endTime)
	        {
	            break;    
	        }
	    } //end for i

	    // System.out.println("RegularTimeSeries.getMinMeasurement(): result = " + minMeasurement); 

	    return minMeasurement;    

	}

	//  ---------------------------------------------------------------------

	public AbsTimeMeasurement getMinMeasurement()
	{

	    return getMinMeasurement(getStartTime(), getEndTime());

	}     

	//---------------------------------------------------------------------
	public AbsTimeMeasurement getMaxMeasurement(long startTime, long endTime)
	{
	    AbsTimeMeasurement measurement = null;
	    AbsTimeMeasurement maxMeasurement = null;

	    double value = 0;

	    for (int i = 0; i < _measurementList.size(); i++)
	    {
	        measurement = getAbsTimeMeasurementByIndex(i);
	        long time = measurement.getTime();

	        if ( (time >= startTime) && (time <= endTime) )
	        { 

	            if ( (measurement.getValue() != _missingValue)  &&
	                    (measurement.getValue() != _otherMissingValue) &&


	                    ( (maxMeasurement == null)  ||
	                            (measurement.getValue() > maxMeasurement.getValue())
	                    )
	            )
	            {
	                maxMeasurement = measurement;
	            }
	        }
	        else if (time > endTime)
	        {
	            break;
	        }
	    } //end for

	    // System.out.println("RegularTimeSeries.getMaxMeasurement(): result = " + maxMeasurement); 


	    return maxMeasurement;   
	}

	//---------------------------------------------------------------------

	public AbsTimeMeasurement getMaxMeasurement()
	{ 
	    return getMaxMeasurement(getStartTime(), getEndTime());
	}

	//---------------------------------------------------------------------

	public static  RegularTimeSeries concatenate( RegularTimeSeries ts1,
												  RegularTimeSeries ts2)
	{
		String header = "RegularTimeSeries.concatenate(): ";
		
		long startTime = Math.min(ts1.getStartTime(), ts2.getStartTime());
		long endTime = Math.max(ts1.getEndTime(), ts2.getEndTime());
	
	
		RegularTimeSeries newTs =
		    new  RegularTimeSeries(startTime, 
		                          endTime,
		                          ts1.getIntervalInHours(),
		                          ts1.getMeasuringUnit());
		                                                                 
		long intervalInMillis = ts1.getIntervalInHours() * _millisPerHour;                                    
		
		// note: the ts1 timeseries takes precedence during overlapping periods
		
		// insert the ts2 data
		for (int i = 0; i < ts2.getMeasurementCount(); i++)
		{
		    Measurement measurement = ts2.getMeasurementByIndex(i);
		    long time = ts2.getMeasurementTimeByIndex(i);


		//    System.out.println(header + "ts2 " + " time = " + DbTimeHelper.getDateTimeStringFromLongTime(time) + " value = " + measurement.getValue());
		    newTs.setMeasurementByTime(measurement, time);		
		} //end for


		// insert the ts1 data
		for (int i = 0; i < ts1.getMeasurementCount(); i++)
		{
		    Measurement measurement = ts1.getMeasurementByIndex(i);
            
		    long time = ts1.getMeasurementTimeByIndex(i);

		//    System.out.println(header + "ts1 " + " time = " + DbTimeHelper.getDateTimeStringFromLongTime(time) + " value = " + measurement.getValue() );
		    newTs.setMeasurementByTime(measurement, time);	
		} //end for 


		return newTs;
	}

//	--------------------------------------------------------

	public void setIntervalInHours(int intervalInHours)
	{
	    _intervalInHours = intervalInHours;
	}
	
	//--------------------------------------------------------
	
	public int getIntervalInHours()
	{
	    return _intervalInHours;
	}
		
// --------------------------------------------------------

   public String toString()
   {
		StringBuffer buffer = new StringBuffer();
			
   		if (_name != null)
   		{
   			buffer.append("Name:  " + getName());	
   		}
        
        buffer.append(" Units: " + _measuringUnit + " ");
        buffer.append(" Measurement Count = " + getMeasurementCount());
 
 		buffer.append(" Start Time = " + DbTimeHelper.getDateTimeStringFromLongTime(_startTime));
 		buffer.append(" End Time = " + DbTimeHelper.getDateTimeStringFromLongTime(_endTime));
        
        buffer.append(" ");				
		 
        NumberFormat f = new DecimalFormat("##0.00"); 
 
		for (int i = 0; i < _measurementList.size(); i++)
		{
            Measurement measurement = getMeasurementByIndex(i);
            
            //long time =  getMeasurementTimeByIndex(i);
            //String timeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
                           
            if (! measurement.isMissing() )
            {
                double value = measurement.getValue();     
    			String valueString = f.format(value);
                
               // buffer.append(timeString + "->");
                buffer.append(valueString + " "); 
            }
            else // it is missing
            {
              //  buffer.append(timeString + "->");
                buffer.append(_missingString + " ");
            }
		}
		return buffer.toString();
		
   }
   

// --------------------------------------------------------
 
   private static void originalTests()
   {
       final long millisPerHour = 1000 * 60 * 60;
        int hoursCount = 6;
        int hoursPerDay = 24;
        long startTime = System.currentTimeMillis();
        startTime = TimeHelper.truncateTimeInMillisToNearestHour(startTime, 1);
        
        long endTime = startTime + (millisPerHour * hoursCount);
        long shift = millisPerHour * hoursCount;
    
        int hoursPerInterval = 1;
    
        RegularTimeSeries ts1 = new RegularTimeSeries(startTime, endTime,
                                             hoursPerInterval, MeasuringUnit.mm);
        
        
        long startTime2 = endTime - (3 * millisPerHour);
        long endTime2 = startTime2 + (millisPerHour * hoursCount);
        RegularTimeSeries ts2 = new RegularTimeSeries(startTime2, endTime2,
                                                      hoursPerInterval,
                                                      MeasuringUnit.mm);
    
    
        long m1Time = startTime;
        long m2Time = startTime2;
        for (int i = 0; i <= hoursCount; i++)
        {
            Measurement measurement = new Measurement(13.0 + i, MeasuringUnit.mm);
            ts1.setMeasurementByTime(measurement, m1Time);

            Measurement measurement2 = new Measurement(47.0 + i, MeasuringUnit.mm);
            ts2.setMeasurementByTime(measurement2, m2Time);
            
            m1Time += millisPerHour;
            m2Time += millisPerHour;
        }
        
       System.out.println("ts1 = " + ts1);
       System.out.println("ts2 = " + ts2);
       
       

       // test out sub time series
        RegularTimeSeries subTimeSeries = ts1.getSubTimeSeries(startTime2, endTime2);
        
        System.out.println("new start time = " + DbTimeHelper.getDateTimeStringFromLongTime(startTime2) +
                " new end time = " + DbTimeHelper.getDateTimeStringFromLongTime(endTime2));
        System.out.println("subTimeSeries = " + subTimeSeries);
        
    
       
       
       // test out adding measurements that occur before and after the way the time series
       // is currently set up
       Measurement measurement = new Measurement(999.0, MeasuringUnit.mm);
       ts1.setMeasurementByTime(measurement, ts1.getStartTime() - millisPerHour);
       
       Measurement measurement2 = new Measurement(1000.0, MeasuringUnit.mm);
       ts1.setMeasurementByTime(measurement2, ts1.getEndTime() + millisPerHour);
    
        System.out.println("altered ts1 = " + ts1);
   
    
       // test out adding measurements before and after the current start and end times
       // of a RegularTimeSeries
       
       long newStartTime =  ts1.getStartTime()- (5 * millisPerHour);
       long newEndTime = ts1.getEndTime() + (5 * millisPerHour);
       
       ts1.stretchTimeSeries(newStartTime, newEndTime, -99999);

       System.out.println("stretched ts1 = " + ts1);
       
 
       newStartTime += (10 * millisPerHour);     
       ts1.trimTimeSeriesAtStart(newStartTime);
       System.out.println("trimmed ts1 = " + ts1);
    
    
    /*  // index testing section
    int index = ts1.getMeasurementIndexByTime(startTime);
        long time = ts1.getMeasurementTimeByIndex(index);
        
        int index2 = ts1.getMeasurementIndexByTime(time);
        long time2 = ts1.getMeasurementTimeByIndex(index2);
        System.out.println("ts1 index = " + index);
        System.out.println("ts1 time = " + time);
        System.out.println("ts1 index = " + index2);
        System.out.println("ts1 time2 = " + time2);
    */
       
      
    
        RegularTimeSeries concatTs = RegularTimeSeries.concatenate(ts1, ts2);
        
        System.out.println("concat Ts = " + concatTs);
   }
// --------------------------------------------------------
    private static void testShifting()
    {
        String header = "RegularTimeSeries.testShifting(): ";
        final long MILLIS_PER_HOUR = _millisPerHour;
        
        int shiftInHours = 12;
        
        int intervalInHours = 24;
        long intervalInMillis = intervalInHours * MILLIS_PER_HOUR;
        
        long startTime = System.currentTimeMillis();
        startTime = TimeHelper.truncateTimeInMillisToNearestHour(startTime, intervalInHours);
        long endTime = startTime +  3 * 24 * _millisPerHour;
        MeasuringUnit unit = MeasuringUnit.inches;
        
       
      
        RegularTimeSeries ts1 = new RegularTimeSeries(startTime, endTime,
                                                     intervalInHours, unit);  
        
        ts1.shiftStartEndTimeHours(shiftInHours);
        startTime = ts1.getStartTime();
             
        StringBuffer inBuffer = new StringBuffer();
        StringBuffer outBuffer = new StringBuffer();
        
        
        //initialize test values
        double value = 0.0;
        String valueTimeString = null;
        
        for (long t = startTime; t <= endTime; t+= intervalInMillis )
        {  
            value += 1.0;
            Measurement m = new Measurement(value, unit);
            ts1.setMeasurementByTime(m, t);
            
            valueTimeString = getValueTimeString(t, value);
            
            inBuffer.append(valueTimeString);
            
            System.out.println(header + "IN " + valueTimeString);
        }
        
        for (long t = startTime; t <= endTime; t+= intervalInMillis)
        {  
            Measurement m = ts1.getMeasurementByTime(t); 
            
            if (m != null)
            {
                value = m.getValue();
            }
            else
            {
                value = _missingValue;
            }
            valueTimeString = getValueTimeString(t, value);
            
            outBuffer.append(valueTimeString);
            
            System.out.println(header + "OUT " + valueTimeString);
        }
        
        //verify that what went in is what comes out
        if (inBuffer.toString().equals(outBuffer.toString()))
        {
            System.out.println(header + "The buffers are the same. ");        
        }
        else
        {
            System.out.println(header + "The buffers are different. ");        
        }
     
    }
//  --------------------------------------------------------
    static String getValueTimeString(long time, double value)
    {  
        return "Value = " + value + " time = " +
                 DbTimeHelper.getDateTimeStringFromLongTime(time);
    }
//  --------------------------------------------------------
    
    public static void main(String[] args)
    {
            
         //originalTests();
         
         testShifting();
         
    }
    
// --------------------------------------------------------

/*  
   private void trace()
   {
       try
       {
           throw new Exception("trace");
       }
       catch(Exception e)
       {
           e.printStackTrace();
       }    
       
   }
*/
// --------------------------------------------------------


} //end  RegularTimeSeries
