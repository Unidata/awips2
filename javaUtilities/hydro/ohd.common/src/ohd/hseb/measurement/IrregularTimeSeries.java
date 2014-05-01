/*
 * Created on Sep 26, 2003
 *
 * 
 */
package ohd.hseb.measurement;

import java.util.*;
import java.text.*;

import ohd.hseb.model.ForecastAdjuster;
import ohd.hseb.model.ForecastAdjusterParams;
import ohd.hseb.model.ForecastInterpolationMethod;

/**
 * @author Chip Gobs
 *
 * 
 */
public class IrregularTimeSeries
{
	
    private static final double _missingValue = -999.0;
    private static final double _otherMissingValue = -9999.0;
	private List _measurementList = new ArrayList();
	private MeasuringUnit _measuringUnit = null;
	private String _name = null;

	//---------------------------------------------------------------------
   
	//---------------------------------------------------------------------
	
	public IrregularTimeSeries(MeasuringUnit unit)
	{
        _measuringUnit = unit;
	}
	
//	---------------------------------------------------------------------
	
	
	/**
	 * 
	 * @param newTimeSeries
	 * This add method perfoms mathematical addition, 
	 * not insertion to the set of values.  See insertXXX methods for insertion.
	 */
	
    public static IrregularTimeSeries add(IrregularTimeSeries ts1, IrregularTimeSeries ts2)
	{
	     	  //algorithm
     	  
		  //1.  find out all the times for which either time series has a measurement.
		  //   store these times in an array
		  //2. iterate over the list to drive the addition of the 2 time series.
		  //  There is a getMeasurementByTime  method that allows for filling in zeroes
		  // and for interpolation adn for exact Measurement retrieval.
	   	
	    MeasuringUnit unit1 = ts1.getMeasuringUnit();
		MeasuringUnit unit2 = ts2.getMeasuringUnit();
	  
		IrregularTimeSeries combinedTs = new IrregularTimeSeries(unit1);
		IrregularTimeSeries newTs = new IrregularTimeSeries(unit1);
	  
		// make sure that ts2 contains the same units;
		// this might create an unpleasant side effect, so I convert it back at the end
	  
		if (unit1 != unit2)
		{
		    ts2.convert(unit1);
		}
	
	    // insert all of the measurements into the combined time series	  
	    combinedTs.insertTimeSeries(ts1);	
	    // System.out.println("combined with ts1 \n " + combinedTs);

	    combinedTs.insertTimeSeries(ts2);	
	    // System.out.println("combined with ts1 and ts2 \n" + combinedTs);
  	  
		// get the array out of the time series
		AbsTimeMeasurement[] combinedArray = combinedTs.getMeasurementArray();
	  
	  
		//for now, assume that any values out of range for a particular time series
		// = 0.0;
	   
	
		boolean allowInterpolation = true;
		boolean fillInEndsWithZero = true;
		  
		long desiredTime = -1;
		long previousTime = -2;
		  
		for (int i = 0 ; i < combinedArray.length; i++ )
		{
		   AbsTimeMeasurement currentMeasurement = combinedArray[i];
		  	   
		   desiredTime = currentMeasurement.getTime();
		  	
		   if (desiredTime != previousTime) //don't want duplicate entries, so checking
		   {
	  	    
		       AbsTimeMeasurement m1 = ts1.getAbsTimeMeasurementByTime(desiredTime,
															 allowInterpolation, 
															 fillInEndsWithZero);
													 
			   AbsTimeMeasurement m2 = ts2.getAbsTimeMeasurementByTime(desiredTime,
															 allowInterpolation, 
															 fillInEndsWithZero);
			   double value = m1.getValue() + m2.getValue();                                              
			   AbsTimeMeasurement newM = new AbsTimeMeasurement(value, desiredTime, unit1);                                              	
			   newTs.insertMeasurement(newM); 
		   }
	  	   
		   previousTime = desiredTime;
	  	                                                 
	   } //end for i
	 
	  
	   //convert back - might be needed
	   ts2.convert(unit2);
	  
	   return newTs;
	
	} // end add
	
	
	//---------------------------------------------------------------------
    public void convert(MeasuringUnit newUnit)
    {
            String header = "TimeSeries.convert(): ";
          //  System.out.println(header + "is being called for ts = " + _name );
        
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
	
	//---------------------------------------------------------------------
	  
	public long getEndTime()
	{
		long endTime = -1;
		if (_measurementList.size() > 0)
		{
		   endTime = getAbsTimeMeasurementByIndex(_measurementList.size() - 1).getTime();
		}
		return endTime;
	}
	
	//---------------------------------------------------------------------
	public AbsTimeMeasurement findClosestMeasurementByTime(long targetTime)
	{
	    AbsTimeMeasurement curMeasurement = null;
           
	    AbsTimeMeasurement closestMeasurement = null;
	    long smallestTimeDiff = -1;
	    
	    long previousTimeDiff = -1;
	    long timeDiff = 0;
	    
	    boolean done = false;
	    
	    for (int i = 0; !done && i < _measurementList.size(); i++)
	    {
	        curMeasurement = (AbsTimeMeasurement) _measurementList.get(i);
	        
	         timeDiff = Math.abs( curMeasurement.getTime() - targetTime);
	        
	        if ( closestMeasurement == null) //first time
	        {
	            smallestTimeDiff = timeDiff;
	            closestMeasurement = curMeasurement;
	        }
	        else //not first time
	        {
	            if (timeDiff < smallestTimeDiff)
	            {
	                smallestTimeDiff = timeDiff;
	                closestMeasurement = curMeasurement;
	            }
	            else if (timeDiff > previousTimeDiff) //gone too far
	            {
	                done = true;
	            }
	        }
	        
	        previousTimeDiff = timeDiff;
	    	
	    }
	    

	    return closestMeasurement;
	}
	
	//	---------------------------------------------------------------------
    public AbsTimeMeasurement getMinMeasurement(long startTime, long endTime)
    {
        //could keep track of this internally when items are added or deleted from the list, but then it would have to
        // be called everytime something is deleted, or else set a boolean marker to note that something would have to be recalculated in this
        // method the next time it is called
        AbsTimeMeasurement measurement = null;
                 
        AbsTimeMeasurement minMeasurement = null;
    
        //String header = "IrregularTimeSeries.getMinMeasurement(): ";
           
        for (int i = 0; i < _measurementList.size(); i++)
        {     
            measurement = (AbsTimeMeasurement) _measurementList.get(i);
            long time = measurement.getTime();
             
            if ( (time >= startTime) && (time <= endTime) )
            {
                
                if (   (measurement.getValue() != _missingValue)  &&
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
            measurement = (AbsTimeMeasurement) _measurementList.get(i);
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
   
   
        return maxMeasurement;   
     }
  
    //---------------------------------------------------------------------
 
	public AbsTimeMeasurement getMaxMeasurement()
	{ 
	   return getMaxMeasurement(getStartTime(), getEndTime());
	}

	//---------------------------------------------------------------------
  
	public AbsTimeMeasurement[] getMeasurementArray()
    {
        AbsTimeMeasurement[] measurementArray = 
            new AbsTimeMeasurement[_measurementList.size()];

        for (int index = 0; index < _measurementList.size(); index++)
        {
            measurementArray[index] = getAbsTimeMeasurementByIndex(index);
        }
        return measurementArray;

    } //getMeasurementArray
    //---------------------------------------------------------------------


    public AbsTimeMeasurement getAbsTimeMeasurementByIndex(int index)
    {
        AbsTimeMeasurement measurement = null;

        measurement = (AbsTimeMeasurement) _measurementList.get(index);

        return measurement;

    } //getMeasurement

    //---------------------------------------------------------------------


    public AbsTimeMeasurement getAbsTimeMeasurementByTime(long desiredTime,
                                                   boolean interpolate,
                                                   boolean fillEndsWithZero)
    {
        AbsTimeMeasurement measurement = null;
        AbsTimeMeasurement curMeasurement = null;

        AbsTimeMeasurement previousMeasurement = null;
        AbsTimeMeasurement nextMeasurement = null;

        if ((desiredTime < this.getStartTime())
                || (desiredTime > this.getEndTime()))
        {
            if (fillEndsWithZero)
            {
                measurement = new AbsTimeMeasurement(0.0, desiredTime,
                        this._measuringUnit);
                measurement.setIsDefaulted(true);
            }
        }

        else
        // the desired time is in the range of the measurements contained in the
        // TimeSeries
        {
            for (int i = 0; i < _measurementList.size(); i++)
            {
                curMeasurement = getAbsTimeMeasurementByIndex(i);
                if (curMeasurement.getTime() == desiredTime)
                {
                    //found it exactly
                    measurement = curMeasurement;
                    break;
                }
                else if (curMeasurement.getTime() < desiredTime)
                {
                    previousMeasurement = curMeasurement;
                    //don't break, keep looking
                }
                else
                //curMeasurement.getTime() > time)
                {
                    nextMeasurement = curMeasurement;

                    if (interpolate)
                    {
                        measurement = AbsTimeMeasurement.interpolate(
                                previousMeasurement, nextMeasurement,
                                desiredTime);
                    }

                    break; // it is not in here, since we are past where it
                           // should be
                }
            } //end for

        } //end else


        return measurement;

    } //getMeasurement

    //---------------------------------------------------------------------


    public int getMeasurementCount()
    {
        return _measurementList.size();
    }

    //---------------------------------------------------------------------


    public double[] getMeasurementValueArray() throws Exception
    {
        return getMeasurementValueArray(_measuringUnit);
    } //end getMeasurementValueArray

    //---------------------------------------------------------------------
	
	public double[] getMeasurementValueArray(MeasuringUnit unit) throws Exception
	{
		double[] valueArray = new double[_measurementList.size()];
	
		if (_measuringUnit != unit)
		{
			convert(unit);
		
		}
	  
		for (int index = 0; index < _measurementList.size(); index++)
		{
	
			valueArray[index] = getAbsTimeMeasurementByIndex(index).getValue();
		}
	
		return valueArray;
	
	} //end getMeasurementValueArray
	
	//---------------------------------------------------------------------
	
	public MeasuringUnit getMeasuringUnit()
	{ 
		 return _measuringUnit;
	}
	
	//---------------------------------------------------------------------
	 
	
	public long getStartTime()
    {
        long startTime = -1;
        if (_measurementList.size() > 0)
        {
            startTime = getAbsTimeMeasurementByIndex(0).getTime();
        }
        return startTime;
    }

    //---------------------------------------------------------------------
	
	
	
	public void insertMeasurement(AbsTimeMeasurement newMeasurement)
	{
		//insert the measurement in the list in ascending order of time
		boolean inserted = false;
	
	
		//convert to the units of the TimeSeries
		//try
		{
			AbsTimeMeasurement convertedMeasurement = 
                               AbsTimeMeasurement.getConvertedCopy(newMeasurement,
                                                                   _measuringUnit);
		}
		//catch (MeasuringUnitConversionException e)
		//{
		//    e.printStackTrace();
		//    throw new Error (e.getMessage());
		//}
	
		// if the first measurement in the list
		if (_measurementList.size() == 0)
		{
			_measurementList.add(0, newMeasurement);
			inserted = true;
		}
		else if (newMeasurement.getTime() >= this.getEndTime())
		{
	    
			_measurementList.add(newMeasurement);
			inserted = true;
		}
	
	
	
		// if the proper location was not found
		if (!inserted)
		{
			//find the location to insert the measurement within the list
			// and insert it
			for (int i = 0; i < _measurementList.size() && ( !inserted); i++)
			{
				AbsTimeMeasurement currentMeasurement = 
								(AbsTimeMeasurement) _measurementList.get(i);
				if (newMeasurement.getTime() < currentMeasurement.getTime() )
				{
					_measurementList.add(i, newMeasurement);
					inserted = true;	
				}
			} //end for i
		}
	
		return;
	
	} //insertMeasurement
	 
	//---------------------------------------------------------------------
	
	
	public void insertTimeSeries(IrregularTimeSeries ts) 
	{
		if (ts != null)
		{
			try
			{
				for (int i = 0; i < ts._measurementList.size(); i++)
				{
				  this.insertMeasurement((ts.getAbsTimeMeasurementByIndex(i)));	
				}
			}
			catch (Exception e)
			{
				 //can't really happen, since the TimeSeries can't
				 //hold a measurement of the wrong type
				 throw new Error("woh! somehow a bad measurement got in here ");
		
			}
		}
	} //end insertTimeSeries
//	---------------------------------------------------------------------
	public IrregularTimeSeries getSubTimeSeries(long startTime, long endTime)
	{
	    /**
	     * Return the time series that is within this time window, inclusively. 
	     */
	    IrregularTimeSeries newTs = new IrregularTimeSeries(this._measuringUnit);
	    
	    for (int i= 0; i < this.getMeasurementCount(); i++)
	    {
	        AbsTimeMeasurement m = this.getAbsTimeMeasurementByIndex(i);
	        long mTime = m.getTime();
	        if ( (mTime >= startTime) && (mTime <= endTime) )
	        {
	            newTs.insertMeasurement(m);
	        }
	    }
		    
	    return newTs;
	    
	}
//---------------------------------------------------------------------

	
	public void setName(String name)
	{
		 _name = name;
	}
//	---------------------------------------------------------------------

	public String getName()
	{
		 return _name;
	}
	//---------------------------------------------------------------------

	
	
	public String toString()
	{
	 
		StringBuffer buffer = new StringBuffer();
		
		if (getName() != null)
		{
		   buffer.append("Name = " + getName() + " ");	
		}
		
		buffer.append("Units: " + _measuringUnit + " ");
	 
	 
	 
        NumberFormat f = new DecimalFormat("###.##"); 
 
        for (int i = 0; i < _measurementList.size(); i++)
        {
            double value = getAbsTimeMeasurementByIndex(i).getValue();
            String valueString = f.format(value);
            buffer.append(valueString + " "); 
        }
        
	
		
		return buffer.toString();
	}

//	---------------------------------------------------------------------
	  public static void main(String[] argStringArray)
	    {
	        MeasuringUnit dischargeUnit = MeasuringUnit.cfs;
	        ForecastAdjusterParams params = new ForecastAdjusterParams();
	        params.setBlendingHours(5);
	        params.setBlendingMethod(ForecastInterpolationMethod.DIFFERENCE);
	        params.setShouldDoAdjustment(true);
	        
	        ForecastAdjuster adjuster = new ForecastAdjuster(params);
	        
	        long fcstStartTime = System.currentTimeMillis();
	        
	        final long MILLIS_PER_HOUR = 1000 * 3600;
	            
	        long obsStartTime = fcstStartTime - (3 * 24 * MILLIS_PER_HOUR); //3 days before fcst start
	        long obsEndTime = fcstStartTime + (7 * MILLIS_PER_HOUR); // 7 hours after fcst start
	        
	        IrregularTimeSeries observedTs = new IrregularTimeSeries(dischargeUnit);
	     
	        double[] obsMeasurementValueArray =  {18, 19, 20, 22, 24, 28, 33, 28, 39, 40, 41, 43, 53, 57, 41, 29, 29, 28, 27, 26, 26, 24, 28  };
	        
	        int index = 0;
	        
	       
	  
	        
	        // load up the observed time series
	          index = 0;
	        for (long time = obsStartTime; time <= obsEndTime; time+= MILLIS_PER_HOUR)
	        {
	            AbsTimeMeasurement m = new AbsTimeMeasurement(obsMeasurementValueArray[index], time,  dischargeUnit);
	            
	            observedTs.insertMeasurement(m);    
	            
	            index++;
	            if (index == obsMeasurementValueArray.length)
	            {
	                index = 0;
	            }
	        }
	 
	        long subStartTime = obsStartTime + (2 * MILLIS_PER_HOUR);
	        long subEndTime = obsEndTime - (2 * MILLIS_PER_HOUR);
		      
	        IrregularTimeSeries subTimeSeries = observedTs.getSubTimeSeries(subStartTime, subEndTime);
	        
	        System.out.println("origObsTimeSeries = " + observedTs);
	        System.out.println("subObsTimeSeries = " + subTimeSeries);
		       
	        
	        
	    }
	   	
} //end class TimeSeries
