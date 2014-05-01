/*
 * Created on Jul 7, 2003
 *
 * Conversion routines modified from code by Russ Erb.
 */
package ohd.hseb.model;

import java.util.*;

import ohd.hseb.measurement.*;

/**
 * @author Chip Gobs
 *
 * This class contains all the data associated with
 * a RatingCurve and allows for conversion to and from discharge and stage.
 * It is not aware of the database.  
 * The RatingShift still needs to be accounted for; this could be done
 * with the code that loads the RatingCurve in the first place.
 */
public class RatingCurve
{
	 private String _locationId = null;
     
     private double _shiftAmount = 0.0;
     
     private String _usgsRatingNumber = null;
     private long _ratingDate = 0; 
     
	 private List _ratingPointList = new ArrayList();
     
     private int _millisPerHour = 60 * 60 * 1000;
     
     private MeasuringUnit _stageUnit = MeasuringUnit.feet;
     private MeasuringUnit _dischargeUnit = MeasuringUnit.cfs;
	
	//---------------------------------------------------------------------
	 public RatingCurve(String locationId)
	 {
	 	setLocationId(locationId);
	 }
	
	 public static RatingCurve getTestRatingCurve(String testLocationId)
	 {
	 	 RatingCurve rc = new RatingCurve(testLocationId);
	     double[] flowArray =  {0.0, 5,  20, 30, 50, 100, 250,  400,  600, 800, 1000 };
	     double[] stageArray = {0.0, .1, 1,  2,  5,   8,  9,   12.5,  15,  17,  19}; 	
	     
	     for (int i = 0; i < flowArray.length; i++)
	     {
	     	 RatingPoint point = new RatingPoint();
	     	 point.setDischarge(flowArray[i]);
			 point.setUnshiftedStage(stageArray[i]);
	     	 
	     	 rc.addRatingPoint(point);
	     }
	 	
	 	 return rc;
	 }
     
     public void setLocationId(String locationId)
     {
           _locationId = locationId;
     }

     public String getLocationId()
     { 
         return _locationId;
     }

     public void setUsgsRatingNumber(String usgsRatingNumber)
     {
         _usgsRatingNumber = usgsRatingNumber;
     }
    
     public String getUsgsRatingNumber()
     {
         return _usgsRatingNumber;
     }
    
     public void setRatingDate(long ratingDate)
     {
         _ratingDate = ratingDate;
     }
    
     public long getRatingDate()
     {
         return _ratingDate;
     }
    //  ---------------------------------------------------------------------
    
     public void setShiftAmount(double shiftAmount)
     {
        _shiftAmount = shiftAmount;
        
        //update all of the associated ratingPoints
        for (int i = 0; i < _ratingPointList.size(); i++)
        {
            RatingPoint ratingPoint = (RatingPoint) _ratingPointList.get(i);    
            ratingPoint.setShiftAmount(_shiftAmount);
        }
     }

    //  ---------------------------------------------------------------------
    
   
     public double getShiftAmount()
     {
        return _shiftAmount;
     }
    //	---------------------------------------------------------------------
 
	 public boolean exists()
	 {
	 	 boolean result = false;
	 	 
	 	 if (_ratingPointList.size() > 1)
	 	 {
	 	     result = true; 	  
	 	 }
	 	 
	 	 return result;
	 }
   //	---------------------------------------------------------------------

	 public void addRatingShift()
	 {
	     
	 }
     //	---------------------------------------------------------------------
     public List getRatingPointList()
     {
         List newList = new ArrayList();
         for (int i = 0; i < _ratingPointList.size(); i++)
         {
             RatingPoint point = (RatingPoint) _ratingPointList.get(i);
             newList.add(point);
         }
         
         return newList;
     }
	 
	 //	---------------------------------------------------------------------
	 
     public int getRatingPointCount()
     {
         return _ratingPointList.size();	
     }
 
	 public RatingPoint getRatingPoint(int index)
	 {  
	 	RatingPoint point = null;
	 	
	 	if  ((index >= 0) && (index <= _ratingPointList.size() ) )
	 	point = (RatingPoint) _ratingPointList.get(index);
	 	
	 	return point;
	 }
//	--------------------------------------------------------------------- 
	 public RatingPoint getFirstRatingPoint()
	 {
		RatingPoint point = null;

		point = getRatingPoint(0);
	 	
		return point;
	 }
//	---------------------------------------------------------------------	 
	 public RatingPoint getSecondRatingPoint()
	 {
		RatingPoint point = null;
		point = getRatingPoint(1);
	 	
		return point;
	 }
//	---------------------------------------------------------------------	
	 public RatingPoint getLastRatingPoint()
	 {
	 	RatingPoint point = null;

	 	point = getRatingPoint(_ratingPointList.size() - 1);
	 	
	 	return point;
	 }
//	---------------------------------------------------------------------
	 
	 public RatingPoint getNextToLastRatingPoint()
	 {
	     RatingPoint point = null;

		 point = getRatingPoint(_ratingPointList.size() - 2);
	 	
	 	 return point;  
	 }
//	---------------------------------------------------------------------
	 public void addRatingPoint(RatingPoint newPoint)
	 {
	      //insert the RatingPoint at the correct point
	      // in order to keep the list in ascending sorted order by stage
	      
         newPoint.setShiftAmount(_shiftAmount);
      
	      double stage = newPoint.getShiftedStage();
	      boolean inserted = false;
	      
          
	      if (_ratingPointList.size() == 0)
	      {
	           _ratingPointList.add(newPoint);
	           inserted = true;	
	      }
	      else
	      {
	           for (int i = 0; i < _ratingPointList.size() && !inserted; i++)
	           {
	           	    RatingPoint curPoint = (RatingPoint) _ratingPointList.get(i);
	                if (stage < curPoint.getShiftedStage())
	                {
	                     _ratingPointList.add(i, newPoint);
	                     inserted = true;	     	
	                }
                    else if (stage == curPoint.getShiftedStage())
                    {
                        _ratingPointList.add(i, newPoint);
                        inserted = true;
                        removeRatingPoint(curPoint);
                    }
                    	
	           }	
	           if (!inserted)
	           {
	           	    //need to add to the end of the list
	                _ratingPointList.add(newPoint);	
	           }
	      } //end else
	     
          return;
	 }
//  ---------------------------------------------------------------------
     public boolean removeRatingPoint(RatingPoint ratingPoint)
     {
          //remove the RatingPoint if a match exists in the list
          // 
          boolean removed = false;
        
          for (int i = 0; i < _ratingPointList.size() && ! removed ; i++)
          {
               RatingPoint curPoint = (RatingPoint) _ratingPointList.get(i);
               if (curPoint.equals(ratingPoint))
               {
                   _ratingPointList.remove(ratingPoint); 
                   removed = true;     
               }   
          }   
           
         return removed;
     }
//  ---------------------------------------------------------------------
	 
	public IrregularTimeSeries getStageTimeSeries(IrregularTimeSeries flowTimeSeries)
	{
        //includes the shift
	    IrregularTimeSeries stageTimeSeries = new IrregularTimeSeries(MeasuringUnit.feet);
	    	
		for (int i = 0; i < flowTimeSeries.getMeasurementCount(); i++)
		{
			AbsTimeMeasurement flowMeasurement = flowTimeSeries.getAbsTimeMeasurementByIndex(i);	
			
			double stageValue = getStageFromDischarge(flowMeasurement.getValue());
			AbsTimeMeasurement stageMeasurement = new AbsTimeMeasurement(stageValue, 
														   flowMeasurement.getTime(), 
														   MeasuringUnit.feet);
														   
			stageTimeSeries.insertMeasurement(stageMeasurement);
			
		} //end for
		
		return stageTimeSeries;
	} 
    
//  ---------------------------------------------------------------------
	public IrregularTimeSeries getDischargeIrregularTimeSeries(IrregularTimeSeries stageTimeSeries)
	{
	    //ensure input units are correct
	    stageTimeSeries.convert(_stageUnit);
       
       
        IrregularTimeSeries dischargeTimeSeries = new IrregularTimeSeries(_dischargeUnit);
            
        for (int i = 0; i < stageTimeSeries.getMeasurementCount(); i++)
        {
            AbsTimeMeasurement stageMeasurement = stageTimeSeries.getAbsTimeMeasurementByIndex(i);   
       
            double dischargeValue = getDischargeFromStage(stageMeasurement.getValue());
            long time = stageMeasurement.getTime();
            AbsTimeMeasurement dischargeMeasurement = 
                	new AbsTimeMeasurement(dischargeValue, time, _dischargeUnit);
                                                      
            dischargeTimeSeries.insertMeasurement(dischargeMeasurement);
       
        } //end for
   
       return dischargeTimeSeries;
       
  	    
	    
	}
	
//  ---------------------------------------------------------------------

    public RegularTimeSeries getStageRegularTimeSeries(RegularTimeSeries flowTimeSeries)
    {
        
         //ensure units are correct
         flowTimeSeries.convert(_dischargeUnit);
        
        
         // get info about flowTimeSeries
         long startTime = flowTimeSeries.getStartTime();
         long endTime = flowTimeSeries.getEndTime();
         int intervalInHours = flowTimeSeries.getIntervalInHours();
         int intervalInMillis = intervalInHours * _millisPerHour;
        
        
         RegularTimeSeries stageTimeSeries = new RegularTimeSeries(startTime, endTime, 
                                                intervalInHours, _stageUnit);
        
        
            
         for (long time = startTime; time <= endTime; time += intervalInMillis)
         {
             Measurement flowMeasurement = flowTimeSeries.getMeasurementByTime(time);   
        
             double stageValue = getStageFromDischarge(flowMeasurement.getValue());
             Measurement stageMeasurement = new Measurement(stageValue, 
                                                            _stageUnit);
                                                       
            stageTimeSeries.setMeasurementByTime(stageMeasurement, time);
        
         } //end for
    
        return stageTimeSeries;
        
    } //end  getStageRegularTimeSeries
//  ---------------------------------------------------------------------
   
    public RegularTimeSeries getDischargeRegularTimeSeries(RegularTimeSeries stageTimeSeries)
    {
        
         //ensure input units are correct
         stageTimeSeries.convert(_stageUnit);
        
        
         // get info about flowTimeSeries
         long startTime = stageTimeSeries.getStartTime();
         long endTime = stageTimeSeries.getEndTime();
         int intervalInHours = stageTimeSeries.getIntervalInHours();
         int intervalInMillis = intervalInHours * _millisPerHour;
        
        
         RegularTimeSeries flowTimeSeries = new RegularTimeSeries(startTime, endTime, 
                                                intervalInHours, _dischargeUnit);
        
        
            
         for (long time = startTime; time <= endTime; time += intervalInMillis)
         {
             Measurement stageMeasurement = stageTimeSeries.getMeasurementByTime(time);   
        
             double flowValue = getDischargeFromStage(stageMeasurement.getValue());
             Measurement flowMeasurement = new Measurement(flowValue, 
                                                           _dischargeUnit);
                                                       
             flowTimeSeries.setMeasurementByTime(flowMeasurement, time);
        
         } //end for
    
        return flowTimeSeries;
        
    } //end  getFlowRegularTimeSeries
         
	 
//  ---------------------------------------------------------------------
	 
//	*****************************************************************************
//	taken from Russ Erb's code
//	  stageToDischarge() - this method is called with a stage value and returns
//				   the corresponding discharge
//	*****************************************************************************
    public double getDischargeFromStage(double stage)
	{

	    RatingPoint firstStage = new RatingPoint();
	    RatingPoint lastStage = new RatingPoint();
	    RatingPoint lowerStage = new RatingPoint();
	    RatingPoint higherStage = new RatingPoint();

	    double  discharge=0.0;
	    double  diff_discharge=0.0;
	    double  diff_stage=0.0;

	    // if the rating curve doesn't exist then return -999
	    if (! this.exists())
		    return(-999.0);

	    // if the stage value passed in is less then the lowest stage in the
	    // rating table then extrapolate the discharge
	    firstStage = getFirstRatingPoint();
	    if (stage < firstStage.getShiftedStage())
	    {
		    higherStage = getSecondRatingPoint();
		    diff_discharge = higherStage.getDischarge() - firstStage.getDischarge();
		    diff_stage = higherStage.getShiftedStage() - firstStage.getShiftedStage();
		    if (diff_stage == 0.0)
		  	   discharge = firstStage.getDischarge();
		    else
			   discharge = firstStage.getDischarge() - ((diff_discharge/diff_stage) * (firstStage.getShiftedStage() - stage));
	    }
	
	    // if the stage value passed in is greater then the highest stage in the
	    // rating table then extrapolate the discharge
	    lastStage = getLastRatingPoint();
	    if (stage > lastStage.getShiftedStage())
	    {
		    lowerStage = getNextToLastRatingPoint();
		    diff_discharge =  lastStage.getDischarge() - lowerStage.getDischarge();
		    diff_stage = lastStage.getShiftedStage() - lowerStage.getShiftedStage();
		    if (diff_stage == 0.0)
		   	   discharge = lastStage.getDischarge();
		    else
			   discharge = lastStage.getDischarge() + ((diff_discharge/diff_stage) * (stage - lastStage.getShiftedStage()));
	    }

	    // if the stage value passed in is between the lowest and highest stage
	    // in the rating table then interpolate the discharge
	    if ( (stage >= firstStage.getShiftedStage()) && (stage <= lastStage.getShiftedStage()) )
	    {
		    for (int ctr=0; ctr< _ratingPointList.size()-1; ctr++)
		    {
			    lowerStage =  getRatingPoint(ctr);
			    higherStage = getRatingPoint(ctr+1);
			    if ( (stage >= lowerStage.getShiftedStage()) && (stage <= higherStage.getShiftedStage()) )
			    {
				    diff_discharge = higherStage.getDischarge() - lowerStage.getDischarge();
				    diff_stage = higherStage.getShiftedStage() - lowerStage.getShiftedStage();

				    if (diff_stage == 0.0)
					    discharge = lowerStage.getDischarge();
				    else
					    discharge = lowerStage.getDischarge() + ((diff_discharge/diff_stage) * (stage - lowerStage.getShiftedStage()));

				    break;
			    }
		    } //end for
	    } //end if

	    // for some reason the discharge is less than zero then return zero
	    if (discharge < 0.0)
		   return(0.0);
	    else
		   return(discharge);
	
	  } // end of stageToDischarge method


//	*****************************************************************************
//   taken from Russ Erb's code
//	  dischargeToStage() - this method is called with a discharge value and returns
//				   the corresponding stage
//	*****************************************************************************
	public double getStageFromDischarge(double discharge)
	{

	    RatingPoint firstDischarge = new RatingPoint();
	    RatingPoint lastDischarge = new RatingPoint();
	    RatingPoint lowerDischarge = new RatingPoint();
	    RatingPoint higherDischarge = new RatingPoint();

	    double  stage=0.0;
	    double  diff_discharge=0.0;
	    double  diff_stage=0.0;

		// if the rating curve doesn't exist then return -999
		if (! this.exists())
		    return(-999.0);
	
		// if the discharge value passed in is less then the lowest discharge in the
		// rating table then extrapolate the stage
		firstDischarge = getFirstRatingPoint();
		if (discharge < firstDischarge.getDischarge())
		{
		    higherDischarge = getSecondRatingPoint();
			diff_discharge = higherDischarge.getDischarge() - firstDischarge.getDischarge();
			diff_stage = higherDischarge.getShiftedStage() - firstDischarge.getShiftedStage();
			if (diff_discharge == 0.0)
			    stage = firstDischarge.getShiftedStage();
			else
			    stage = firstDischarge.getShiftedStage() - ((diff_stage/diff_discharge) * (firstDischarge.getDischarge() - discharge));
		}
	
	    // if the discharge value passed in is greater then the highest discharge in the
	    // rating table then extrapolate the stage
	    lastDischarge = getLastRatingPoint();
	    if (discharge > lastDischarge.getDischarge())
	    {
		    lowerDischarge = getNextToLastRatingPoint();
		    diff_discharge =  lastDischarge.getDischarge() - lowerDischarge.getDischarge();
		    diff_stage = lastDischarge.getShiftedStage() - lowerDischarge.getShiftedStage();
		    if (diff_discharge == 0.0)
		        stage = lastDischarge.getShiftedStage();
		    else
			    stage = lastDischarge.getShiftedStage() + ((diff_stage/diff_discharge) * (discharge - lastDischarge.getDischarge()));
	    }

	    // if the discharge value passed in is between the lowest and highest discharge
	    // in the rating table then interpolate the stage
	    if ( (discharge >= firstDischarge.getDischarge()) && (discharge <= lastDischarge.getDischarge()) )
	    {
		    for (int ctr=0; ctr< _ratingPointList.size()-1; ctr++)
		    {
		        lowerDischarge =  getRatingPoint(ctr);
			    higherDischarge = getRatingPoint(ctr+1);
			    if ( (discharge >= lowerDischarge.getDischarge()) && (discharge <= higherDischarge.getDischarge()) )
			    {
				    diff_discharge = higherDischarge.getDischarge() - lowerDischarge.getDischarge();
				    diff_stage = higherDischarge.getShiftedStage() - lowerDischarge.getShiftedStage();

				    if (diff_discharge == 0.0)
					    stage = lowerDischarge.getShiftedStage();
				    else
					    stage = lowerDischarge.getShiftedStage() + ((diff_stage/diff_discharge) * (discharge - lowerDischarge.getDischarge()));

				    break;
			    }
		    } //end for
	    } //end if 
	    
	   // System.out.println("discharge = " + discharge + " stage = " + stage);

	    return(stage);
	
	} // end of dischargeToStage method




//	*****************************************************************************
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        buffer.append(getLocationId());
        
        for (int i = 0; i < getRatingPointCount() ; i++)
        {
			RatingPoint point = (RatingPoint) this.getRatingPoint(i);
			buffer.append(point + "\n");
        }
        
        return buffer.toString();
        	
    } //end toString




   
} //end class RatingCurve
