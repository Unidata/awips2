/*
 * Created on Aug 27, 2004
 *
 * 
 */
package ohd.hseb.model;

import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;

//import ohd.hseb.util.CodeTimer;

/**
 * @author Chip Gobs
 *
 * The job of this class is to manage the blending of observed and forecast data such
 * that the forecast time series is adjusted to match better with the observed data.
 */
public class ForecastAdjuster
{
    private static final long MILLIS_PER_MINUTE = 60 * 1000;
    private static final long MILLIS_PER_HOUR = 60 * MILLIS_PER_MINUTE;  
    
    private ForecastAdjusterParams _params = null;   
    private List _observedForecastPairList = null;
    private Map _observedForecastMap = null;
    private long _lastObservedTimeToUseAsInput = 0;
   
    private final static boolean _debug = false;
    
    // ----------------------------------------------------------------------------------
    
    public ForecastAdjuster(ForecastAdjusterParams params)
    {
       // String header = "ForecastAdjuster()" ;
        
      //  System.out.println("inside " + header);
        
        _params = params;   
        
        _observedForecastPairList = new ArrayList();
        _observedForecastMap = new HashMap();
    }
  
    
    // ----------------------------------------------------------------------------------
    public RegularTimeSeries getAdjustedTimeSeries(IrregularTimeSeries observedTs, 
                                                   RegularTimeSeries origFcstTs )
     {
         RegularTimeSeries adjustedFcstTs = null;
              
         ObservedForecastMeasurementPair previousPair = null;
         ObservedForecastMeasurementPair nextPair = null;
         
         AbsTimeMeasurement origFcstMeasurement = null;
         AbsTimeMeasurement adjustedMeasurement = null;
                  
          
         //trim the observedTimeSeries as needed
         IrregularTimeSeries trimmedObsTs = observedTs.getSubTimeSeries(observedTs.getStartTime(),
                 									                    getLastObservedTimeToUseAsInput());
         
         int allowableBlendingHours = _params.getBlendingHours();
        
         ForecastBlender forwardBlender = new ForecastBlender(allowableBlendingHours);
         ForecastBlender backwardBlender = new ForecastBlender(allowableBlendingHours);
         
         
         if ( _params.shouldDoAdjustment())
         {
            // System.out.println(header + "Actually doing the adjustment");
             //create the data structure for helping go from a forecast point to its matching
             //observed point (if any)
            
             buildObsAndForecastPairList( trimmedObsTs, 
                     					  origFcstTs,
                     					  _params.getPairingTimeMinutes() * MILLIS_PER_MINUTE);
       
             // create new forecast time series
             adjustedFcstTs = new RegularTimeSeries(origFcstTs.getStartTime(), origFcstTs.getEndTime(), 
                     							origFcstTs.getIntervalInHours(),
                     							origFcstTs.getMeasuringUnit());
                  
              //for each forecast value
             for (long time = origFcstTs.getStartTime(); time <= origFcstTs.getEndTime(); time += MILLIS_PER_HOUR )
             {      
                 origFcstMeasurement = origFcstTs.getAbsTimeMeasurementByTime(time);
                 
                 //used for debugging/printing purposes
                 String fcstTimeString = DbTimeHelper.getDateTimeStringFromLongTime(time);
                  
                 adjustedMeasurement = new AbsTimeMeasurement(origFcstMeasurement);
         
                 //try to find the origFcstMeasurement in the hashMap                
                 AbsTimeMeasurement matchingObsMeasurement = 
                     (AbsTimeMeasurement) _observedForecastMap.get(origFcstMeasurement);
                
                 
                 if (matchingObsMeasurement != null) //there is a matching observedMeasurement, use direct adjustment
                 {
                     
                     performMatchingAdjustment(matchingObsMeasurement,
                                               adjustedMeasurement, 
                                               forwardBlender, 
                                               backwardBlender,
                                               fcstTimeString);
                     
      
                 }
                 else // can't do direct adjustment, try interpolation and blending
                 {   	
                    previousPair = findBestPreviousPairByTime(time);
                    nextPair = findBestNextPairByTime(time);
                    
                    boolean doBlending = performInterpolation(origFcstMeasurement, 
                                                      adjustedMeasurement,
                                                      previousPair, 
                                                      nextPair,
                                                      forwardBlender,
                                                      backwardBlender,
                                                      fcstTimeString);
     
                    if (doBlending) //pairs not close enough to each other, or at least
                         // one pair is missing, so try to use blending
                    {
                        performBlending(origFcstMeasurement,
                                        adjustedMeasurement,
                                        previousPair,
                                        nextPair,
                                        forwardBlender,
                                        backwardBlender,
                                        fcstTimeString);
                    }
       
                 } //end else for unmatched fcst measurements
          
                 if (adjustedMeasurement.getValue() < 0.0)
                 {
                     adjustedMeasurement.setValue(0.0);
                 }

                 adjustedFcstTs.setMeasurementByTime(adjustedMeasurement, time);
                 
             } //end for each forecast value
          
         }
         else //don't adjust anything
         {
             if (_debug)
             {
                System.out.println("Not doing the adjustment");
             }
             adjustedFcstTs = origFcstTs;
         }
        
         return adjustedFcstTs;
         
     }
    // -------------------------------------------------------------------------------------------------
    private void performMatchingAdjustment(AbsTimeMeasurement matchingObsMeasurement,
            AbsTimeMeasurement adjustedMeasurement,
            ForecastBlender forwardBlender,
            ForecastBlender backwardBlender,
            String fcstTimeString)
    {
      
        if (_debug)
        {
            double amount = matchingObsMeasurement.getValue() - adjustedMeasurement.getValue();
            System.out.println(fcstTimeString + " using direct correction by pairing. Adjusting by " + amount);
        }
        //set value to current pairing observed value
        //adjustedMeasurement = new AbsTimeMeasurement(origFcstMeasurement);
        adjustedMeasurement.setValue(matchingObsMeasurement.getValue());
        
        forwardBlender.clearBlender();
        backwardBlender.clearBlender();

    }

    
    // -------------------------------------------------------------------------------------------------

    private  boolean performInterpolation(AbsTimeMeasurement origFcstMeasurement,
            AbsTimeMeasurement adjustedMeasurement,
            ObservedForecastMeasurementPair previousPair,
            ObservedForecastMeasurementPair nextPair,
            ForecastBlender forwardBlender,
            ForecastBlender backwardBlender,
            String fcstTimeString)
    {
        AbsTimeMeasurement previousObs = null;
        AbsTimeMeasurement nextObs = null;
 
        boolean doBlending = false;
        
        if ((previousPair != null) && (nextPair != null))  //
        {
            
            previousObs = previousPair.getObservedMeasurement();
            nextObs = nextPair.getObservedMeasurement();

            long prevTime = previousObs.getTime();
            long nextTime = nextObs.getTime();

            //consider adding a method to AbsTimeMeasurement to
            // allow for time window comparisons

            long timeDiff = nextTime - prevTime;
            
             // if can do interpolation
            if (timeDiff <= _params.getInterpolationHours()  * MILLIS_PER_HOUR)
            {
       
                if (_debug)
                {
                    System.out.print(fcstTimeString +  " using interpolation by");
                }
                adjustByInterpolation(origFcstMeasurement,
                                      adjustedMeasurement,
                                      previousPair,
                                      nextPair);

                forwardBlender.clearBlender();
                backwardBlender.clearBlender();
                doBlending = false;
            }
            else //can't do interpolation, need to try blending
            {
                doBlending = true;
            }
        } //end if previousPair and nextPair not null
        
        else //can't do interpolation, need to try blending
        {
            doBlending = true;
        }
        
        
        return doBlending;
    }
    // -------------------------------------------------------------------------------------------------
    
    private AbsTimeMeasurement performBlending(AbsTimeMeasurement origFcstMeasurement,
                                               AbsTimeMeasurement adjustedMeasurement,
                                               ObservedForecastMeasurementPair previousPair,
                                               ObservedForecastMeasurementPair nextPair,
                                               ForecastBlender forwardBlender,
                                               ForecastBlender backwardBlender,
                                               String fcstTimeString)
    {
        AbsTimeMeasurement forwardAdjustment = forwardBlender.getBlendAdjustment(previousPair, origFcstMeasurement);
        AbsTimeMeasurement backwardAdjustment = backwardBlender.getBlendAdjustment(nextPair, origFcstMeasurement);

        AbsTimeMeasurement blendingAdjustment = combineBlendingAdjustments(forwardAdjustment, 
                                                                           backwardAdjustment);
        adjustedMeasurement.addMeasurement(blendingAdjustment);
        
        if (blendingAdjustment.getValue() != 0.0)
        {     
            if (_debug)
            {
               System.out.println(fcstTimeString + " Total blending adjustment =  " + blendingAdjustment.getValue() +
                                   " original value = " + origFcstMeasurement.getValue() +
                                   " new value = " + adjustedMeasurement.getValue());
            }    
        }
        else
        {
            if (_debug)
            {
               System.out.println(fcstTimeString +  " No adjustment.");
            }    
        }
        
        return adjustedMeasurement;
    }
    
    // -------------------------------------------------------------------------------------------------
    private AbsTimeMeasurement combineBlendingAdjustments(AbsTimeMeasurement forwardBlendAdjustment,
                                                          AbsTimeMeasurement backwardBlendAdjustment)
    {
        
        double forwardValue = forwardBlendAdjustment.getValue();
        double backwardValue = backwardBlendAdjustment.getValue();
        
        double adjustmentValue =  forwardValue + backwardValue;
        
        int notEqualsZeroCount = 0;
        
        //count the non-zero adjustments
        if (forwardValue != 0.0)
        {
            notEqualsZeroCount ++;
            
        }
        if (backwardValue != 0.0)
        {
            notEqualsZeroCount ++;
            
        }
        
        if (notEqualsZeroCount > 0)
        {
           adjustmentValue /= (double) notEqualsZeroCount;
        }
        else
        {
           adjustmentValue = 0.0; 
        }
        
        
        if (_debug)
        {
             System.out.println("forward blending Adjustment = " + forwardValue +
                                " backward blending adjustment = " + backwardValue);
        }
        
        AbsTimeMeasurement blendingAdjustment = new AbsTimeMeasurement(forwardBlendAdjustment); //sets the time, will change the value
        blendingAdjustment.setValue(adjustmentValue);
        
        return blendingAdjustment;
        
        
    }
      
    // ----------------------------------------------------------------------------------
    private void adjustByInterpolation(AbsTimeMeasurement origFcstMeasurement, 
                                                     AbsTimeMeasurement adjustedFcstMeasurement, 
                                                     ObservedForecastMeasurementPair prevPair,
                                                     ObservedForecastMeasurementPair nextPair)	
    {
            
        ForecastInterpolationMethod blendingMethod = _params.getBlendingMethod();
         
        AbsTimeMeasurement prevObs = prevPair.getObservedMeasurement();
        AbsTimeMeasurement prevFcst = prevPair.getForecastMeasurement();
         
        AbsTimeMeasurement nextObs = nextPair.getObservedMeasurement();
        AbsTimeMeasurement nextFcst = nextPair.getForecastMeasurement();
     
        long origFcstTime = origFcstMeasurement.getTime();
        
        if ( (blendingMethod != null ) && 
             (blendingMethod.equals(ForecastInterpolationMethod.RATIO) ))
        {
            double ratio1 = prevObs.getValue() / prevFcst.getValue();
            double ratio2 = nextObs.getValue() / nextFcst.getValue();

            double ratioDiff = Math.abs(ratio1 - ratio2);

            //if out of sanity range, then do DIFFERENCES anyway
            if ( (ratio1 > 5.0) || (ratio2 > 5.0)  || (ratioDiff > 2.0) )
            {
                System.out.println(" differences ");
            	double  adjustment = getAdjustmentUsingDifferences(origFcstTime, prevPair, nextPair);
            	double oldValue = adjustedFcstMeasurement.getValue();
            	adjustedFcstMeasurement.setValue(oldValue + adjustment);
            }
            else // it is OK to use ratio method
            {
                System.out.println(" ratios ");
                double  adjustment = getAdjustmentUsingRatios(origFcstTime, prevPair, nextPair);
            	double oldValue = adjustedFcstMeasurement.getValue();
            	adjustedFcstMeasurement.setValue(oldValue * adjustment);
            }    
        }
        else //use differences
        {
            System.out.println(" differences ");
      		double  adjustment = getAdjustmentUsingDifferences(origFcstTime, prevPair, nextPair);
        	double oldValue = adjustedFcstMeasurement.getValue();
        	adjustedFcstMeasurement.setValue(oldValue + adjustment);
            
        }
        return;
    }
    
    // ----------------------------------------------------------------------------------
    private double getAdjustmentUsingDifferences(long origFcstTime,
                                                 ObservedForecastMeasurementPair prevPair,
                                                 ObservedForecastMeasurementPair nextPair)
    {
        AbsTimeMeasurement prevObs = prevPair.getObservedMeasurement();
        AbsTimeMeasurement prevFcst = prevPair.getForecastMeasurement();
         
        AbsTimeMeasurement nextObs = nextPair.getObservedMeasurement();
        AbsTimeMeasurement nextFcst = nextPair.getForecastMeasurement();
 
     
    	double diff1 = prevObs.getValue() - prevFcst.getValue();
    	double diff2 = nextObs.getValue() - nextFcst.getValue();
    	
    	AbsTimeMeasurement diff1Measurement = 
    	    	new AbsTimeMeasurement(diff1, prevObs.getTime(), prevObs.getUnit());
    	
    	AbsTimeMeasurement diff2Measurement = 
	    	new AbsTimeMeasurement(diff2, nextObs.getTime(), nextObs.getUnit());

    	
    	AbsTimeMeasurement adjustmentMeasurement = 
    	    	interpolate(diff1Measurement, diff2Measurement, origFcstTime);
        
    
    	double adjustmentValue = adjustmentMeasurement.getValue();
    	
    	return adjustmentValue;
    	
    }
    
    // ----------------------------------------------------------------------------------
    
    
    private double getAdjustmentUsingRatios(long origFcstTime,
                                                 ObservedForecastMeasurementPair prevPair,
                                                 ObservedForecastMeasurementPair nextPair)
    {
        AbsTimeMeasurement prevObs = prevPair.getObservedMeasurement();
        AbsTimeMeasurement prevFcst = prevPair.getForecastMeasurement();
         
        AbsTimeMeasurement nextObs = nextPair.getObservedMeasurement();
        AbsTimeMeasurement nextFcst = nextPair.getForecastMeasurement();
 
     
    	double ratio1 = prevObs.getValue() / prevFcst.getValue();
    	double ratio2 = nextObs.getValue() / nextFcst.getValue();
    	
    	AbsTimeMeasurement ratio1Measurement = 
    	    	new AbsTimeMeasurement(ratio1, prevObs.getTime(), prevObs.getUnit());
    	
    	AbsTimeMeasurement ratio2Measurement = 
	    	new AbsTimeMeasurement(ratio2, nextObs.getTime(), nextObs.getUnit());

    	AbsTimeMeasurement adjustmentMeasurement = 
    	    	AbsTimeMeasurement.interpolate(ratio1Measurement, ratio2Measurement, origFcstTime);
            
    	double adjustmentValue = adjustmentMeasurement.getValue();
    	
    	return adjustmentValue;
    }
    
    // ----------------------------------------------------------------------------------
    
    private void buildObsAndForecastPairList(IrregularTimeSeries observedTs,
                                              RegularTimeSeries forecastTs,
                                              long maxTimeDifferenceForPairing)

    {
  //      String header = "ForecastAdjuster.buildObsAndForecastPairList(): ";
 
        //initialize the internal structures
        _observedForecastPairList = new ArrayList();
        _observedForecastMap = new HashMap();
         
        
        // for each forecast value
        for (int fcstIndex = 0; fcstIndex < forecastTs.getMeasurementCount(); fcstIndex++)
        { 
            
            AbsTimeMeasurement forecastMeasurement  =
                forecastTs.getAbsTimeMeasurementByIndex(fcstIndex);
            
            findObsAndForecastPairAndAddtoList(forecastMeasurement, observedTs, maxTimeDifferenceForPairing);
            
         
        } //end for each forecast value
        
        return;
        
    } // buildObsAndForecastPairList
    // -----------------------------------------------------------------------------------------------
    private void findObsAndForecastPairAndAddtoList(AbsTimeMeasurement forecastMeasurement,
            IrregularTimeSeries observedTs,
            long maxTimeDifferenceForPairing)
    { 
        
        long forecastTime = 0;
        long obsTime = 0;
        long timeDiff = 0;

        int startingObsIndex = 0;

        forecastTime = forecastMeasurement.getTime();

        //set up inner loop variables
        boolean foundFirstLegalMatch = false;
        long smallestTimeDifference = -1;
        AbsTimeMeasurement bestObsMeasurement = null;
        boolean done = false;

       // System.out.println(header + "forecastMeasurement = " + forecastMeasurement);
        
//      look at each observed to find the best match (if any) for this forecast value

             
       for (int obsIndex = startingObsIndex;
            (!done &&  (obsIndex < observedTs.getMeasurementCount()));
            obsIndex++)
       {
            AbsTimeMeasurement obsMeasurement  =
               observedTs.getAbsTimeMeasurementByIndex(obsIndex);

            obsTime = obsMeasurement.getTime();
     
            
            timeDiff = Math.abs(forecastTime - obsTime);
            
            //  observed is in valid range for pairing
            if (timeDiff <= maxTimeDifferenceForPairing) 
            {
                if (! foundFirstLegalMatch)
                {
                    foundFirstLegalMatch = true;
                    smallestTimeDifference  = timeDiff;
                    bestObsMeasurement = obsMeasurement;
                }
                else //have already found a valid match for this forecast point
                {
                    if (timeDiff < smallestTimeDifference)
                    {
                        smallestTimeDifference = timeDiff;
                        bestObsMeasurement = obsMeasurement;
                    } 
                    else //we have gone past the best-matching observed
                        //we have already found smallest time difference 
                        //(list is sorted in ascending order by time)
                    {
                        done = true;
                        startingObsIndex = obsIndex; //next time we do the inner loop, start here
                        
                    }
                }
            }
            else // observed is not within valid range for pairing
            {
                if (foundFirstLegalMatch)  // we now know the best match
                {
                    done = true;
                    startingObsIndex = obsIndex; //next time we do the inner loop, start here
            
                }
                else // (! foundFirstLegalMatch) 
                {
                    if (obsTime > forecastTime)
                    {
                        // there is no match for this forecast measurement,
                        // since we have passed the forecastTime and
                        // are out of the time window
                        done = true;
                        startingObsIndex = obsIndex;
                    }
                    else // obsTime <= forecastTime, actually, if it were =, the time window would match 
                    {
                        //System.out.println("obsTime < forecastTime");
                        // we need to keep looking, because we have not 
                        // yet gotten in range
                    }
                } //end else ! foundFirstLegalMatch
              
            } //end else observed not within valid range for printing
            
          
        }   //end for each observed
       
       
       //check to see if any matches were found
       // if so, then add the pair of points to the list and map
       if (foundFirstLegalMatch)
       {
           ObservedForecastMeasurementPair pair = new ObservedForecastMeasurementPair(
                   bestObsMeasurement, forecastMeasurement);
           
           _observedForecastPairList.add(pair);
           _observedForecastMap.put(forecastMeasurement, bestObsMeasurement);
           
           if (_debug)
           {
               System.out.println("pair = " + pair);
           }
       }
       else
       {
           
           //System.out.println("did not find a pair for " + forecastMeasurement);
       }
   
       
    } // end findObsAndForecastPairAndAddtoList
    
    // -----------------------------------------------------------------------------------------------
    
    
    private ObservedForecastMeasurementPair findBestNextPairByTime(long targetTime)
    {
        	ObservedForecastMeasurementPair nextPair = null;
        	ObservedForecastMeasurementPair currentPair = null;
        	boolean done = false;
        	
        	for (int i = 0; !done && i < _observedForecastPairList.size(); i++)
        	{
        	    currentPair = (ObservedForecastMeasurementPair) _observedForecastPairList.get(i);
        	    if (currentPair.getForecastMeasurement().getTime() > targetTime)
        	    {
        	        nextPair = currentPair;
        	        done = true;
        	    }
        	}
        	
        	return nextPair;
    }    
//  -----------------------------------------------------------------------------------------------
    private ObservedForecastMeasurementPair findBestPreviousPairByTime(long targetTime)
    {
        	ObservedForecastMeasurementPair previousPair = null;
        	ObservedForecastMeasurementPair currentPair = null;
        	boolean done = false;
        	
        	for (int i = 0; !done && i < _observedForecastPairList.size(); i++)
        	{
        	    currentPair = (ObservedForecastMeasurementPair) _observedForecastPairList.get(i);
        	    if (currentPair.getForecastMeasurement().getTime() < targetTime)
        	    {
        	        previousPair = currentPair;
        	    }
        	    else // m.time >= targetTime
        	    {  
        	        done = true;
        	    }
        	}
        	
        	return previousPair;
    }    
//  -----------------------------------------------------------------------------------------------

    private ObservedForecastMeasurementPair oldFindBestNextPairByTime(
                                                                   IrregularTimeSeries observedTs,
                                                                   RegularTimeSeries forecastTs,
                                                                   long targetTime,
                                                                   long maxTimeDifferenceForPairing)
    {
        ObservedForecastMeasurementPair pair = null;

        IrregularTimeSeries choppedObservedTs = observedTs.getSubTimeSeries(
                targetTime, observedTs.getEndTime());

        AbsTimeMeasurement currentObsMeasurement = null;
        AbsTimeMeasurement forecastMeasurement = null;

        boolean done = false;
        long obsTime = 0;
        // the measurements returned from getAbsTimeMeasurementByIndex(index)
        // are in ascending order
        // from earliest to latest in time

        // start near or at targetTime and work forward in time
        for (int i = 0; (!done && i < choppedObservedTs.getMeasurementCount()); i++)
        {
            currentObsMeasurement = choppedObservedTs.getAbsTimeMeasurementByIndex(i);
            obsTime = currentObsMeasurement.getTime();

            forecastMeasurement = 
                findClosestForecastByObsTimeAtOrAfterTargetTime(forecastTs, 
                        										obsTime, 
                        										targetTime);

            if (areMeasurementsCloseEnoughInTime(forecastMeasurement,
                    currentObsMeasurement, maxTimeDifferenceForPairing))
            {

                pair = new ObservedForecastMeasurementPair(
                        currentObsMeasurement, forecastMeasurement);

                done = true;
            }

            else
            //keep looking
            {

            }
        } //end for


        return pair;

    }
             
    // ---------------------------------------------------------------------------------------------------
    private AbsTimeMeasurement findClosestForecastByObsTimeAtOrAfterTargetTime(RegularTimeSeries forecastTs,
                                                                                long obsTime, long targetTime)
        {
            //find the closest forecast (by time) to the obstime.
            //The forecast found must be at or after the targetTime
            
            boolean done = false;
            boolean firstTime = true;
            long smallestTimeDiff = 0;
            long timeDiff = 0;
            AbsTimeMeasurement bestFcstMeasurement = null;
            
            
            for (int i = 0; !done && i < forecastTs.getMeasurementCount(); i++)
            {
                
                AbsTimeMeasurement fcstMeasurement = forecastTs.getAbsTimeMeasurementByIndex(i);
                long fcstTime = fcstMeasurement.getTime();
                
                if (fcstTime >= targetTime)
                {
                    if ( firstTime ) //first time, do some initialization
                    {
                        smallestTimeDiff = Math.abs(obsTime - fcstTime);
                        bestFcstMeasurement = fcstMeasurement;
                        firstTime = false;
                    }
                    else //not the first iteration
                    {
                        timeDiff = Math.abs(obsTime - fcstTime);
                        
                        if (timeDiff < smallestTimeDiff)
                        {
                            smallestTimeDiff = timeDiff;
                            bestFcstMeasurement = fcstMeasurement;
                        }
                        else //we are getting farther from the obsTime, so stop
                        {
                           done = true;
                        }
                    }
                    
                }
                else //fcstTime < targetTime, so we need to just keep iterating
                {
                    // do nothing
                }
                
            }
            
            return bestFcstMeasurement;
        }
        
    // -----------------------------------------------------------------------------------------------
    
    private boolean areMeasurementsCloseEnoughInTime(AbsTimeMeasurement m1, 
                                                     AbsTimeMeasurement m2,
                                                     long maxMillisForCloseness)
    {
        boolean result = false;
          
        if ((m1 != null) && (m2 != null))
        {      
            long timeDiff = m1.getTime() - m2.getTime();

            if (Math.abs(timeDiff) <= maxMillisForCloseness)
            {
                result = true;
            }
        }
        
        return result;
    }
    // ----------------------------------------------------------------------------------
    public static AbsTimeMeasurement interpolate(AbsTimeMeasurement m1,
                                                 AbsTimeMeasurement m2,
                                                 long desiredTime)
    {
        long t1 = m1.getTime();
        long t2 = m2.getTime();

        double value1 = m1.getValue();
        double value2 = m2.getValue();

        double slope = (value2 - value1) / (t2 - t1);
        double intercept = (value1) - (slope * t1);

        double value = (slope * desiredTime) + intercept;
        AbsTimeMeasurement measurement = new AbsTimeMeasurement(value,
                desiredTime, m1.getUnit());

        measurement.setIsInterpolated(true);

        return measurement;
    }
    
    // ----------------------------------------------------------------------------------
    

    public ForecastAdjusterParams getParams()
    {
        return _params;
    }
    
    // ----------------------------------------------------------------------------------
   
    public void setParams(ForecastAdjusterParams params)
    {
        _params = params;
    }
   
    // ----------------------------------------------------------------------------------
    public static void main(String[] argStringArray)
    {
        MeasuringUnit dischargeUnit = MeasuringUnit.cfs;
        ForecastAdjusterParams params = new ForecastAdjusterParams();
        params.setBlendingHours(5);
        params.setBlendingMethod(ForecastInterpolationMethod.DIFFERENCE);
        params.setShouldDoAdjustment(true);
        
        ForecastAdjuster adjuster = new ForecastAdjuster(params);
        
        long fcstStartTime = System.currentTimeMillis();
        long fcstEndTime = fcstStartTime + (7 * 24 * MILLIS_PER_HOUR); //7 days
        
        long obsStartTime = fcstStartTime - (3 * 24 * MILLIS_PER_HOUR); //3 days before fcst start
        long obsEndTime = fcstStartTime + (7 * MILLIS_PER_HOUR); // 7 hours after fcst start
        
        IrregularTimeSeries observedTs = new IrregularTimeSeries(dischargeUnit);
        RegularTimeSeries forecastTs = 
            	new RegularTimeSeries(fcstStartTime, fcstEndTime, 1, dischargeUnit);
        
    

        // load up the forecast time series
        double[] fcstMeasurementValueArray = {20, 21, 22, 24, 26, 30, 35, 40, 41, 42, 43, 45, 55, 67, 43, 32, 31, 30, 29, 28, 27, 26, 25  };
        
        int index = 0;
        
        for (long time = fcstStartTime; time <= fcstEndTime; time+= MILLIS_PER_HOUR)
        {
            Measurement m = new Measurement(fcstMeasurementValueArray[index], dischargeUnit);
            forecastTs.setMeasurementByTime(m, time);    
            
            index++;
            if (index == fcstMeasurementValueArray.length)
            {
                index = 0;
            }
        }
  
        
        // load up the observed time series
          index = 0;
        double[] obsMeasurementValueArray =  {18, 19, 20, 22, 24, 28, 33, 28, 39, 40, 41, 43, 53, 57, 41, 29, 29, 28, 27, 26, 26, 24, 28  };
          
        long MILLIS_PER_MINUTE = 1000 * 60;
        for (long time = obsStartTime; time <= obsEndTime; time+= 11 * MILLIS_PER_MINUTE)
        {
            AbsTimeMeasurement m = new AbsTimeMeasurement(obsMeasurementValueArray[index], time,  dischargeUnit);
            
            observedTs.insertMeasurement(m);    
            
            index++;
            if (index == obsMeasurementValueArray.length)
            {
                index = 0;
            }
        }
 
        
        RegularTimeSeries adjustedForecastTs = null;
        
        adjustedForecastTs  = adjuster.getAdjustedTimeSeries(observedTs, forecastTs);
        
        
        //print results
        System.out.println("observedTs = " + observedTs + "\n");
        
        System.out.println("original forecast Ts = " + forecastTs + "\n");
        System.out.println("adjusted forecast Ts = " + adjustedForecastTs);
     
    }
    
    // ----------------------------------------------------------------------------------
     
    /**
     * @param lastObservedTimeToUseAsInput The lastObservedTimeToUseAsInput to set.
     */
    public void setLastObservedTimeToUseAsInput(long lastObservedTimeToUseAsInput)
    {
    //    String header = " ForecastAdjuster.setLastObservedTimeToUseAsInput()";
     //   System.out.println(header + "lastObservedTimeToUseAsInput = " + 
    //            DbTimeHelper.getDateTimeStringFromLongTime(lastObservedTimeToUseAsInput));
        _lastObservedTimeToUseAsInput = lastObservedTimeToUseAsInput;
    }

    // ----------------------------------------------------------------------------------
    
    /**
     * @return Returns the lastObservedTimeToUseAsInput.
     */
    public long getLastObservedTimeToUseAsInput()
    { 
      //  String header = " ForecastAdjuster.getLastObservedTimeToUseAsInput()";
      //  System.out.println(header + "lastObservedTimeToUseAsInput = " + 
      //          DbTimeHelper.getDateTimeStringFromLongTime(_lastObservedTimeToUseAsInput));
        return _lastObservedTimeToUseAsInput;
    }
    // ----------------------------------------------------------------------------------
    

    private class BlendingData
    {
        
        boolean _usedBlend = false;
        int _blendHoursCount = 0;
        double _blendAmount = 0.0;
   
        
        
 
    }
    // ----------------------------------------------------------------------------------
     
   
} //end ForecastAdjuster
