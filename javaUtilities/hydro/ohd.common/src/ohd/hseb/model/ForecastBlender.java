/*
 * Created on Sep 23, 2004
 *
 * 
 */
package ohd.hseb.model;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;

/**
 * @author Chip Gobs
 *
 *  Forecast Blender
 */
public class ForecastBlender
{

    private int _blendHoursCount = 0;
    private double _baseBlendAmount = 0;
    private boolean _usedBlend = false;
    
    private int _allowableBlendingHours = 0;
       
    private long _allowableTimeWindow = 0;
    
    private static final long MILLIS_PER_MINUTE = 60 * 1000;
    private static final long MILLIS_PER_HOUR = 60 * MILLIS_PER_MINUTE;
    
    private static boolean _debug = false;
    
    // -----------------------------------------------------------------------------------------
   public ForecastBlender(int allowableBlendingHours)	
   {
       
       _blendHoursCount = 0;
       _baseBlendAmount= 0;
       _usedBlend = false;
       
       _allowableTimeWindow = allowableBlendingHours * MILLIS_PER_HOUR;
       _allowableBlendingHours = allowableBlendingHours;
   }

   // -----------------------------------------------------------------------------------------
   public void clearBlender()
   {
       _blendHoursCount  = 0;
       _baseBlendAmount = 0;
       _usedBlend = false;
       
   }
   // ----------------------------------------------------------------------------------
   
    /**
     * @param _blendHoursCount The _blendHoursCount to set.
     */
    private void setBlendHoursCount(int blendHoursCount)
    {
        this._blendHoursCount = blendHoursCount;
    }
    
    // -----------------------------------------------------------------------------------------
    /**
     * @return Returns the _blendHoursCount.
     */
    private int getBlendHoursCount()
    {
        return _blendHoursCount;
    }
    // -----------------------------------------------------------------------------------------   
    /**
     * @param _baseBlendAmount The _blendAmount to set.
     */
    private void setBlendAmount(double blendAmount)
    {
        this._baseBlendAmount = blendAmount;
    }
    // -----------------------------------------------------------------------------------------  
    /**
     * @return Returns the _blendAmount.
     */
    private double getBlendAmount()
    {
        return _baseBlendAmount;
    }
    // -----------------------------------------------------------------------------------------
    /**
     * @param _usedBlendForward The _usedBlendForward to set.
     */
    public void setUsedBlend(boolean usedBlend)
    {
        this._usedBlend = usedBlend;
    }
    // -----------------------------------------------------------------------------------------
    /**
     * @return Returns the _usedBlendForward.
     */
    public boolean usedBlend()
    {
        return _usedBlend;
    }
    // -----------------------------------------------------------------------------------------
    
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
    public AbsTimeMeasurement getBlendAdjustment(ObservedForecastMeasurementPair obsFcstPair,
                             AbsTimeMeasurement origFcstMeasurement)
    {
        String header = "ForecastBlender.getBlendAdjustment(): ";
        
        AbsTimeMeasurement adjustmentMeasurement = null;
        AbsTimeMeasurement observedMeasurement = null;
        AbsTimeMeasurement pairedFcstMeasurement = null;

        long fcstTime = origFcstMeasurement.getTime();
        
 
        if (obsFcstPair != null)
        {
            observedMeasurement = obsFcstPair.getObservedMeasurement();
            pairedFcstMeasurement = obsFcstPair.getForecastMeasurement();
            
            if (areMeasurementsCloseEnoughInTime(observedMeasurement,
                    							 origFcstMeasurement,
                    							 _allowableTimeWindow))
            {


                //first time for forward blending of this group of
                // forecast values
                if (! _usedBlend)
                {
                    _usedBlend = true;

                    long pairFcstTime = obsFcstPair.getForecastMeasurement().getTime();
                    long timeDiff = fcstTime - pairFcstTime;

                    _blendHoursCount = (int) (timeDiff / MILLIS_PER_HOUR); 
                    _baseBlendAmount = observedMeasurement.getValue() - pairedFcstMeasurement.getValue();
                
                }

                //          have already usedBlendForward
                else
                {
                    _blendHoursCount ++;
                }

                double absBlendHoursCount = Math.abs(_blendHoursCount);
                
                double fadeFactor = ((double) _allowableBlendingHours - (double) absBlendHoursCount) /
                                    (double) (_allowableBlendingHours);

                double adjustmentValue = (_baseBlendAmount * fadeFactor);
              
                adjustmentMeasurement = new AbsTimeMeasurement(origFcstMeasurement);  //conveniently copies units and time
                adjustmentMeasurement.setValue(adjustmentValue);
                     
                
                if (_debug)
                {    
                    if (_blendHoursCount > 0)
                    {
                        System.out.print(header + "Forward blend for " +
                                                DbTimeHelper.getDateTimeStringFromLongTime(origFcstMeasurement.getTime()) + " ");
                    }
                    else
                    {
                        System.out.print(header + "Backward blend for " +
                                                DbTimeHelper.getDateTimeStringFromLongTime(origFcstMeasurement.getTime()) + " ");
                    }
                    
                    System.out.println("blending adjustment value = " + adjustmentValue);
                    
                    System.out.println(header + "raw blendAmount = " + _baseBlendAmount +
                                       " fadeFactor = " + fadeFactor + 
                                       " blendHoursCount = " + _blendHoursCount + "\n");
                    
                }

            } //end if measurements are close in time
        } //end  if previous pair != null

        
        //make sure that there are no null adjustments,
        // if they are null at this point, make it a 0.0 adjustment
        if (adjustmentMeasurement == null)
        {
            adjustmentMeasurement = new AbsTimeMeasurement(origFcstMeasurement);
            adjustmentMeasurement.setValue(0.0);     
        }
        
        return adjustmentMeasurement;
        
    } //end getBlendAdjustment()
    // -----------------------------------------------------------------------------------------

} //end class  ForecastBlender
