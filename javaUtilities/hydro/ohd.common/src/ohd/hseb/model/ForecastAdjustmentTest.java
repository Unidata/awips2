package ohd.hseb.model;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.measurement.Measurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;

import junit.framework.TestCase;

public class ForecastAdjustmentTest extends TestCase
{

    protected static final long MILLIS_PER_HOUR = 3600 * 1000;
    protected static final long MILLIS_PER_DAY = 24 * MILLIS_PER_HOUR; 

    
    // --------------------------------------------------------------------------------------------------------
    
    public ForecastAdjustmentTest(String name)
    {
        super(name);
    }
    
    // --------------------------------------------------------------------------------------------------------
    
    protected void setUp() throws Exception
    {
        super.setUp();
        
    }

    // --------------------------------------------------------------------------------------------------------
    
    
    protected void tearDown() throws Exception
    {
        super.tearDown();
    }
    
    // --------------------------------------------------------------------------------------------------------
    
    private DateValuePair dvp(String dateTimeString, double value)
    {
        return new DateValuePair(dateTimeString, value);
        
    }
    
    // --------------------------------------------------------------------------------------------------------
    
    
    private class DateValuePair
    {
        private String dateString = null;
        private double value = 0.0;

        public DateValuePair(String dateTimeString, double value)
        {
             setDateString(dateTimeString);
             setValue(value);
        }
        
        public void setDateString(String dateString)
        {
            this.dateString = dateString;
        }
        public String getDateString()
        {
            return dateString;
        }
        public void setValue(double value)
        {
            this.value = value;
        }
        public double getValue()
        {
            return value;
        }
    }
    
    // --------------------------------------------------------------------------------------------------------
    
    private RegularTimeSeries getForecastTimeSeries()
    {
        
        DateValuePair fcstMeasurementValueArray[] = 
        
        {
                dvp("2007-03-01 13:00:00", .25),
                dvp("2007-03-01 14:00:00", .10),
                dvp("2007-03-01 15:00:00", .15),
                dvp("2007-03-01 16:00:00", .50),
                dvp("2007-03-01 17:00:00", .80),
                dvp("2007-03-01 18:00:00", 1.2),
                dvp("2007-03-01 19:00:00", 1.5),
                dvp("2007-03-01 20:00:00", 1.9),
                dvp("2007-03-01 21:00:00", 2.1),
                dvp("2007-03-01 22:00:00", 2.5),
                dvp("2007-03-01 23:00:00", 2.9),
                dvp("2007-03-02 00:00:00", 3.2),
                dvp("2007-03-02 01:00:00", 3.6),
                dvp("2007-03-02 02:00:00", 4.0),
                dvp("2007-03-02 03:00:00", 4.9),
                dvp("2007-03-02 04:00:00", 6.1),
                dvp("2007-03-02 05:00:00", 7.9),
                dvp("2007-03-02 06:00:00", 10.0),
                dvp("2007-03-02 07:00:00", 12.0),
                dvp("2007-03-02 08:00:00", 14.1),
                dvp("2007-03-02 09:00:00", 16.0),
                dvp("2007-03-02 10:00:00", 17.0),
                dvp("2007-03-02 11:00:00", 17.5),
                dvp("2007-03-02 12:00:00", 17.8),
                dvp("2007-03-02 13:00:00", 17.6),
                dvp("2007-03-02 14:00:00", 17.3),
                dvp("2007-03-02 15:00:00", 16.8),
                dvp("2007-03-02 16:00:00", 16.0),
                dvp("2007-03-02 17:00:00", 14.7),
                dvp("2007-03-02 18:00:00", 13.6),
                dvp("2007-03-02 19:00:00", 12.7),
                dvp("2007-03-02 20:00:00", 11.9),
                dvp("2007-03-02 21:00:00", 12.0),
                dvp("2007-03-02 22:00:00", 12.0),
                dvp("2007-03-02 23:00:00", 12.0),
                dvp("2007-03-03 00:00:00", 12.0)
        };
        
        String dateTimeString = "2007-03-06 01:00:00";
        long fcstStartTime = DbTimeHelper.getLongTimeFromDateTimeString(dateTimeString);
        
        long fcstEndTime = getTime("2007-03-03 06:00:00");
 
        MeasuringUnit dischargeUnit = MeasuringUnit.cfs;
        RegularTimeSeries forecastTs = 
            new RegularTimeSeries(fcstStartTime, fcstEndTime, 1, dischargeUnit);

        for (int i = 0; i < fcstMeasurementValueArray.length; i++)
        {
            DateValuePair dvp = fcstMeasurementValueArray[i];
            
            double value = dvp.getValue();
            long time =  DbTimeHelper.getLongTimeFromDateTimeString(dvp.getDateString());
            
            Measurement m = new Measurement(value , dischargeUnit);
            forecastTs.setMeasurementByTime(m, time); 
        }
        
        
        return forecastTs;
        
    }
    
    // --------------------------------------------------------------------------------
    
    private IrregularTimeSeries getObservedTimeSeries()
    {
        
        DateValuePair obsMeasurementValueArray[] = 
        
        {
                dvp("2007-03-01 13:00:00", 4),
                dvp("2007-03-01 14:00:00", 4),
                dvp("2007-03-01 15:00:00", 4),
                dvp("2007-03-01 16:00:00", 4),
                dvp("2007-03-01 17:00:00", 4),
                dvp("2007-03-01 18:00:00", 4),
                dvp("2007-03-01 19:00:00", 4),
                dvp("2007-03-01 20:00:00", 4),
                dvp("2007-03-01 21:00:00", 4),
                dvp("2007-03-01 22:00:00", 4),
                dvp("2007-03-01 23:00:00", 4),
                dvp("2007-03-02 00:00:00", 4),                
                dvp("2007-03-02 01:00:00", 4.2),
                dvp("2007-03-02 02:00:00", 4.4),
               
                dvp("2007-03-02 03:00:00", 4.5),
                dvp("2007-03-02 04:00:00", 4.8),
              //  dvp("2007-03-02 03:00:00", -9999),
              //  dvp("2007-03-02 04:00:00", -9999),
                
                dvp("2007-03-02 05:00:00", 5.0),
                dvp("2007-03-02 06:00:00", 5.2),
                dvp("2007-03-02 07:00:00", 5.4),
                
                dvp("2007-03-02 08:00:00", 10.1),
                dvp("2007-03-02 09:00:00", 10.5),
               // dvp("2007-03-02 08:00:00", -9999),
               // dvp("2007-03-02 09:00:00", -9999),
             
                // test the big gap
                dvp("2007-03-02 10:00:00", 10.8), //17
                //fcst at 11:00 is 17.5  
            //    dvp("2007-03-02 15:50:00", 11.8),
                //fcst at 13:00 is 17.6
                
                 //fcst at 15 is 16.8
                 dvp("2007-03-02 16:00:00", 11.8), //16
                 
                 
                //test a small gap (the size for interpolation)
                dvp("2007-03-02 20:00:00", 10),
                dvp("2007-03-02 23:00:00", 10)
           
        };
      
        MeasuringUnit dischargeUnit = MeasuringUnit.cfs;
        IrregularTimeSeries observedTs = new IrregularTimeSeries(dischargeUnit);
      
        for (int i = 0; i < obsMeasurementValueArray.length; i++)
        {
            DateValuePair dvp = obsMeasurementValueArray[i];
            
            double value = dvp.getValue();
            long time =  DbTimeHelper.getLongTimeFromDateTimeString(dvp.getDateString());
            
            AbsTimeMeasurement m = new AbsTimeMeasurement(value, time, dischargeUnit);
            observedTs.insertMeasurement(m);
        }
  
        return observedTs;
        
    }
    
    // --------------------------------------------------------------------------------

    public void testPairing()
    {
            RegularTimeSeries fcstTimeSeries = getForecastTimeSeries();
            IrregularTimeSeries obsTimeSeries = getObservedTimeSeries();
            
            long lastObsTime = obsTimeSeries.getEndTime();
            
            ForecastAdjuster adjuster = getForecastAdjuster(lastObsTime);
            
            RegularTimeSeries adjustedForecastTs  = adjuster.getAdjustedTimeSeries(obsTimeSeries, fcstTimeSeries);
     
            MeasuringUnit unit = adjustedForecastTs.getMeasuringUnit();
            
            double value = 0;
            
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 00:00:00"), unit);
            assertEquals(4.0, value);
            
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 05:00:00"), unit);
            assertEquals(5.0, value);
        
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 9:00:00"), unit);
            assertEquals(10.5, value);
       
            
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 10:00:00"), unit);
            assertEquals(10.8, value);
            
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 11:00:00"), unit);
            assertEquals(10.8, value);
      
           
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 15:00:00"), unit);
            assertEquals(11.8, value);
      
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 16:00:00"), unit);
            assertEquals("after gap ", 11.8, value);
        
 
            
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 20:00:00"), unit);
            assertEquals("before small gap ", 10.0, value);
       
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 23:00:00"), unit);
            assertEquals("after small gap ", 10.0, value);
       
            
            // System.out.println("observedTs = " + obsTimeSeries + "\n");
           // 
           // System.out.println("original forecast Ts = " + fcstTimeSeries + "\n");
           // System.out.println("adjusted forecast Ts = " + adjustedForecastTs);
            
    }
    
    // ----------------------------------------------------------------
    
    public void testInterpolation()
    {
            RegularTimeSeries fcstTimeSeries = getForecastTimeSeries();
            IrregularTimeSeries obsTimeSeries = getObservedTimeSeries();
            
            long lastObsTime = obsTimeSeries.getEndTime();
            
            ForecastAdjuster adjuster = getForecastAdjuster(lastObsTime);
    
            
            RegularTimeSeries adjustedForecastTs  = adjuster.getAdjustedTimeSeries(obsTimeSeries, fcstTimeSeries);
     
            MeasuringUnit unit = adjustedForecastTs.getMeasuringUnit();
            
            double value = 0;
            
            value = adjustedForecastTs.getMeasurementValueByTime(getTime("2007-03-02 19:00:00"), unit);
            assertEquals(10.0, value);            
    }
    
    // --------------------------------------------------------------------------------------------------------
    private double getDesiredDoubleMatchedBlendedValue(ForecastAdjuster adjuster,
                                          RegularTimeSeries fcstTimeSeries,
                                          IrregularTimeSeries obsTimeSeries,
                                          String prevForecastTimeString, String prevObsTimeString,
                                          String nextForecastTimeString, String nextObsTimeString,
                                          String targetForecastTimeString,
                                          int targetHoursFromPrevAdjustment,
                                          int targetHoursFromNextAdjustment)
    {
        
        String header = "ForecastAdjustmentTest.getDesiredBlendedValue(): ";
        MeasuringUnit unit = fcstTimeSeries.getMeasuringUnit();
        
        double fcstValue = fcstTimeSeries.getMeasurementValueByTime(getTime(targetForecastTimeString), unit);
        
        
        //setup of previous pair
        AbsTimeMeasurement prevObsMeasurement = obsTimeSeries.findClosestMeasurementByTime(getTime(prevObsTimeString));       
        double prevObsValue = prevObsMeasurement.getValue(unit);
        double prevFcstValue = fcstTimeSeries.getMeasurementValueByTime(getTime(prevForecastTimeString), unit);
        double prevAdjustment = prevObsValue - prevFcstValue;
                                   
        
        //setup of next pair
        AbsTimeMeasurement nextObsMeasurement = obsTimeSeries.findClosestMeasurementByTime(getTime(nextObsTimeString));
        double nextObsValue = nextObsMeasurement.getValue(unit);
        double nextFcstValue = fcstTimeSeries.getMeasurementValueByTime(getTime(nextForecastTimeString), unit);
        double nextAdjustment = nextObsValue  - nextFcstValue;
          
        
        // calculate the blend
        double blendingHours = adjuster.getParams().getBlendingHours();
  
        double forwardFadeFactor = (blendingHours-targetHoursFromPrevAdjustment)/(blendingHours);
        double forwardBlend =  prevAdjustment * forwardFadeFactor;
        
        double backwardFadeFactor = (blendingHours-targetHoursFromNextAdjustment)/(blendingHours);
        double backwardBlend =  nextAdjustment * backwardFadeFactor ;
        
        double totalBlend = (forwardBlend + backwardBlend) / 2;
        
        // add the blend shift of the original forecast value
        double newDesiredValue = fcstValue + totalBlend;
        
        
        System.out.println(header + "forwardBlend  " + forwardBlend + 
                " backwardBlend =  " + backwardBlend + 
                " totalBlend = " + totalBlend + 
                " newDesired Value = " + newDesiredValue);

        
        return newDesiredValue;
    }
    //  --------------------------------------------------------------------------------------------------------
    
    
    public void testBlending()
    {
        
        String header = "ForecastAdjustmentTest.testBlending(): ";
        
            RegularTimeSeries fcstTimeSeries = getForecastTimeSeries();
            IrregularTimeSeries obsTimeSeries = getObservedTimeSeries();
            
            long lastObsTime = obsTimeSeries.getEndTime();
            
            ForecastAdjuster adjuster = getForecastAdjuster(lastObsTime);
    
            
            RegularTimeSeries adjustedForecastTs  = adjuster.getAdjustedTimeSeries(obsTimeSeries, fcstTimeSeries);
     
            MeasuringUnit unit = adjustedForecastTs.getMeasuringUnit();
            
            String targetTimeString = "2007-03-02 13:00:00";
            double value = adjustedForecastTs.getMeasurementValueByTime(getTime(targetTimeString), unit);
            
            double newDesiredValue = getDesiredDoubleMatchedBlendedValue(
                    adjuster, fcstTimeSeries, obsTimeSeries, 
                    "2007-03-02 11:00:00",  "2007-03-02 10:00:00", //fcst can match an obs up to 70 minutes away
                    "2007-03-02 15:00:00", "2007-03-02 16:00:00",  //fcst can match an obs up to 70 minutes away
                    targetTimeString,  //target time
                    2,
                    2);
            
            System.out.println(header + "newDesiredValue = " + newDesiredValue);
            
            
            assertEquals(newDesiredValue, value);
            
            
            targetTimeString = "2007-03-02 14:00:00";
            value = adjustedForecastTs.getMeasurementValueByTime(getTime(targetTimeString), unit);
            
            newDesiredValue = getDesiredDoubleMatchedBlendedValue(
                    adjuster, fcstTimeSeries, obsTimeSeries, 
                    "2007-03-02 11:00:00",  "2007-03-02 10:00:00", //fcst can match an obs up to 70 minutes away
                    "2007-03-02 15:00:00", "2007-03-02 16:00:00",  //fcst can match an obs up to 70 minutes away
                    targetTimeString,  //target time
                    3,
                    1);
            
            System.out.println(header + "newDesiredValue = " + newDesiredValue);
            
            
            assertEquals(newDesiredValue, value);
            
            return;
          
             
    }
    
    // --------------------------------------------------------------------------------------------------------
    
    private long getTime(String dateTimeString)
    {       
        return DbTimeHelper.getLongTimeFromDateTimeString(dateTimeString);
    }
   
    // --------------------------------------------------------------------------------------------------------
       
    private ForecastAdjuster getForecastAdjuster(long lastObsTime)
    {
            
        ForecastAdjusterParams params = new ForecastAdjusterParams();
        
        int pairingTimeMinutes = 70;
        int interpolationHours = 3;
        
        
        params.setBlendingHours(100);
        params.setBlendingMethod(ForecastInterpolationMethod.DIFFERENCE);
        params.setShouldDoAdjustment(true);
        params.setPairingTimeMinutes(pairingTimeMinutes);
        params.setInterpolationHours(interpolationHours);
        System.out.println("pairing minutes hours = " + params.getPairingTimeMinutes());
        System.out.println("Interpolation hours = " + params.getInterpolationHours());
        
        
        ForecastAdjuster adjuster = new ForecastAdjuster(params);
        
        adjuster.setLastObservedTimeToUseAsInput(lastObsTime);
        
        
        return adjuster;
    }
    
    // --------------------------------------------------------------------------------------------------------
     

}
