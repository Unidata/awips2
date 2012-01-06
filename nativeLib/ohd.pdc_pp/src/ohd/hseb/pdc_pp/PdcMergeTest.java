package ohd.hseb.pdc_pp;

import java.util.ArrayList;
import java.util.List;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;

import junit.framework.TestCase;

public class PdcMergeTest extends TestCase
{
    public static final long MILLIS_PER_HALF_HOUR = 30 * 60 * 1000;
    public static final long MILLIS_PER_HOUR = 60 * 60 * 1000;
    public static final long MILLIS_PER_DAY = MILLIS_PER_HOUR * 24;
    public static final long MILLIS_PER_12_HOURS = MILLIS_PER_HOUR * 12;
    public static final long MILLIS_PER_MINUTE = 60 * 1000;
   

    private String pdc_pp_dirValue = "./pdctest/";
    
    private RegularObsTimeSeries _cached3HourTimeSeries = null;
    private RegularObsTimeSeries _fresh3HourTimeSeries = null;
    private RegularObsTimeSeries _expectedMerged3HourTimeSeries = null;
    
    private RegularObsTimeSeries _cached24HourTimeSeries = null;
    private RegularObsTimeSeries _fresh24HourTimeSeries = null;
    private RegularObsTimeSeries _expectedMerged24HourTimeSeries = null;
    
    private PDCPreprocessorDataMgr _dataMgr = null;
      
    // 3 hour variables
    private long _3HourStartTime = _3HourCachedTimeValuePairArray[0].getDateTime();
    private String _3HourStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_3HourStartTime);
    private String _3HourEndTimeString = "2007-01-02 05:15:00";
    private long _3HourEndTime = DbTimeHelper.getLongTimeFromDateTimeString(_3HourEndTimeString);
    private int _3HourLookBackWindow = 24;
 
          
    // 24 hour variables
    private long _24HourStartTime = _24HourCachedTimeValuePairArray[0].getDateTime();
    private String _24HourStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_24HourStartTime);
    private String _24HourEndTimeString = "2007-01-11 03:07:00";
    private long _24HourEndTime = DbTimeHelper.getLongTimeFromDateTimeString(_24HourEndTimeString);
    private int _24HourLookBackWindow = 240;

    
     private static TimeValuePair[] _3HourCachedTimeValuePairArray =  
    {
        tvp("2007-01-01 00:00:00", 1.0),
        tvp("2007-01-01 03:00:00", 2.0),
        tvp("2007-01-01 06:00:00", 3.0),
        tvp("2007-01-01 09:00:00", 4.0),
        tvp("2007-01-01 12:00:00", 5.0),
        tvp("2007-01-01 15:00:00", 6.0),
        tvp("2007-01-01 18:00:00", 7.0),
        tvp("2007-01-01 21:00:00", 8.0),
        tvp("2007-01-02 00:00:00", 9.0),
        tvp("2007-01-02 03:00:00", 10.0)
    };

    private static TimeValuePair[] _3HourFreshTimeValuePairArray =  
    {
      //  tvp("2007-01-01 00:00:00", 1.0),
      //  tvp("2007-01-01 03:00:00", 2.0),
     //   tvp("2007-01-01 06:00:00", 3.0),
     //   tvp("2007-01-01 09:00:00", 4.0),
    //    tvp("2007-01-01 12:00:00", 5.0),
    //    tvp("2007-01-01 15:00:00", 6.0),
    //    tvp("2007-01-01 18:00:00", 7.0),
        tvp("2007-01-01 21:00:00", 8.0),
        tvp("2007-01-02 00:00:00", 9.0),
        tvp("2007-01-02 03:00:00", 11.0),
        tvp("2007-01-02 06:00:00", 5.5)
    };
    
    private static TimeValuePair[] _3HourExpectedMergedTimeValuePairArray =  
    {
      //  tvp("2007-01-01 00:00:00", 1.0),
    //    tvp("2007-01-01 03:00:00", 2.0),
        tvp("2007-01-01 06:00:00", 3.0),
        tvp("2007-01-01 09:00:00", 4.0),
        tvp("2007-01-01 12:00:00", 5.0),
        tvp("2007-01-01 15:00:00", 6.0),
        tvp("2007-01-01 18:00:00", 7.0),
        tvp("2007-01-01 21:00:00", 8.0),
        tvp("2007-01-02 00:00:00", 9.0),
        tvp("2007-01-02 03:00:00", 11.0),
        tvp("2007-01-02 06:00:00", 5.5)
    };
    
    
    private static TimeValuePair[] _24HourCachedTimeValuePairArray =  
    {
        tvp("2007-01-01 12:00:00", 1.0),
        tvp("2007-01-02 12:00:00", 2.0),
        tvp("2007-01-03 12:00:00", 3.0),
        tvp("2007-01-04 12:00:00", 4.0),
        tvp("2007-01-05 12:00:00", 5.0),
        tvp("2007-01-06 12:00:00", 6.0),
        tvp("2007-01-07 12:00:00", 7.0),
        tvp("2007-01-08 12:00:00", 8.0),
       // tvp("2007-01-09 12:00:00", 9.0),
       // tvp("2007-01-10 12:00:00", 10.0)
    };

    private static TimeValuePair[] _24HourFreshTimeValuePairArray =  
    {
       // tvp("2007-01-01 12:00:00", 1.0),
      //  tvp("2007-01-02 12:00:00", 2.0),
      //  tvp("2007-01-03 12:00:00", 3.0),
      //  tvp("2007-01-04 12:00:00", 4.0),
      //  tvp("2007-01-05 12:00:00", 5.0),
        tvp("2007-01-06 12:00:00", 6.0),
        tvp("2007-01-07 12:00:00", 7.0),
        tvp("2007-01-08 12:00:00", 8.0),
        tvp("2007-01-09 12:00:00", 9.0),
        tvp("2007-01-10 12:00:00", 11.0),
        tvp("2007-01-11 12:00:00", 5.5)
    };
    
    private static TimeValuePair[] _24HourExpectedMergedTimeValuePairArray =  
    {
        tvp("2007-01-01 12:00:00", 1.0),
        tvp("2007-01-02 12:00:00", 2.0),
        tvp("2007-01-03 12:00:00", 3.0),
        tvp("2007-01-04 12:00:00", 4.0),
        tvp("2007-01-05 12:00:00", 5.0),
        tvp("2007-01-06 12:00:00", 6.0),
        tvp("2007-01-07 12:00:00", 7.0),
        tvp("2007-01-08 12:00:00", 8.0),
        tvp("2007-01-09 12:00:00", 9.0),
        tvp("2007-01-10 12:00:00", 11.0),
        tvp("2007-01-11 12:00:00", 5.5)
    };
    
    //  --------------------------------------------------------------------------------------------------------  
  
   
    public PdcMergeTest()
    {
        AppsDefaults ad = new AppsDefaults();
        
        String preprocessedFilePath = ad.getToken( "pdc_pp_dir" ) + "/";
        String logDirName = ad.getToken( "pdc_pp_log_dir" ) + "/";
        
        Logger logger = new FileLogger(logDirName + "PdcMergeTest.log");
  
        String connectionString = "jdbc:postgresql://dx1-nhdr:5432/hd_ob81fwr?user=pguser";
        
        _dataMgr = new PDCPreprocessorDataMgr(connectionString, logger, preprocessedFilePath,  5, 5, true);

        return;
    }
    
    //  --------------------------------------------------------------------------------------------------------  
    protected void setUp() throws Exception
    {
        super.setUp();
        
        createCachedTimeSeries();
        createFreshTimeSeries();       
        createExpectedMergedTimeSeries(); 
     
    }
    // --------------------------------------------------------------------------------------------------------
    
    
    protected void tearDown() throws Exception
    {
        super.tearDown();
    }
    
    // --------------------------------------------------------------------------------------------------------
     
    private RegularObsTimeSeriesDescriptor getRegularObsTimeSeriesDescriptor(String lid, 
                                                                             String startTimeString, 
                                                                             String endTimeString)
    {
        RegularObsTimeSeriesDescriptor descriptor = new RegularObsTimeSeriesDescriptor();
        descriptor.setLid(lid);
        descriptor.setPe("PP");
        descriptor.setDur((short) 1001);
        descriptor.setTs("RG");
        descriptor.setExtremum("Z");     
         
        long startTime = DbTimeHelper.getLongTimeFromDateTimeString(startTimeString);
        descriptor.setStartTime(startTime);
        
        long endTime = DbTimeHelper.getLongTimeFromDateTimeString(endTimeString);
        descriptor.setEndTime(endTime);
   
        
        return descriptor;
    }
    // --------------------------------------------------------------------------------------------------------
    private RegularObsTimeSeriesDescriptor getRegularObsTimeSeriesDescriptor(String lid, 
            long  startTime, 
            long endTime)
    {
        RegularObsTimeSeriesDescriptor descriptor = new RegularObsTimeSeriesDescriptor();
        descriptor.setLid(lid);
        descriptor.setPe("PP");
        descriptor.setDur((short) 1001);
        descriptor.setTs("RG");
        descriptor.setExtremum("Z");     

        descriptor.setStartTime(startTime);
        descriptor.setEndTime(endTime);


        return descriptor;
    }
// --------------------------------------------------------------------------------------------------------

    private void addPairs(TimeValuePair[] timeValuePairArray, RegularObsTimeSeries regularObsTimeSeries)
    {
        String header = "PdcMergeTest.addPairs(): ";
        for (int i = 0; i < timeValuePairArray.length; i++)
        {
            TimeValuePair tvp = timeValuePairArray[i];
            if (tvp != null)
            {
                regularObsTimeSeries.addTimeValuePairIfBetterMatch(tvp);
            }
            else
            {
                System.out.println(header + "tvp is null");
            }
        }
        
    }
    
    // --------------------------------------------------------------------------------------------------------
    private RegularObsTimeSeries createTimeSeries(String lid, int durationInHours, 
                                                  TimeValuePair[] timeValuePairArray )
    {
        //TimeSlotPolicy timeSlotPolicy = new HourlyTimeSlotPolicy();
        TimeSlotPolicy timeSlotPolicy = null;
        
        TimeValuePair firstPair = timeValuePairArray[0];
        TimeValuePair lastPair = timeValuePairArray[timeValuePairArray.length-1];
             
        long startTime = firstPair.getDateTime();
        long endTime = lastPair.getDateTime();
        
        if (durationInHours == 3)
        {
             timeSlotPolicy = new HourlyTimeSlotPolicy();
        }
        else if (durationInHours == 24)
        {
             timeSlotPolicy = new DailyTimeSlotPolicy();
        }
        
        
        RegularObsTimeSeriesDescriptor descriptor = getRegularObsTimeSeriesDescriptor(lid, startTime, endTime);
        
        RegularObsTimeSeries timeSeries  = 
            new RegularObsTimeSeries(descriptor, durationInHours, timeSlotPolicy);
             
        addPairs(timeValuePairArray, timeSeries);
        
        return timeSeries;
           
    }    
      // --------------------------------------------------------------------------------------------------------
      
    private void createCachedTimeSeries()
    {
         _cached3HourTimeSeries = createTimeSeries("TEST1", 3,  _3HourCachedTimeValuePairArray);  
         _cached24HourTimeSeries = createTimeSeries("TEST1", 24, _24HourCachedTimeValuePairArray);
    }    
    
    // --------------------------------------------------------------------------------------------------------
    
    private void createFreshTimeSeries()
    {
        _fresh3HourTimeSeries = createTimeSeries("TEST1", 3,  _3HourFreshTimeValuePairArray);  
        _fresh24HourTimeSeries = createTimeSeries("TEST1", 24,  _24HourFreshTimeValuePairArray);   
    }
    
    // --------------------------------------------------------------------------------------------------------
   
    private void createExpectedMergedTimeSeries()
    {
        _expectedMerged3HourTimeSeries = createTimeSeries("TEST1", 3, _3HourExpectedMergedTimeValuePairArray);  
        _expectedMerged24HourTimeSeries = createTimeSeries("TEST1", 24,  _24HourExpectedMergedTimeValuePairArray);   
    }
    
    // --------------------------------------------------------------------------------------------------------

    public void innerTestMerge(final long endTime,
                               int timeStepInHours, int numHoursToPreprocess, 
                               RegularObsTimeSeries cachedTimeSeries,
                               RegularObsTimeSeries freshTimeSeries,
                               RegularObsTimeSeries expectedMergedTimeSeries)
    {
              
        String header = "PdcMergeTest.innerTestMerge(): ";
        
        //The current PDC merge way
       long newStartTime = _dataMgr.getNewStartTime(endTime, timeStepInHours, numHoursToPreprocess);
       long newEndTime = _dataMgr.getNewEndTime(endTime, timeStepInHours);
         
       // long newStartTime = getNewStartTime(endTime, timeStepInHours, numHoursToPreprocess);
       // long newEndTime = getNewEndTime(endTime, timeStepInHours);
        
       String originalEndTimeString = DbTimeHelper.getDateTimeStringFromLongTime(endTime);
       
       String newStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(newStartTime); 
       String newEndTimeString = DbTimeHelper.getDateTimeStringFromLongTime(newEndTime);
       
       System.out.println(header + "original end time = " + originalEndTimeString);
       System.out.println(header + " " + 
               timeStepInHours + "-hour timestep,  newStartTime = " +
               newStartTimeString + " newEndTime = " + newEndTimeString);
     
        //These lists of time series (plural) will have only 1 time series each
        List freshList = new ArrayList();
        freshList.add(freshTimeSeries);
        
        List cachedList = new ArrayList();
        cachedList.add(cachedTimeSeries);
        
          
        List mergedList = TimeSeriesListMgr.mergeTimeSeriesLists(freshList, cachedList, newStartTime, newEndTime);
        RegularObsTimeSeries mergedTs = null;
        
        //get out the 1 merged time series that we are expecting
        for (int i = 0; i < mergedList.size(); i++)
        {
            mergedTs = (RegularObsTimeSeries) mergedList.get(i);
        }
        
        //check that the merged list still has only 1 time series
        assertEquals(1, mergedList.size());
        
        //verify that the name of the time series is correct
        String lid = mergedTs.getDescriptor().getLid();
        assertEquals("TEST1", lid);
        
    
        //verify that there are contents each time series
   
        boolean biggerThanZero = false;
        
        //fresh
        biggerThanZero = freshTimeSeries.getTimeValuePairList(true).size() > 0;
        assertEquals(true, biggerThanZero);
        
        //cached
        biggerThanZero = cachedTimeSeries.getTimeValuePairList(true).size() > 0;
        assertEquals(true, biggerThanZero);
        
        //merged
        biggerThanZero = mergedTs.getTimeValuePairList(true).size() > 0;
        assertEquals(true, biggerThanZero);
 
        
        // verify that the fresh and cached are not equal
        boolean freshTsAndCachedEqual = regularTimeSeriesEqual(freshTimeSeries, cachedTimeSeries);
        assertEquals(false, freshTsAndCachedEqual);
             
        // verify that the fresh and mergedTs are equal, since mergeTs really IS freshTs
        boolean mergeTsAndFreshEqual = regularTimeSeriesEqual(mergedTs, freshTimeSeries); 
        assertEquals(true, mergeTsAndFreshEqual);
        
        
        
           
        // verify that the cached and mergeTs are not equal
        boolean mergeTsAndCachedEqual = regularTimeSeriesEqual(mergedTs, cachedTimeSeries);
        assertEquals(false, mergeTsAndCachedEqual);
        
        // verify that the mergedTs is equal to the expectedMergedTimeSeries
          
        boolean mergedTsAndExpectedMergedTsEqual = regularTimeSeriesEqual(mergedTs, expectedMergedTimeSeries);
        if (! mergedTsAndExpectedMergedTsEqual)
        {
            System.out.println(" mergedTs = ");
            System.out.println(mergedTs);
            
            System.out.println("expected mergedTs = ");
            System.out.println(expectedMergedTimeSeries + "\n");
        }
        assertEquals(true, mergedTsAndExpectedMergedTsEqual);
        
    }
    // --------------------------------------------------------------------------------------------------------
  
    public void test3HourMerge()
    {
        innerTestMerge(_3HourEndTime, 3, _3HourLookBackWindow, 
                      _cached3HourTimeSeries,
                      _fresh3HourTimeSeries,
                      _expectedMerged3HourTimeSeries);  
        
        long lastValue = _fresh3HourTimeSeries.getLatestTimeValue(-9999);
        long expectedLastValue = DbTimeHelper.getLongTimeFromDateTimeString("2007-01-02 06:00:00");
        assertEquals(expectedLastValue, lastValue);
    }
    // --------------------------------------------------------------------------------------------------------
    
    public void testGetNewEndTime()
    {
         innerTestGetNewEndTime("2007-01-01 12:00:00", "2007-01-01 00:00:00", 24);
         innerTestGetNewEndTime("2007-01-01 12:00:00", "2007-01-01 03:00:00", 24);
         innerTestGetNewEndTime("2007-01-01 12:00:00", "2007-01-01 11:00:00", 24);
         innerTestGetNewEndTime("2007-01-02 12:00:00", "2007-01-01 12:00:00", 24);
         innerTestGetNewEndTime("2007-01-02 12:00:00", "2007-01-01 13:00:00", 24);
         innerTestGetNewEndTime("2007-01-02 12:00:00", "2007-01-01 16:00:00", 24);
    }
    
    // --------------------------------------------------------------------------------------------------------
    
    
    public void innerTestGetNewEndTime(String expectedDateTimeString,
                                       String originalDateTimeString,
                                       int timeStepInHours)
    {
        
        String header = "PdcMergeTest.innerTestGenNewEndTime(): ";
        
        long origEndTime = DbTimeHelper.getLongTimeFromDateTimeString(originalDateTimeString);
        long expectedEndTime = DbTimeHelper.getLongTimeFromDateTimeString(expectedDateTimeString);
        
        long newEndTime = _dataMgr.getNewEndTime(origEndTime, timeStepInHours);
        String newEndDateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(newEndTime);
        
        if (expectedEndTime != newEndTime)
        {
            System.out.println(header + "originalDateTimeString = " + originalDateTimeString  +
                                        "\n expectedEndTime = " + expectedDateTimeString + " while the " +
                                        "\n actual new end time is  " + newEndDateTimeString);
        }
        
        assertEquals(expectedEndTime, newEndTime);
         
    }
    
    public void test24HourMerge()
    {
        innerTestMerge(_24HourEndTime, 24, _24HourLookBackWindow,
                       _cached24HourTimeSeries,
                       _fresh24HourTimeSeries,
                       _expectedMerged24HourTimeSeries);
        
        long lastValue = _fresh24HourTimeSeries.getLatestTimeValue(-9999);
        System.out.println("24 hour last value = " + DbTimeHelper.getDateTimeStringFromLongTime(lastValue));
        long expectedLastValue = DbTimeHelper.getLongTimeFromDateTimeString("2007-01-11 12:00:00");
        assertEquals(expectedLastValue, lastValue);
    }
    // --------------------------------------------------------------------------------------------------------
    
    private boolean regularTimeSeriesEqual(RegularObsTimeSeries rts1, RegularObsTimeSeries rts2)
    {
        boolean result = true;
        
        List tvpList1 = rts1.getTimeValuePairList(true);
        List tvpList2 = rts2.getTimeValuePairList(true);
        
        
    //    System.out.println("tvpList1.size() = " + tvpList1.size());
    //    System.out.println("tvpList2.size() = " + tvpList2.size());
        
        //check the sizes
        if (tvpList1.size() != tvpList2.size() )
        {
            result = false;
        }

        
        
        
        //if that passed, check the individual parts
        for (int i = 0; result && i < tvpList1.size(); i++)
        {
            TimeValuePair tvp1 = (TimeValuePair) tvpList1.get(i);
            TimeValuePair tvp2 = (TimeValuePair) tvpList2.get(i);
            
            if (mismatchedNullness(tvp1, tvp2))
            {
                result = false;
            }
            
            else if ((tvp1 == null) && (tvp2 == null))
            {
       //         System.out.println("tvp 1 and tvp2 are both null ");
                ; //do nothing
            }
            
            else if (tvp1 == null)
            {
      //          System.out.println("tvp1 is null ");
                ; //do nothing
            }
            
            
            else if (tvp2 == null)
            {
    //            System.out.println("tvp2 is null ");
                ; //do nothing
            }
            
            else if ((tvp1.getValue() != tvp2.getValue()) )
            {
                //System.out.println("tvp 1 = " +  tvp1 +  " tvp2 = " + tvp2);
                result = false; 
            }
            
        }
            
        return result;
    }
    
    
    // --------------------------------------------------------------------------------------------------------
    private boolean mismatchedNullness(TimeValuePair tvp1, TimeValuePair tvp2)
    {
        boolean result = false;
        
        if ((tvp1 == null) && (tvp2 != null))
        {
            result = true;
        }
        else if ( (tvp2 != null) && (tvp2 == null))
        {
            result = true;
        }
        
        return result;
    }
    // --------------------------------------------------------------------------------------------------------
    
    private static TimeValuePair tvp(String dateTimeString, double value)
    {
        long time = DbTimeHelper.getLongTimeFromDateTimeString(dateTimeString);
        return new TimeValuePair(time, value);
        
    }
    
    // --------------------------------------------------------------------------------------------------------
    
    
}
