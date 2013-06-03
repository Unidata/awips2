package ohd.hseb.pdc_pp;

import java.util.List;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;

import junit.framework.TestCase;

public class PdcReadTest extends TestCase
{
    Database _db = null;
    private RegularObsTimeSeries _correct3HourTimeSeries = null;
     
    private RegularObsTimeSeries _correct24HourTimeSeries = null;
    private PDCPreprocessorDataMgr _dataMgr = null;
    
     private static TimeValuePair[] _3HourCorrectTimeValuePairArray =  
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
        tvp("2007-01-02 03:00:00", 11.0)
    };

    private static TimeValuePair[] _24HourCorrectTimeValuePairArray =  
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
        tvp("2007-01-10 12:00:00", 10.0)
    };
        
  
    // 3 hour variables
    private long _3HourStartTime = _3HourCorrectTimeValuePairArray[0].getDateTime();
    private String _3HourStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_3HourStartTime);
    
    private long _3HourEndTime = _3HourCorrectTimeValuePairArray[9].getDateTime();
    private String _3HourEndTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_3HourEndTime);
        
        
    // 24 hour variables
    private long _24HourStartTime = _24HourCorrectTimeValuePairArray[0].getDateTime();
    private String _24HourStartTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_24HourStartTime);
    
    private long _24HourEndTime = _24HourCorrectTimeValuePairArray[9].getDateTime();
    private String _24HourEndTimeString = DbTimeHelper.getDateTimeStringFromLongTime(_24HourEndTime);
        
  
    public PdcReadTest()
    {
          
        AppsDefaults ad = new AppsDefaults();
        
        String preprocessedFilePath = ad.getToken( "pdc_pp_dir" ) + "/";
        String logDirName = ad.getToken( "pdc_pp_log_dir" ) + "/";
        
        Logger logger = new FileLogger(logDirName + "PdcReadTest.log");
  
        String connectionString = "jdbc:postgresql://dx1-nhdr:5432/hd_ob81fwr?user=pguser";
        
        _dataMgr = new PDCPreprocessorDataMgr(connectionString, logger, preprocessedFilePath,  5, 5, true);

        long time = DbTimeHelper.getLongTimeFromDateTimeString("2007-01-01 12:00:00");
        System.out.println("time = " + time);
        
        time = DbTimeHelper.getLongTimeFromDateTimeString(" 2007-01-10 12:00:00");
        System.out.println("time = " + time);
       
        
    }
    //  --------------------------------------------------------------------------------------------------------  
    protected void setUp() throws Exception
    {
        super.setUp();
        
          
        createCorrectTimeSeries();
        
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
    private RegularObsTimeSeries createTimeSeries(String lid, int durationInHours, TimeValuePair[] timeValuePairArray )
    {
        //TimeSlotPolicy timeSlotPolicy = new HourlyTimeSlotPolicy();
        TimeSlotPolicy timeSlotPolicy = null;
        
        String startTimeString = null;
        String endTimeString = null; 
        
        if (durationInHours == 3)
        {
            startTimeString = _3HourStartTimeString;
            endTimeString = _3HourEndTimeString;
        }
        else if (durationInHours == 24)
        {
            startTimeString = _24HourStartTimeString;
            endTimeString = _24HourEndTimeString;
        }
        
        
        RegularObsTimeSeriesDescriptor descriptor = getRegularObsTimeSeriesDescriptor(lid, startTimeString, endTimeString);
        
        RegularObsTimeSeries timeSeries  = 
            new RegularObsTimeSeries(descriptor, durationInHours, timeSlotPolicy);
             
        addPairs(timeValuePairArray, timeSeries);
        
        return timeSeries;
           
    }    
    // --------------------------------------------------------------------------------------------------------

    private void addPairs(TimeValuePair[] timeValuePairArray, RegularObsTimeSeries regularObsTimeSeries)
    {
        for (int i = 0; i < timeValuePairArray.length; i++)
        {
            TimeValuePair tvp = timeValuePairArray[i];
            if (tvp != null)
            {
                regularObsTimeSeries.addTimeValuePairIfBetterMatch(tvp);
            }
        }
        
    }
    
     // --------------------------------------------------------------------------------------------------------
     
      
    private void createCorrectTimeSeries()
    {
         _correct3HourTimeSeries = createTimeSeries("TEST1", 3, _3HourCorrectTimeValuePairArray);  
         _correct24HourTimeSeries = createTimeSeries("TEST1", 24, _24HourCorrectTimeValuePairArray);
    }    
    // --------------------------------------------------------------------------------------------------------
     
    public void innerCFileRead(int timeStepIntervalInHours, int durationCode, RegularObsTimeSeries correctTimeSeries)
    {
       
        long newStartTime = correctTimeSeries.getDescriptor().getStartTime();
        long endTime = correctTimeSeries.getDescriptor().getEndTime();
        
        //These lists of time series (plural) will have only 1 time series each
         
        List readTsList = _dataMgr.readPrecipTimeSeriesListFromCGeneratedFile(timeStepIntervalInHours, durationCode);
        
        RegularObsTimeSeries readTs = null;
        
        for (int i = 0; i < readTsList.size(); i++)
        {
            readTs = (RegularObsTimeSeries) readTsList.get(i);
        }
        
        String lid = readTs.getDescriptor().getLid();
        assertEquals("TEST1", lid);
                   
        boolean readAndCorrectTimeSeriesEqual = regularTimeSeriesEqual(readTs, correctTimeSeries);       
        assertEquals(true, readAndCorrectTimeSeriesEqual);       
        
    }
    // --------------------------------------------------------------------------------------------------------
  
    public void test3HourRead()
    {
        innerCFileRead(3, 1003, _correct3HourTimeSeries);  
    }
    // --------------------------------------------------------------------------------------------------------
    
    public void test24HourRead()
    {
        innerCFileRead(24, 2001, _correct24HourTimeSeries);  
    }
    // --------------------------------------------------------------------------------------------------------
    public void testPDCFileRead()
    {
        CodeTimer timer1 = new CodeTimer();
        CodeTimer timer2 = new CodeTimer();
        
        String fileName = "/awips/hydroapps/ob82/whfs/local/data/pdc_pp/cached1HourPrecip.dat";
        
        
        timer1.start();
        PDCFileReaderSlow reader1 = new PDCFileReaderSlow(fileName);
        List list1 = reader1.read();
        timer1.stop("old PDCFileReaderSlow took");
       
        timer2.start();
        PDCFileReader reader2 = new PDCFileReader(fileName);
   //     reader2.setDebug(true);
        List list2 = reader2.read();
        timer2.stop("new PDCFileReaderSlow took");
   
        
        boolean actualResult = listOfRegularTimeSeriesEqual(list1, list2);
        
        assertEquals(true, actualResult);
        
    }
    // --------------------------------------------------------------------------------------------------------
   private boolean atLeastOneIsNull(Object object1, Object object2)
   {
       boolean result = false;
       
       if ( (object1 == null) || (object2 == null))
       {
           result = true;
       }
     
       return result;
   }
    
    // --------------------------------------------------------------------------------------------------------
   
   private boolean bothAreNull(Object object1, Object object2)
   {
       boolean result = false;
       
       if ( (object1 == null) && (object2 == null))
       {
           result = true;
       }
     
       return result;
   }
    
    // --------------------------------------------------------------------------------------------------------

   
    private boolean listOfRegularTimeSeriesEqual(List list1, List list2)
    {
        boolean result = true;
        
        
        
        if (atLeastOneIsNull(list1, list2))
        {
            if (bothAreNull(list1, list2))
            {
                result = true;
            }
            else //they are different
            {
                result = false;
            }
        }
    
        else if (list1.size() != list2.size())  //neither is null
        {
            result = false;  //size is different, the lists are not the same
        }
        
        else //size is same, need to check some more
        {
            result = true;  //assume they are the same. change if proved false
            
            for (int i = 0; i < list1.size(); i++)
            {
                RegularObsTimeSeries ts1 = (RegularObsTimeSeries) list1.get(i);
                RegularObsTimeSeries ts2 = (RegularObsTimeSeries) list2.get(i);
                
                if (! regularTimeSeriesEqual(ts1, ts2))  
                {
                    result = false; // something is different, so the lists are not the same
                    break;
                    
                }
            }
            
        }
        
        return result;
        
    }
    
    //  --------------------------------------------------------------------------------------------------------
    
    
    private boolean regularTimeSeriesEqual(RegularObsTimeSeries rts1, RegularObsTimeSeries rts2)
    {
        boolean result = true;
        
        List tvpList1 = rts1.getTimeValuePairList(true);
        List tvpList2 = rts2.getTimeValuePairList(true);
        
        
        //System.out.println("tvpList1.size() = " + tvpList1.size());
        
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
            
            else if (tvp1 == null)
            {
                //System.out.print("tvp 1 and tvp2 are both null ");
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
