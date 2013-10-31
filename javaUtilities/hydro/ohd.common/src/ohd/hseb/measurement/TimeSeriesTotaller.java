package ohd.hseb.measurement;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.util.TimeHelper;

public class TimeSeriesTotaller
{
    public final static long MILLIS_PER_HOUR = 1000 * 60 * 60;
    
    // --------------------------------------------------------------------------------
    public static RegularTimeSeries 
                  getRegularTimeSeries(int newIntervalInHours,
                                       int shiftHours,
                                       RegularTimeSeries origTimeSeries) 
    {
        
        String header = "TimeSeriesTotaller.getRegularTimeSeries(): ";
        
        long  intervalInMillis = newIntervalInHours * MILLIS_PER_HOUR;
         
        long startTime = determineStartTime(origTimeSeries.getStartTime(),
                                            newIntervalInHours,
                                            shiftHours);
         
        long endTime = determineEndTime(origTimeSeries.getEndTime(),
                                          newIntervalInHours,
                                          shiftHours);
        
     //   System.out.println(header + 
     //                       "startTime = " + getTimeString(startTime) +
     //                       " endTime = " + getTimeString(endTime));
        
        MeasuringUnit unit = origTimeSeries.getMeasuringUnit();
        
        RegularTimeSeries newTs = new RegularTimeSeries(startTime,
                                                        endTime,
                                                        newIntervalInHours,
                                                        unit );
        newTs.shiftStartEndTimeHours(shiftHours);
         
        //start at end of period that we want to calculate
        long scanStartTime = newTs.getStartTime();
         
        long lookBackTimeInMillis = intervalInMillis - MILLIS_PER_HOUR;
        
        //calculate the totals
        for (long newTime = scanStartTime; 
                  newTime <= endTime; 
                  newTime += intervalInMillis)
        {
            double totalValue = 0.0;
            
   //         System.out.println("-begin -----newTime = " + getTimeString(newTime));
            
            //for each original interval
            for (long origTime = newTime - lookBackTimeInMillis;
                 origTime <= newTime;
                 origTime+= MILLIS_PER_HOUR)
            {
    //            System.out.println("origTime = " + getTimeString(origTime));
                Measurement m = origTimeSeries.getMeasurementByTime(origTime);
                if (m != null)
                {
                    totalValue += m.getValue();
                }
            }
   //         System.out.println("-end -----newTime = " + getTimeString(newTime) + 
   //                     " totalValue = " + totalValue);
            
            Measurement m = new Measurement(totalValue, unit);
            
            newTs.setMeasurementByTime(m, newTime);
            
        }
        
        
        return newTs;
    }
    
    // --------------------------------------------------------------------------------
    private static String getTimeString(long time)
    {
            return DbTimeHelper.getDateTimeStringFromLongTime(time);
    }
    
    // --------------------------------------------------------------------------------
    
    public static long determineStartTime(long origHourlyStartTime, int intervalInHours, int shiftHours)
    {
         
        long newStartTime = TimeHelper.truncateTimeInMillisToNearestHour(
                                    origHourlyStartTime, intervalInHours);
        
        newStartTime += intervalInHours * MILLIS_PER_HOUR;
        
       // newStartTime += shiftHours;
        
        return newStartTime;
    }
    // --------------------------------------------------------------------------------
    public static long determineEndTime(long origHourlyEndTime, int intervalInHours, int shiftHours)
    {
         
        long endStartTime = TimeHelper.truncateTimeInMillisToNearestHour(
                                        origHourlyEndTime, intervalInHours);
        
        endStartTime += intervalInHours * MILLIS_PER_HOUR;
        
       // endStartTime += shiftHours;
        
        return endStartTime;
    }
    // --------------------------------------------------------------------------------
    
    public static RegularTimeSeries 
                  getRegularTimeSeries(int intervalInHours,
                                       RegularTimeSeries origTimeSeries) 
    {
        
        return getRegularTimeSeries(intervalInHours, 0, origTimeSeries );
    }
    
    // --------------------------------------------------------------------------------
    
    public static void main(String[] stringArgs)
    {
        long startTime = TimeHelper.truncateTimeInMillisToNearestHour(System.currentTimeMillis(), 24);
        long endTime = startTime +  (3 * 24*MILLIS_PER_HOUR);
        MeasuringUnit unit = MeasuringUnit.inches;
        
        RegularTimeSeries origTimeSeries = new RegularTimeSeries(startTime,
                                                                 endTime,
                                                                 1,
                                                                 unit);
        int intervalInHours = 6;
        int shiftInHours = 0;
        RegularTimeSeries totalledTimeSeries = null;
        
        //initialize test values
        double value = 0.0;
        for (long t = startTime; t <= endTime; t+= MILLIS_PER_HOUR)
        {  
            Measurement m = new Measurement(value, unit);
            value += 1.0;
            origTimeSeries.setMeasurementByTime(m, t);
        }
        
        //calculate 6 hour values
        
        intervalInHours = 3;
        shiftInHours = 2;
        totalledTimeSeries = 
                TimeSeriesTotaller.getRegularTimeSeries(
                                          intervalInHours,
                                          shiftInHours,
                                          origTimeSeries);
        
        
        
        System.out.println("original time series = \n" + origTimeSeries);

        System.out.println("for every " + intervalInHours +
                            " hours : \n "  + totalledTimeSeries);
         
        return;
    }
    
    // --------------------------------------------------------------------------------

}
