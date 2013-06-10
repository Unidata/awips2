/*
 * Created on Oct 8, 2003
 *
 *
 */
package ohd.hseb.util;

/**
 * @author GobsC
 *
 */

public class TimeHelper
{

    public static final long MILLIS_PER_MINUTE = 60 * 1000;
    public static final long MILLIS_PER_HALF_HOUR = 30 * MILLIS_PER_MINUTE; 
    public static final long MILLIS_PER_HOUR = 60 * MILLIS_PER_MINUTE; 
    public static final long MILLIS_PER_12_HOURS = MILLIS_PER_HOUR * 12;
    public static final long MILLIS_PER_DAY = MILLIS_PER_HOUR * 24;

//  --------------------------------------------------------------------------

	public static long truncateTimeInMillisToNearestHour(long timeInMillis,
													     int hoursToTruncate)
	{
			long roundingAmount = hoursToTruncate * MILLIS_PER_HOUR;
			long roundedTime = timeInMillis;
			roundedTime /= roundingAmount;
			roundedTime *= roundingAmount;
        
			return roundedTime;
	}
//		 --------------------------------------------------------------------------
    public static long roundTimeInMillisToNearestHour(long timeInMillis)                                                  
    {     
            long roundingAmount = MILLIS_PER_HOUR;
            long halfHourInMillis = MILLIS_PER_HOUR/2;
            long roundedTime = truncateTimeInMillisToNearestHour(timeInMillis + halfHourInMillis, 1);
     
            return roundedTime;
    }
//  --------------------------------------------------------------------------

}
