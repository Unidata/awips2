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

	public static final long MILLIS_PER_HOUR = 1000 * 60 * 60;

	public static long roundTimeInMillisToNearestHour(long timeInMillis,
															  int hoursToRound)
	{
			long roundingAmount = hoursToRound * MILLIS_PER_HOUR;
			long roundedTime = timeInMillis;
			roundedTime /= roundingAmount;
			roundedTime *= roundingAmount;
        
			return roundedTime;
	}
//		 --------------------------------------------------------------------------


}
