package ohd.hseb.sshp.var;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.measurement.IrregularTimeSeries;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.measurement.AbsTimeMeasurement;

import static ohd.hseb.util.TimeHelper.*;

/************************************************************************************************************************************
 * A helper class for holding some commonly used constants and for providing some commonly used methods as static methods.
 * @author lix
 *
 */
public class VarHelper
{
 
    /********************************************************************************************************************************
     * Convert any IrregularTimeSeries to RegularTimeSeries with interpolation method.
     * @param irts
     * @return
     */
    public static RegularTimeSeries getRegularTimeSeriesFromIrregularTimeSeries(IrregularTimeSeries irts)
    {
        String header = "VarHelper.getRegularTimeSeriesFromIrregularTimeSeries(): ";
        long startTimeHour = TimeHelper.roundTimeInMillisToNearestHour(irts.getStartTime());  //rounding to nearest hour
        long endTime   = irts.getEndTime(); //no need for rounding

        RegularTimeSeries rts = new RegularTimeSeries(startTimeHour, endTime, 1, irts.getMeasuringUnit()); //interval = 1 hour

        for (long t = startTimeHour; t <= endTime; t += MILLIS_PER_HOUR)
        {
            AbsTimeMeasurement m = irts.getAbsTimeMeasurementByTime(t, true, false);  //interpolation to get missed data points
            
            if (m != null)
            {
                rts.setMeasurementByTime(m, m.getTime());
            }
            else //m == null
            {
                System.out.println(header + "measurement not available for time = " + 
                                   DbTimeHelper.getDateTimeStringFromLongTime(t));
                System.out.println(header + "rts.startTime = " + DbTimeHelper.getDateTimeStringFromLongTime(startTimeHour) +
                                    " rts.endTime = " + DbTimeHelper.getDateTimeStringFromLongTime(endTime));
                System.out.println(header + "irts.startTime = " +
                                    DbTimeHelper.getDateTimeStringFromLongTime( irts.getStartTime()));      
            }
                
        }

        return rts;
    }

//  ----------------------------------------------------------------------------------------------------------------------------------
    
    /*********************************************************************************************************************
     * Take the argument of long and computes the year, month, day and hour. Due to the postgres
     * database difference with Fortran (in Postgres database, hours are count from 0 to 23, with 0 as the first hour; in Fortran, from 1 to 24), so if the 
     * hour is 0, it needs to be changed to 24 and the day needs to be backward by 1.
     * Returns an array of String with 4 elements: year, month, day, hour
     * @param long
     */
    public static String[] getArrayYearMonthDayHour24HConventionFromLongTime(long longTime)
    {
        // Map map = new HashMap(tring.class, Integer.class);
        String strHour = null;
        if (VarHelper.getStringFromLongTime(longTime, "HH").equals("00"))
        {
            longTime -= MILLIS_PER_DAY;  //roll back one day: 0AM today(Java) == 24PM yesterday(Fortran)    
            strHour = "24";   //VarHelper.getStringFromLongTime(longTime, "k") is buggy, don't trust it. So use hard-coded "24"
        }
        else  //is not 0AM o'clock case
        {
            strHour = VarHelper.getStringFromLongTime(longTime, "H");   //no 0 padding
        }

        String[] strYearMonthDayHour = {VarHelper.getStringFromLongTime(longTime, "yyyy"), 
                VarHelper.getStringFromLongTime(longTime, "M"),
                VarHelper.getStringFromLongTime(longTime, "d"),
                strHour};  //VarHelper.getStringFromLongTime(longTime, "k") k: 1-24, H: 0-23
        return strYearMonthDayHour;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    /*************************************************************************************************************
     * Get a String based on the passed in parameter dateFormat.
     * @param time
     * @param dateFormat
     * @return
     */
    public static String getStringFromLongTime(long time, String dateFormat)
    {
        String timeString  = DbTimeHelper.getStringFromLongTime(time, dateFormat);
        return timeString;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
    /****************************************************************************************
     * Return a long based on the passed in parameter format and the timeString.
     * @param timeString
     * @param format
     * @return
     */    
    public static long getLongTimeFromDateTimeString(String timeString, String format)
    {
        long time = 0;
        if (timeString != null)
        {
            Date date = VarHelper.getDateTimeFromString(timeString, format);

            time = date.getTime();
        }   
        return time;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------

    /****************************************************************************************
     * Return a Date object based on the passed in timeString and the format.
     * @param timeString
     * @param format
     * @return
     */
    public static Date getDateTimeFromString(String timeString, String format)
    {
        Date shiftedDate = null;

        if (timeString != null)
        {
            SimpleDateFormat utcSdf2 = new SimpleDateFormat(format);
            utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));

            try
            {
                shiftedDate = utcSdf2.parse(timeString);
            }
            catch(ParseException e)
            {
                e.printStackTrace();    
            }
        }

        return shiftedDate;
    }
//  ----------------------------------------------------------------------------------------------------------------------------------
} //close class
