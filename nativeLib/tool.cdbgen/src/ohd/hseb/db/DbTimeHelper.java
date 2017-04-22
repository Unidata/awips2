/*
 * Created on Sep 16, 2003
 *
 * 
 */
package ohd.hseb.db;

import java.util.*;
import java.util.Date;
import java.text.*;
import java.sql.*;

/**
 * @author GobsC
 *
 * 
 */
public class DbTimeHelper
{
//	---------------------------------------------------------	

	public static String getStringFromDate(java.sql.Date date)
	{
		String dateString  = null;
		
		if (date != null)
		{
			//System.out.println("timeString = !" + timeString + "!");
			SimpleDateFormat utcSdf2 = new SimpleDateFormat("yyyy-MM-dd");
			utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
	
			dateString = utcSdf2.format(date);
		}
		return dateString;
	}	
//	---------------------------------------------------------	

	// *****************************************************************************
	//	getTimeStampFromString() is used to convert DateTime year to sec variables
	//	It is typically called from a concrete generated class to 
	//  get the time in the database in UTC - which is to say, the time the database
	//  already stores it as.
	// *****************************************************************************
	public static Date getDateTimeFromString(String timeString)
	{
		Date shiftedDate = null;
		
		if (timeString != null)
		{

		    //System.out.println("timeString = !" + timeString + "!");
		    SimpleDateFormat utcSdf2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
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
    
    // *****************************************************************************
    //  getTimeFromString() is used to convert DateTime year to sec variables
    //  It is typically called from a concrete generated class to 
    //  get the time in the database in UTC - which is to say, the time the database
    //  already stores it as.
    // *****************************************************************************
    public static Date getTimeToSecondsFromString(String timeString)
    {
        Date shiftedTime = null;
        
        if (timeString != null)
        {

            //System.out.println("timeString = !" + timeString + "!");
            SimpleDateFormat utcSdf2 = new SimpleDateFormat("HH:mm:ss");
            utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
            try
            {
                shiftedTime = utcSdf2.parse(timeString);
            }
            catch(ParseException e)
            {
                e.printStackTrace();    
            }
        
        }

        return shiftedTime;
    }
    
	// *****************************************************************************
    
    // *****************************************************************************
    //  getTimeStampFromString() is used to convert DateTime year to sec variables
    //  It is typically called from a concrete generated class to 
    //  get the time in the database in UTC - which is to say, the time the database
    //  already stores it as.
    // *****************************************************************************
    public static Date getDateTimeToMinutesFromString(String timeString)
    {
        Date shiftedDate = null;
        
        if (timeString != null)
        {

            //System.out.println("timeString = !" + timeString + "!");
            SimpleDateFormat utcSdf2 = new SimpleDateFormat("yyyy-MM-dd HH:mm");
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
    
	//	getDateFromString() is used to convert Date year to day variables
	//	It is typically called from a concrete generated class to 
	//  get the time in the database in UTC - which is to say, the time the database
	//  already stores it as.
	// *****************************************************************************
	public static Date getDateFromString(String timeString)
	{
		Date shiftedDate = null;

		if (timeString == null)
			return shiftedDate;
	
		//System.out.println("timeString = !" + timeString + "!");
		SimpleDateFormat utcSdf2 = new SimpleDateFormat("yyyy-MM-dd");
		utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
		try
		{
			shiftedDate = new Date(utcSdf2.parse(timeString).getTime());
		}
		catch(ParseException e)
		{
			e.printStackTrace();	
		}

		return shiftedDate;
	}
//	-------------------------------------------------------
	public static long getLongTimeFromDateTimeString(String timeString)
	{
		long time = 0;
		if (timeString != null)
		{
			Date date = getDateTimeFromString(timeString);
		
			time = date.getTime();
		}	
		return time;
	}
//	-------------------------------------------------------
    public static long getLongTimeFromDateTimeToMinutesString(String timeString)
    {
        long time = 0;
        if (timeString != null)
        {
            Date date = getDateTimeToMinutesFromString(timeString);
        
            time = date.getTime();
        }   
        return time;
    }
    
//  -------------------------------------------------------
    
	public static long getLongTimeFromDateString(String timeString)
	{
		long time = 0;
		if (timeString != null)
		{
			Date date = getDateFromString(timeString);
		
			time = date.getTime();
		}	
		return time;
	}
    
//  -------------------------------------------------------
 
    public static long getLongTimeFromTimeToSecondsString(String timeString)
    {
        long time = 0;
        if (timeString != null)
        {
            Date date = getTimeToSecondsFromString(timeString);
        
            time = date.getTime();
        }   
        return time;
    }
//      -------------------------------------------------------
    
//		-------------------------------------------------------
	public static String getStringFromTimestamp(Timestamp timestamp)
	{
		String timeString  = null;
	
		if (timestamp != null)
		{ 
			SimpleDateFormat utcSdf2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
			timeString = utcSdf2.format(timestamp);
		}
	
		//System.out.println("timeString = !" + timeString + "!");
	
		return timeString;
	}
// 	-------------------------------------------------------
    public static String getTimeToSecondsStringFromLongTime(long time)
    {
        String timeString  = getStringFromLongTime(time, "HH:mm:ss");
    
        return timeString;
    }
	//-------------------------------------------------------

	public static String getDateTimeStringFromLongTime(long time)
	{
		String timeString  = getStringFromLongTime(time, "yyyy-MM-dd HH:mm:ss");
	
		return timeString;
	}

   //-------------------------------------------------------

    public static String getDateTimeToMinutesStringFromLongTime(long time)
    {
        String timeString  = getStringFromLongTime(time, "yyyy-MM-dd HH:mm");
    
        return timeString;
    }

    //  -------------------------------------------------------

    
    
	public static String getDateStringFromLongTime(long time)
	{
		String timeString  = getStringFromLongTime(time, "yyyy-MM-dd");
	
		return timeString;
	}
	
	//-------------------------------------------------------
	
	public static String getStringFromLongTime(long time, String dateFormat)
	{
	    String timeString  = null;
	
		//System.out.println("timeString = !" + timeString + "!");
		SimpleDateFormat utcSdf2 = new SimpleDateFormat(dateFormat);
		utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
		timeString = utcSdf2.format(new java.util.Date(time));
	
		return timeString;
	}




//	-------------------------------------------------------


}
