package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

/**
 * Class to handle formatting and conversion of various date/time formats for DAA, DSA, and DPR products for 
 * MPE and HPE/HPN. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * July 2013 DCS 167    C. Gobs   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Chip Gobs
 */

public class HydroTimeUtility 
{
	
	private static final int MILLIS_PER_DAY = 24 * 60 * 60 * 1000;

	private static ThreadLocal<SimpleDateFormat> sqlSdf = new ThreadLocal<SimpleDateFormat>() {

		@Override
		protected SimpleDateFormat initialValue() {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
			return sdf;
		}

	};
	
	private static ThreadLocal<SimpleDateFormat> sqlDateOnlySdf = new ThreadLocal<SimpleDateFormat>() {

		@Override
		protected SimpleDateFormat initialValue() {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
			sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
			return sdf;
		}

	};
	
	private static ThreadLocal<SimpleDateFormat> monthDayYearHourMinuteSdf = new ThreadLocal<SimpleDateFormat>() {

		@Override
		protected SimpleDateFormat initialValue() {
			SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy HH:mm");
			sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
			return sdf;
		}

	};
	
	
	public static String changeDate(String originalDateString, int days)
	{
	
		Date originalDate = getDateFromSQLDateOnlyString(originalDateString);
		long millis = originalDate.getTime();
		
		//change the time by milliseconds
		millis += days * MILLIS_PER_DAY;
		
		//create new date and get the string representation
		Date newDate = new Date(millis);
		String newDateString = getSQLDateOnlyStringFromDate(newDate);

		return newDateString;
	}
	
	
	public static String getSQLDateOnlyStringFromDate(Date date)
	{
		String dateString = null;
		try 
		{
			dateString = sqlDateOnlySdf.get().format(date);
		} 	
		catch (Exception e1)
		{
			e1.printStackTrace();
		}
		
		return dateString;
	}
	
	public static Date getDateFromSQLDateOnlyString(String sqlDateString)
	{
		Date date = null;
		try 
		{
			date = sqlDateOnlySdf.get().parse(sqlDateString);	
		} 	
		catch (ParseException e1)
		{
			e1.printStackTrace();
		}
		
		return date;
	}
	
	
	public static String getSQLStringFromDate(Date date)
	{
		String dateString = null;
		try 
		{
			dateString = sqlSdf.get().format(date);
		} 	
		catch (Exception e1)
		{
			e1.printStackTrace();
		}
		
		return dateString;
	}
	
	public static Date getDateFromSQLString(String sqlDateString)
	{
		Date date = null;
		try 
		{
			date = sqlSdf.get().parse(sqlDateString);	
		} 	
		catch (ParseException e1)
		{
			e1.printStackTrace();
		}
		
		return date;
	}
	

	public static long getMillisFromSQLString(String sqlDateString)
	{
		Date date = null;
		try 
		{
			date = sqlSdf.get().parse(sqlDateString);	
		} 	
		catch (ParseException e1)
		{
			e1.printStackTrace();
		}
		
		return date.getTime();
	}
	
	public static long getMillisFromAmericanDateTimeString(String sqlDateString)
	{
		Date date = null;
		try 
		{
			date = monthDayYearHourMinuteSdf.get().parse(sqlDateString);	
		} 	
		catch (ParseException e1)
		{
			e1.printStackTrace();
		}
		
		return date.getTime();
	}
	
	
	
	public static String JulianDateConvertToYMD(int jdate)
    {
		return JulianDateConvertWithFormat(jdate, "ymd");
    }
	
	public static String JulianDateConvertToMDY(int jdate)
    {
		return JulianDateConvertWithFormat(jdate, "mdy");
    }
	
	
	public static String JulianDateConvertWithFormat(int jdate, String ymdFormat)
    {
	    // Convert from Julian day number to month, day, year
		// Based on Jan 1, 1970

		int days[]={31,28,31,30,31,30,31,31,30,31,30,31} ;
		int year = 0, month = 0,ndays,date =
			0,total,leap_year;
		int TRUE =  1;
		int FALSE = 0;
		String cdate = null;

		total=0;
		for (year=1970;year<2080;year++)
		{
			leap_year=FALSE;
			
			if ((((year%4) == 0) && ((year%100) != 0)) 	|| (year%400==0))
			{
				leap_year=TRUE;
			}
			
			for (month=0;month<12;month++)
			{
				total = total+days[month];
				if (month==1 && leap_year==TRUE)
				{
					total++;
				}
				
				if (total >= jdate)
				{
					ndays=days[month];
					if (month==1 && leap_year==TRUE)
					{
						ndays++;
					}
					
					date=ndays - (total-jdate);
					int newMonth = month+1;
					
					if (ymdFormat.equals("ymd"))
					{
						cdate = String.format("%04d-%02d-%02d",year,newMonth,date);
					}
					else
					{
						cdate = String.format("%02d%02d%04d",newMonth,date,year);
					}
					return cdate;

				}
			}
		}
		return cdate;
    }
	
	
	public static String JulianDateConvertToYMD2(int jdate)
    {
		return JulianDateConvertWithFormat2(jdate, "ymd");
    }
	
	public static String JulianDateConvertWithFormat2(int jdate, String ymdFormat)
	{

		String cdate = null;
		int days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31} ;
		int year, month, ndays, date, total;

		boolean leap_year = false;

		total = 0;
		for (year = 1970; year < 2030; year++)
		{
			leap_year = false;
			if (year == 1972 || year == 1976 || year == 1980 ||
					year == 1984 || year == 1988 || year == 1992 || year == 1996 ||
					year == 2000 || year == 2004 || year == 2008 || year == 2012 ||
					year == 2016 || year == 2020 || year == 2024 || year == 2028)
			{
				leap_year = true;
			}

			for (month = 0; month < 12; month++)
			{
				total = total + days[month];
				if (month == 1 && leap_year)
				{
					total ++;
		            }

				if (total >= jdate)
				{
					ndays = days[month];
					if (month == 1 && leap_year)
					{
						ndays++;
					}
					date = ndays - (total-jdate);
					month++;

					if (ymdFormat.equals("ymd"))
					{
						cdate = String.format("%04d-%02d-%02d",year, month, date);
					}
					else
					{
						cdate = String.format("%02d%02d%04d",month, date, year);
					}

					return cdate;
				}
			}
		}

		return cdate;
	}

	
	
	public static void testJulian(int julianDate)
	{
		String yearMonthDayString = HydroTimeUtility.JulianDateConvertToYMD(julianDate);
		System.out.printf("julianDate of %d = %s\n", julianDate, yearMonthDayString);
	}
	
	public static void testJulian2(int julianDate)
	{
		String yearMonthDayString = HydroTimeUtility.JulianDateConvertToYMD2(julianDate);
		System.out.printf("julianDate of %d = %s\n", julianDate, yearMonthDayString);
	}
	
	
	public static void main(String[] argArray)
	{
		//closer to A1 DAA code
		HydroTimeUtility.testJulian(0);
		
		HydroTimeUtility.testJulian(1);
		
		
		//closer to A1 DPR code
		
		HydroTimeUtility.testJulian2(0);
		
		HydroTimeUtility.testJulian2(1);
		
	}
}
