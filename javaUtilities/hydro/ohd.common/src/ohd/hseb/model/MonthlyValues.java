/*
 * Created on Jan 20, 2004
 *
 * 
 */
package ohd.hseb.model;

import java.util.*;
import ohd.hseb.db.*;

/**
 * @author GobsC
 *
 * This class encapsulates the double precision floating point values
 * that are stored for each month.
 */
public class MonthlyValues
{
	private String _basinId = null;
	private String _pe = null;
	private short _dur = 0;
	private String _ts = null;
	private String _extremum = null;
	private boolean _adjustment = false;
	private long _postingTime = 0; 
    private double[] _valueArray = new double[12];
    private int _monthsInYear = 12;
    

    // ------------------------------------------------------------
    
    public MonthlyValues()
    {
    } //MonthlyValues()
    
    // ------------------------------------------------------------
    public MonthlyValues(double[] valueArray)
    {
        setValues(valueArray);
    } //MonthlyValues()
    
    // ------------------------------------------------------------
    public void setValues(double[] valueArray)
    {
        for (int i = 0 ; i < valueArray.length && 
                                i < _monthsInYear ; i++)
        {
            _valueArray[i] = valueArray[i];       
        }  
        
    } //setValues()
    // ------------------------------------------------------------
 
    // ------------------------------------------------------------
    
    public double getValue(long time)
    {
        double value = 0.0;
        //this method returns an interpolated daily value 
        
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(new Date(time));
       
       
        // extract needed info based on the current time
        int monthIndex = cal.get(Calendar.MONTH);
        int dayOfMonth = cal.get(Calendar.DAY_OF_MONTH);
        int year = cal.get(Calendar.YEAR);
        
        int year1 = year;
        int year2 = year;
        
        
        int currentMonth = monthIndex;
         
        double month1Value = 0;
        double month2Value = 0;
        
        double  weight1 = 0;
        double  weight2 = 0;

        double monthToMonthDayDistance = 0.0;
        double month1ToDayDistance = 0.0;
        double month2ToDayDistance = 0.0;

        
        //determine which months to interpolate against
        if (dayOfMonth == 16)
        {
            value = _valueArray[currentMonth]; 
            
            month1Value = _valueArray[currentMonth];
            month2Value = 0;
            
            monthToMonthDayDistance = 1;
            
            month1ToDayDistance = 0;
            month2ToDayDistance = 1;   
        }
        else if (dayOfMonth < 16)
        {
            int previousMonth = monthIndex-1;
            if (previousMonth < Calendar.JANUARY)
            {
               previousMonth = Calendar.DECEMBER;
               year1 = year - 1;
            }
       
            month1Value = _valueArray[previousMonth];
            month2Value = _valueArray[currentMonth];

            monthToMonthDayDistance = getDayDistance(year1, previousMonth, 16, year2, currentMonth, 16);
            
            month1ToDayDistance = getDayDistance(year1, previousMonth, 16, year2, currentMonth, dayOfMonth);
            month2ToDayDistance = getDayDistance(year2, currentMonth, dayOfMonth, year2, currentMonth, 16);

        }
        
        else //dayOfMonth > 16
        {
            
            int nextMonth = currentMonth + 1;
            if (nextMonth > Calendar.DECEMBER)
            {
                nextMonth = Calendar.JANUARY;
                year2 = year + 1;
            }
        
            month1Value = _valueArray[currentMonth];
            month2Value = _valueArray[nextMonth]; 
            
            monthToMonthDayDistance = getDayDistance(year1, currentMonth, 16, year2, nextMonth, 16);
            
            month1ToDayDistance = getDayDistance(year1, currentMonth, 16, year1, currentMonth, dayOfMonth);
            month2ToDayDistance = getDayDistance(year1, currentMonth, dayOfMonth, year2, nextMonth, 16);  

        } 
        

        weight1 = (1.0 - (month1ToDayDistance/monthToMonthDayDistance));
        weight2 = (1.0 - (month2ToDayDistance/monthToMonthDayDistance));


        value = (weight1 * month1Value) + (weight2 * month2Value);
         
        return value;
    }
    
    // ------------------------------------------------------------

    private double getDayDistance(int year1, int month1, int day1, 
                               int year2, int month2, int day2)
    {     
        int daysInYear1 = 365;
        
        if (isLeapYear(year1))
        {
            daysInYear1 = 366;
        }
        
        GregorianCalendar cal1 = new GregorianCalendar(year1, month1, day1);
        int dayOfYear1 = cal1.get(Calendar.DAY_OF_YEAR);
            
        GregorianCalendar cal2 = new GregorianCalendar(year2, month2, day2);
        int dayOfYear2 = cal2.get(Calendar.DAY_OF_YEAR);
          
        //if there is a year change, make the first day of the second year
        // start at 366 or 367

        if (year2 != year1) 
        {       
            dayOfYear2 += daysInYear1;
        }  

        int dayDistance = dayOfYear2 - dayOfYear1;
          
                
        return dayDistance; 
   
    } //end getDayDistance()
    
    // ------------------------------------------------------------
    
    private boolean isLeapYear(int year)
    {
        boolean isLeap = false;
        
        if ((year % 4) == 0)
        {
           if ((year % 100) == 0)
           {
               if ((year % 400) == 0 )    //2000 is a leap year, 1900 and 2100 are not
               {
                  isLeap = true;
               }
               else
               {
                  isLeap = false;    
               }
           }
           else
           {
              isLeap = true;    
           }    
        }
        else
        {
           isLeap = false;    
        }
    
        return isLeap;
    } //end isLeapYear()
    
    // ------------------------------------------------------------
    public static void main (String[] args)
    {
      
        double[] valuesArray = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0,
            
                                7.0, 8.0, 9.0, 10.0, 11.0, 12.0  };
      
        MonthlyValues values = new MonthlyValues(valuesArray);  
           
        final long millisPerDay = 1000 * 60 * 60 * 24;   
        long currentTime = System.currentTimeMillis(); 
        long todayTime = (currentTime / millisPerDay) * millisPerDay;  
        
        todayTime += 1000 * 3600 *12; //add 12  hours
        
        long loopTime = todayTime;
        int daysInYear = 365;
           
        for (int i = 0; i < daysInYear; i++)
        {
           String dateString = DbTimeHelper.getDateStringFromLongTime(loopTime);
           
           String dateTimeString = DbTimeHelper.getDateTimeStringFromLongTime(loopTime);
           
           double value = values.getValue(loopTime);
           System.out.println("On " + dateString + " at " + dateTimeString + ", the value is " + value);
           
             
           loopTime += millisPerDay;
        }    
        
        
    }
    
    public String getBasinId()
    {
    	return _basinId;
    }
    
    public long getPostingTime()
    {
    	return _postingTime;
    }
    
    public void setBasinId( String basinId)
    {
    	_basinId = basinId;
    }
    
    public void setPostingTime( long postingTime )
    {
    	_postingTime = postingTime;
    }
    
    public double[] getValueArray()
    {
    	double[] valueArray = new double[ _valueArray.length ];
    	
    	for ( int i = 0; i < _valueArray.length; i++ )
    	{
    		valueArray[ i ] = _valueArray[ i ];
    	}
    	return valueArray;
    }

	public void setPe( String pe )
	{
		_pe = pe;
	}

	public String getPe()
	{
		return _pe;
	}

	public void setDur( short dur )
	{
		_dur = dur;
	}

	public short getDur()
	{
		return _dur;
	}

	public void setTs( String ts )
	{
		_ts = ts;
	}

	public String getTs()
	{
		return _ts;
	}

	public void setExtremum( String extremum )
	{
		_extremum = extremum;
	}

	public String getExtremum()
	{
		return _extremum;
	}

	public void setAdjustment( boolean adjustment )
	{
		_adjustment = adjustment;
	}

	public boolean isAdjustment()
	{
		return _adjustment;
	}

    // ------------------------------------------------------------

}
