/*
 * Created on Aug 6, 2003
 *
 * 
 */
package ohd.hseb.db;

import java.sql.*;

/**
 * @author Chip Gobs
 *
 * This class is the abstract superclass of all of the xxxRecord classes.
 * xxx represents the name of the table or view.  This class
 * provides some utility methods and acts as a nice way of unifying all of the
 * subclasses under one umbrella. 
 */
public abstract class DbRecord
{
	public abstract String toString();
	
	//-------------------------------------------------------

	public Timestamp copyTimestamp(Timestamp origTimestamp)
	{
	   	Timestamp newTimestamp = new Timestamp(origTimestamp.getTime());
		return newTimestamp;
	}
	
	//	-------------------------------------------------------

	public java.sql.Date copyDate(java.sql.Date origDate)
	{
	    java.sql.Date newDate = new java.sql.Date(origDate.getTime());
		return newDate;
	}
	
    //	-------------------------------------------------------
	
	public String getStringFromTimestamp(Timestamp timestamp)
	{	
		return DbTimeHelper.getStringFromTimestamp(timestamp);
	}
//	-------------------------------------------------------
	
	public String getStringFromDate(java.sql.Date date)
	{

        return DbTimeHelper.getStringFromDate(date);
    }
    //---------------------------------------------------------	
	public long getLongTimeFromDateTimeString(String timeString)
	{
	    return DbTimeHelper.getLongTimeFromDateTimeString(timeString);
	}
	//-------------------------------------------------------
	
	public String getDateTimeStringFromLongTime(long time)
	{
	    return DbTimeHelper.getDateTimeStringFromLongTime(time);
	}
//-------------------------------------------------------
	
	public String getDateStringFromLongTime(long time)
	{
	    return DbTimeHelper.getDateStringFromLongTime(time);

	}
	//-------------------------------------------------------

}
