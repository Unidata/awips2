/*
 * Created on Nov 6, 2003
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Nov 6, 2003
 *  
 */
package ohd.hseb.util.gui;

/**
 * @author SoodG
*/
public class DateTimeEvent
{
	private boolean _isGoodDate = false;
	private long _dateTimeLong = 0;
	
	public DateTimeEvent()
	{
	}

	public DateTimeEvent( long dateTimeLong, boolean isGoodDate )
	{
		_dateTimeLong = dateTimeLong;
		_isGoodDate = isGoodDate;
	}
	
	public void setGoodDate( boolean isGoodDate )
	{
		_isGoodDate = isGoodDate;
	}

	public boolean isGoodDate()
	{
		return _isGoodDate;
	}

	public void setDateTimeLong( long dateTimeLong )
	{
		_dateTimeLong = dateTimeLong;
	}

	public long getDateTimeLong()
	{
		return _dateTimeLong;
	}
}
