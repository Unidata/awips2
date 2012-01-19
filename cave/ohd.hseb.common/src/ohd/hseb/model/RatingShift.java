/*
 * Created on Jun 9, 2004
 *
 * Filename : RatingShiftEntry.java 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 9, 2004
 *  
 */
/**
 * @author Gautam Sood
*/

package ohd.hseb.model;

public class RatingShift
{
	private String _lid = null;
	
	private long _date = 0;
	
	private double _shiftAmount = 0.0;
	
	private boolean _active = false;
	
	public RatingShift()
	{
	}
	
	public RatingShift( RatingShift ratingShift )
	{
		setLid( ratingShift.getLid() );
		setDate( ratingShift.getDate() );
		setShiftAmount( ratingShift.getShiftAmount() );
		setActive( ratingShift.isActive() );
	}

	public void setLid(String lid)
	{
		_lid = lid;
	}

	public String getLid()
	{
		return _lid;
	}

	
	public boolean equals( RatingShift entry )
	{
		boolean isEqual = false;
		
		if ( ( _lid.equalsIgnoreCase( entry.getLid() ) ) &&
		   ( _date ==  entry.getDate() ) )
		{
			isEqual = true;
		}
		return isEqual;
	}

	public void setDate( long date )
	{
		_date = date;
	}

	public long getDate()
	{
		return _date;
	}

	public void setShiftAmount( double shiftAmount )
	{
		_shiftAmount = shiftAmount;
	}

	public double getShiftAmount()
	{
		return _shiftAmount;
	}

	public void setActive( boolean active )
	{
		_active = active;
	}

	public boolean isActive()
	{
		return _active;
	}
}
