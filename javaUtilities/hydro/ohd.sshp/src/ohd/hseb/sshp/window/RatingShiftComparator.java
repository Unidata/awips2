/*
 * Created on Jun 14, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 14, 2004
 *  
 */
package ohd.hseb.sshp.window;

import ohd.hseb.model.RatingShift;
import ohd.hseb.util.gui.TableColumnComparator;

/**
 * @author SoodG
*/
public class RatingShiftComparator extends TableColumnComparator
{
	public RatingShiftComparator()
	{
	}

	public int compareShiftDate( RatingShift entry1, RatingShift entry2 )
	{
		return( numberComparison( entry1.getDate(), entry2.getDate() ) );
	}
	
	public int compareValue( RatingShift entry1, RatingShift entry2 )
	{
		return( numberComparison( entry1.getShiftAmount(), entry2.getShiftAmount() ) );
	}
	
	public int compareActive( RatingShift entry1, RatingShift entry2 )
	{
		String entry1Active = Boolean.toString( entry1.isActive() );
		String entry2Active = Boolean.toString(entry2.isActive() );
		
		int result = entry1Active.compareTo( entry2Active );
		
		return result;
	}
	
	public int numberComparison( double value1, double value2 )
	{
		int returnValue = 0;
		
		if ( value1 < value2 )
		{
			returnValue = -1;
		}
		else if ( value1 == value2 )
		{
			returnValue = 0;
		}
		else // value1 > value2 
		{
			returnValue = 1;
		}
		
		return returnValue;
	}
	
	
	public int compare( Object object1, Object object2 )
	{
		RatingShift entry1 = (RatingShift) object1;
		RatingShift entry2 = (RatingShift) object2;
		
		int returnValue = 0;
		
		if( _columnName.equalsIgnoreCase( RatingCurveEditor.SHIFT_DATE ) )
		{
			returnValue = compareShiftDate( entry1, entry2 );
		}
		else if ( _columnName.equalsIgnoreCase( RatingCurveEditor.VALUE ) )
		{
			returnValue = compareValue( entry1, entry2 );
		}
		else if ( _columnName.equalsIgnoreCase( RatingCurveEditor.ACTIVE ) )
		{
			returnValue = compareActive( entry1, entry2 );
		}
		
		if ( ! _ascOrder )
		{
			returnValue = -1 * returnValue;
		}
		
		return returnValue;
	}
}
