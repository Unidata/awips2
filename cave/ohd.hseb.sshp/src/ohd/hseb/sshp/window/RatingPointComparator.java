/*
 * Created on Jun 14, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 14, 2004
 *  
 */
package ohd.hseb.sshp.window;

import ohd.hseb.model.RatingPoint;
import ohd.hseb.util.gui.TableColumnComparator;

/**
 * @author SoodG
*/
public class RatingPointComparator extends TableColumnComparator
{
	public RatingPointComparator()
	{
	}

	public int compareUnshiftedStage( RatingPoint entry1, RatingPoint entry2 )
	{
		return( numberComparison( entry1.getUnshiftedStage(), entry2.getUnshiftedStage() ) );
	}
	
	public int compareShiftedStage( RatingPoint entry1, RatingPoint entry2 )
	{
		return( numberComparison( entry1.getUnshiftedStage(), entry2.getUnshiftedStage() ) );
	}
	
	public int compareDischarge( RatingPoint entry1, RatingPoint entry2 )
	{
		return( numberComparison( entry1.getDischarge(), entry2.getDischarge() ) );
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
		RatingPoint entry1 = (RatingPoint) object1;
		RatingPoint entry2 = (RatingPoint) object2;
		
		int returnValue = 0;
		
		if( _columnName.equalsIgnoreCase( RatingCurveEditor.STAGE ) )
		{
			returnValue = compareUnshiftedStage( entry1, entry2 );
		}
		else if ( _columnName.equalsIgnoreCase( RatingCurveEditor.STAGE_WITH_SHIFT) )
		{
			returnValue = compareShiftedStage( entry1, entry2 );
		}
		else if ( _columnName.equalsIgnoreCase( RatingCurveEditor.DISCHARGE ) )
		{
			returnValue = compareDischarge( entry1, entry2 );
		}
		
		if ( ! _ascOrder )
		{
			returnValue = -1 * returnValue;
		}
		
		return returnValue;
	}
}
