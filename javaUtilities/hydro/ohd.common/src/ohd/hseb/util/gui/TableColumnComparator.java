/*
 * Created on Jun 24, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 24, 2004
 *  
 */

/**
 * @author SoodG
*/
package ohd.hseb.util.gui;

import java.util.Comparator;

public abstract class TableColumnComparator implements Comparator
{
	protected String _columnName = null;
	protected boolean _ascOrder = true;
//	-------------------------------------------------------------------------------	
	public abstract int compare(Object arg0, Object arg1);
//	-------------------------------------------------------------------------------
	public void initColumnInfo( String columnName, boolean ascOrder )
	{
		_columnName = columnName;
		_ascOrder = ascOrder;
	}
//	-------------------------------------------------------------------------------
	public int compareNumbers( double value1, double value2 )
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
//	-------------------------------------------------------------------------------
	public int compareStrings( String string1, String string2 )
	{
		int returnValue = 0;
		
		returnValue = string1.compareTo(string2);
				
		return returnValue;
	}
	
	//-------------------------------------------------------------------------------
}
