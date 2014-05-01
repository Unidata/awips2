/*
 * Created on Jun 14, 2004
 *
 * Filename : 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 14, 2004
 *  
 */
package ohd.hseb.sshp.window;

import ohd.hseb.model.UnitHydrographEntry;
import ohd.hseb.util.gui.TableColumnComparator;

/**
 * @author SoodG
*/
public class UnitHydrographEntryComparator extends TableColumnComparator
{
	public UnitHydrographEntryComparator()
	{
	}

	public int compareArea_Id( UnitHydrographEntry entry1, UnitHydrographEntry entry2 )
	{
		int result = entry1.getAreaId().compareToIgnoreCase( entry2.getAreaId() );
		return result;
	}
	
	public int compareModel( UnitHydrographEntry entry1, UnitHydrographEntry entry2 )
	{
		return( compareStrings( entry1.getModel(), entry2.getModel() ) );
	}
	
	public int compareDuration( UnitHydrographEntry entry1, UnitHydrographEntry entry2 )
	{
		return( compareNumbers( entry1.getDur(), entry2.getDur() ) );
	}
	
	public int compareOrdinal( UnitHydrographEntry entry1, UnitHydrographEntry entry2 )
	{
		return( compareNumbers( entry1.getOrdinal(), entry2.getOrdinal() ) );
	}
	
	public int compareDischarge( UnitHydrographEntry entry1, UnitHydrographEntry entry2 )
	{
		return( compareNumbers( entry1.getDischarge(), entry2.getDischarge() ) );
	}


	public int compare(Object object1, Object object2)
	{
		UnitHydrographEntry entry1 = (UnitHydrographEntry) object1;
		UnitHydrographEntry entry2 = (UnitHydrographEntry) object2;
		
		int returnValue = 0;
		
		if( _columnName.equalsIgnoreCase( UnitHydrographEditor.AREA_ID ) )
		{
			returnValue = compareArea_Id( entry1, entry2 );
		}
		else if  ( _columnName.equalsIgnoreCase( UnitHydrographEditor.MODEL ) )
		{
			returnValue = compareModel( entry1, entry2 );
		}
		else if ( _columnName.equalsIgnoreCase( UnitHydrographEditor.DURATION ) )
		{
			returnValue = compareDuration( entry1, entry2 );
		}
		else if ( _columnName.equalsIgnoreCase( UnitHydrographEditor.ORDINAL ) )
		{
			returnValue = compareOrdinal( entry1, entry2 );
		}
		else if ( _columnName.equalsIgnoreCase( UnitHydrographEditor.DISCHARGE ) )
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
