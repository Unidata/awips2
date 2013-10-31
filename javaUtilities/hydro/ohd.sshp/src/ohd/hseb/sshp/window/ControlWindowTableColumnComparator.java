/*
 * Created on Aug 19, 2004
 *
 * 
 */
package ohd.hseb.sshp.window;


import ohd.hseb.model.LocationDescriptor;
import ohd.hseb.util.gui.TableColumnComparator;

/**
 * @author GobsC
 *
 */ 
public class ControlWindowTableColumnComparator extends TableColumnComparator
{
    public ControlWindowTableColumnComparator()
	{
	}
    
	public int compareLid( LocationDescriptor desc1, LocationDescriptor desc2 )
	{
		int result = compareStrings(desc1.getId(), desc2.getId());
		return result;
	}
		
	public int compareName( LocationDescriptor desc1, LocationDescriptor desc2 )
	{
		return( compareStrings( desc1.getLocationName(), desc2.getLocationName() ) );
	}
	
	public int compareStreamName( LocationDescriptor desc1, LocationDescriptor desc2 )
	{
		return( compareStrings( desc1.getStreamName(), desc2.getStreamName() ) );
	}
	
	public int compareBasinId( LocationDescriptor desc1, LocationDescriptor desc2 )
	{
		return( compareStrings( desc1.getBasinId(), desc2.getBasinId() ) );
	}
	
    public int compareHsa( LocationDescriptor desc1, LocationDescriptor desc2 )
    {
        return( compareStrings( desc1.getHsa(), desc2.getHsa() ) );
    }
    
    public int compareModelPreference( LocationDescriptor desc1, LocationDescriptor desc2 )
    {
        return( compareStrings( desc1.getModelPreference(), desc2.getModelPreference() ) );
    }

    public int compare(Object object1, Object object2)
	{
		LocationDescriptor desc1 = (LocationDescriptor) object1;
		LocationDescriptor desc2 = (LocationDescriptor) object2;
		
		int returnValue = 0;
	
		
		if( _columnName.equalsIgnoreCase( ControlWindow.LID ) )
		{
			returnValue = compareLid( desc1, desc2 );
		}
		else if  ( _columnName.equalsIgnoreCase( ControlWindow.NAME ) )
		{
			returnValue = compareName( desc1, desc2 );
		}
		else if ( _columnName.equalsIgnoreCase( ControlWindow.STREAM_NAME))
		{
			returnValue = compareStreamName( desc1, desc2 );
		}
		else if (_columnName.equalsIgnoreCase( ControlWindow.BASIN_ID) )
		{
			returnValue = compareBasinId( desc1, desc2 );
		}
        
        else if (_columnName.equalsIgnoreCase( ControlWindow.HSA) )
        {
            returnValue = compareHsa( desc1, desc2 );
        }
        
        else if (_columnName.equalsIgnoreCase( ControlWindow.MODEL_PREFERENCE) )
        {
            returnValue = compareModelPreference(desc1, desc2);
        }
		
		if ( ! _ascOrder )
		{
			returnValue = -1 * returnValue;
		}
		
		return returnValue;
	}
}
