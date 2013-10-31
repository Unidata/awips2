/*
 * Created on Jun 9, 2004
 *
 * Filename : UnitHydrographEntry.java 
 * Author   : Gautam Sood
 * Last Revision Date : Jun 9, 2004
 *  
 */
/**
 * @author Gautam Sood
*/

package ohd.hseb.model;


public class UnitHydrographEntry
{
	private String _locationId = null;

	private String _areaId = null;
	
	private String _model = null;

	private int _dur = 0;
	
	private int _ordinal = 0;

	private double _discharge = 0.0;
//  ------------------------------------------------------------------------------  
	
	public UnitHydrographEntry()
	{
	}
//  ------------------------------------------------------------------------------  
	
	public UnitHydrographEntry( UnitHydrographEntry uhgEntry )
	{
		setLocationId( uhgEntry.getLocationId() );
		setAreaId( uhgEntry.getAreaId() );
		setDur( uhgEntry.getDur() );
		setOrdinal( uhgEntry.getOrdinal() );
		setDischarge( uhgEntry.getDischarge() );
	}
//  ------------------------------------------------------------------------------  

	public void setLocationId(String locationId)
	{
		_locationId = locationId;
	}
//  ------------------------------------------------------------------------------  

	public String getLocationId()
	{
		return _locationId;
	}
//  ------------------------------------------------------------------------------  

	public void setAreaId(String areaId)
	{
		_areaId = areaId;
	}
//  ------------------------------------------------------------------------------  

	public String getAreaId()
	{
		return _areaId;
	}
//  ------------------------------------------------------------------------------  

	public void setDur(int dur)
	{
		_dur = dur;
	}
//  ------------------------------------------------------------------------------  

	public int getDur()
	{
		return _dur;
	}
//  ------------------------------------------------------------------------------  

	public void setOrdinal(int ordinal)
	{
		_ordinal = ordinal;
	}
//  ------------------------------------------------------------------------------  

	public int getOrdinal()
	{
		return _ordinal;
	}
//  ------------------------------------------------------------------------------  

	public void setDischarge(double discharge)
	{
		_discharge = discharge;
	}
//  ------------------------------------------------------------------------------  

	public double getDischarge()
	{
		return _discharge;
	}
//  ------------------------------------------------------------------------------  
	
	public boolean hasEqualKey( UnitHydrographEntry entry )
	{
		boolean isEqual = false;
		
		if ( 
		        ( _locationId.equalsIgnoreCase( entry.getLocationId() ) ) &&
		        ( _areaId.equalsIgnoreCase( entry.getAreaId() ) ) &&
		        ( _model.equalsIgnoreCase( entry.getModel() ) ) &&			
		        ( _dur == entry.getDur() ) &&
		        ( _ordinal == entry.getOrdinal() ) 
		   )
		{
			isEqual = true;
		}
		return isEqual;
	}
//  ------------------------------------------------------------------------------  
	
	public boolean equals( Object object )
	{
	    UnitHydrographEntry entry = (UnitHydrographEntry) object;
		boolean isEqual = false;
		
		if (
		        ( _locationId.equalsIgnoreCase( entry.getLocationId() ) ) &&
		        ( _areaId.equalsIgnoreCase( entry.getAreaId() ) ) &&
		        ( _model.equalsIgnoreCase( entry.getModel() ) ) &&			
		        ( _dur == entry.getDur() ) &&
		        ( _ordinal == entry.getOrdinal() )  &&
		        ( _discharge == entry.getDischarge())
		   )
		{
			isEqual = true;
		}
		return isEqual;
	}
//  ------------------------------------------------------------------------------  
	
	public int hashCode()
    {
        String hashString = _locationId.toLowerCase() + "|" + _areaId.toLowerCase() +
        					"|" + _model.toLowerCase() + "|" + _dur + "|" + _ordinal + "|" +
        					_discharge;

        int hashValue = hashString.hashCode();

        return hashValue;
    }
//  ------------------------------------------------------------------------------  

    /**
     * @param model The model to set.
     */
    public void setModel(String model)
    {
        _model = model;
    }
//  ------------------------------------------------------------------------------  

    /**
     * @return Returns the model.
     */
    public String getModel()
    {
        return _model;
    }
//  ------------------------------------------------------------------------------  
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        buffer.append("location = " + _locationId + " " +
                	  "area = " + _areaId + " " +
                	  "model = " + _model + " " +
                	  "duration = " + DurationCode.getHoursFromCode(_dur) + " " +
                	  "ordinal = " +_ordinal + " " +
                	  "discharge = " + _discharge);
        
        return buffer.toString();
    }
    //------------------------------------------------------------------------------  

   

}
