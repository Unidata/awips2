package ohd.hseb.fcstservice;    

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class LocationJTableRowData extends AbstractJTableRowData 
{
	private String locId;
	
	public LocationJTableRowData()
	{
	}
	
	public LocationJTableRowData(String missingRepresentation)
	{
		setMissingRepresentation(missingRepresentation);
	}
	
	public String toString()
	{
		String str = getLocId();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		LocationJTableRowData riverstatRecord = (LocationJTableRowData) rowData;
		if(columnName.equals("Upstream Segment"))
			ret = compareStrings(getLocId(), riverstatRecord.getLocId());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Upstream Segment"))
			dataValue = getStringValue(getLocId());
		return dataValue;
	}

	public String getLocId() {
		return locId;
	}

	public void setLocId(String locId) {
		this.locId = locId;
	}
}
