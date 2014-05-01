package ohd.hseb.fcstservice;

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class ServiceTypeJTableRowData extends AbstractJTableRowData 
{
	private String serviceType;
	private String missingRep = null;
	
	public ServiceTypeJTableRowData()
	{
	}
	
	public ServiceTypeJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getServiceType();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		ServiceTypeJTableRowData serviceTypeRecord = (ServiceTypeJTableRowData) rowData;
		if(columnName.equals("Service Type"))
			ret = compareStrings(getServiceType(), serviceTypeRecord.getServiceType());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Service Type"))
			dataValue = getStringValue(getServiceType());
		return dataValue;
	}
*/	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Service Type", CellType.STRING, getServiceType(), missingRep));
	}
	
	public String getServiceType() {
		return serviceType;
	}

	public void setServiceType(String serviceType) {
		this.serviceType = serviceType;
	}
}
