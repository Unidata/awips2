package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class RoutingMethodJTableRowData extends AbstractJTableRowData 
{
	private String routingMethod;
	private String missingRep = null;
	
	public RoutingMethodJTableRowData()
	{
	}
	
	public RoutingMethodJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getRoutingMethod();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		RoutingMethodJTableRowData routingMethodRecord = (RoutingMethodJTableRowData) rowData;
		if(columnName.equals("Routing Method"))
			ret = compareStrings(getRoutingMethod(), routingMethodRecord.getRoutingMethod());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Routing Method"))
			dataValue = getStringValue(getRoutingMethod());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Routing Method", CellType.STRING, getRoutingMethod(), missingRep));
	}
	
	public String getRoutingMethod() {
		return routingMethod;
	}

	public void setRoutingMethod(String routingMethod) {
		this.routingMethod = routingMethod;
	}

}
