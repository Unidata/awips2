package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FlowTypeJTableRowData extends AbstractJTableRowData
{
	private String flowType;
	private String missingRep = null;
	
	public FlowTypeJTableRowData()
	{
	}
	
	public FlowTypeJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getFlowType();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		FlowTypeJTableRowData flowTypeRecord = (FlowTypeJTableRowData) rowData;
		if(columnName.equals("Flow Type"))
			ret = compareStrings(getFlowType(), flowTypeRecord.getFlowType());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Flow Type"))
			dataValue = getStringValue(getFlowType());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Flow Type", CellType.STRING, getFlowType(), missingRep));
	}
	
	public String getFlowType() {
		return flowType;
	}

	public void setFlowType(String flowType) {
		this.flowType = flowType;
	}


}
