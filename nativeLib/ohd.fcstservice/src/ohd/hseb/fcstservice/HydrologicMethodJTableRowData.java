package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class HydrologicMethodJTableRowData extends AbstractJTableRowData
{
	private String hydrologicMethod;
	private String missingRep = null;
	
	public HydrologicMethodJTableRowData()
	{
	}
	
	public HydrologicMethodJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getHydrologicMethod();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		HydrologicMethodJTableRowData hydrologicMethodRecord = (HydrologicMethodJTableRowData) rowData;
		if(columnName.equals("Hydrologic Method"))
			ret = compareStrings(getHydrologicMethod(), hydrologicMethodRecord.getHydrologicMethod());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Hydrologic Method"))
			dataValue = getStringValue(getHydrologicMethod());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Hydrologic Method", CellType.STRING, getHydrologicMethod(), missingRep));
	}
	
	public String getHydrologicMethod() {
		return hydrologicMethod;
	}

	public void setHydrologicMethod(String hydrologicMethod) {
		this.hydrologicMethod = hydrologicMethod;
	}



}
