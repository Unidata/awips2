package ohd.hseb.fcstservice;  

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class SnowMethodJTableRowData  extends AbstractJTableRowData
{
	private String snowMethod;
	private String missingRep = null;
	
	public SnowMethodJTableRowData()
	{
	}
	
	public SnowMethodJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getSnowMethod();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		SnowMethodJTableRowData snowMethodRecord = (SnowMethodJTableRowData) rowData;
		if(columnName.equals("Snow Method"))
			ret = compareStrings(getSnowMethod(), snowMethodRecord.getSnowMethod());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Snow Method"))
			dataValue = getStringValue(getSnowMethod());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Snow Method", CellType.STRING, getSnowMethod(), missingRep));
	}
	
	public String getSnowMethod() {
		return snowMethod;
	}

	public void setSnowMethod(String snowMethod) {
		this.snowMethod = snowMethod;
	}
}
