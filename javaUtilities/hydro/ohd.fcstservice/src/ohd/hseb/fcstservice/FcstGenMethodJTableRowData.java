package ohd.hseb.fcstservice;  

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FcstGenMethodJTableRowData extends AbstractJTableRowData
{
	private String genMethod;
	private String missingRep = null;
	
	public FcstGenMethodJTableRowData()
	{
	}
	
	public FcstGenMethodJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getGenMethod();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		FcstGenMethodJTableRowData genMethodRecord = (FcstGenMethodJTableRowData) rowData;
		if(columnName.equals("Typical Fcst Gen Method"))
			ret = compareStrings(getGenMethod(), genMethodRecord.getGenMethod());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Typical Fcst Gen Method"))
			dataValue = getStringValue(getGenMethod());
		return dataValue;
	}
*/		
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Typical Fcst Gen Method", CellType.STRING, getGenMethod(), missingRep));
	}
	

	public String getGenMethod() {
		return genMethod;
	}

	public void setGenMethod(String genMethod) {
		this.genMethod = genMethod;
	}

}
