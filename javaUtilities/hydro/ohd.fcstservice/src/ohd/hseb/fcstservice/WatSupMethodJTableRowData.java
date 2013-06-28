package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class WatSupMethodJTableRowData  extends AbstractJTableRowData 
{
	private String watsupMethod;
	private String missingRep = null;
	
	public WatSupMethodJTableRowData()
	{
	}
	
	public WatSupMethodJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getWatsupMethod();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		WatSupMethodJTableRowData watsupMethodRecord = (WatSupMethodJTableRowData) rowData;
		if(columnName.equals("Wat Sup Method"))
			ret = compareStrings(getWatsupMethod(), watsupMethodRecord.getWatsupMethod());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Wat Sup Method"))
			dataValue = getStringValue(getWatsupMethod());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Wat Sup Method", CellType.STRING, getWatsupMethod(), missingRep));
	}
	
	public String getWatsupMethod() {
		return watsupMethod;
	}

	public void setWatsupMethod(String watsupMethod) {
		this.watsupMethod = watsupMethod;
	}
}
