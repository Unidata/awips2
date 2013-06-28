package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class RequiredPeriodJTableRowData extends AbstractJTableRowData 
{
	private String requiredPeriod;
	private String missingRep = null;
	
	public RequiredPeriodJTableRowData()
	{
	}
	
	public RequiredPeriodJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getRequiredPeriod();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		RequiredPeriodJTableRowData requiredPeriodMethodRecord = (RequiredPeriodJTableRowData) rowData;
		if(columnName.equals("Required Period"))
			ret = compareStrings(getRequiredPeriod(), requiredPeriodMethodRecord.getRequiredPeriod());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Required Period"))
			dataValue = getStringValue(getRequiredPeriod());
		return dataValue;
	}
*/
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Required Period", CellType.STRING, getRequiredPeriod(), missingRep));
	}
	
	public String getRequiredPeriod() {
		return requiredPeriod;
	}

	public void setRequiredPeriod(String requiredPeriod) {
		this.requiredPeriod = requiredPeriod;
	}
}
