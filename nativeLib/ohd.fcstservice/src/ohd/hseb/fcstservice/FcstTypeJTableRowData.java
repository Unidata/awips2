package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FcstTypeJTableRowData extends AbstractJTableRowData 
{
	private String fcstType;
	private String missingRep = null;
	
	public FcstTypeJTableRowData()
	{
	}
	
	public FcstTypeJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getFcstType();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		FcstTypeJTableRowData fcstTypeRecord = (FcstTypeJTableRowData) rowData;
		if(columnName.equals("Fcst Type"))
			ret = compareStrings(getFcstType(), fcstTypeRecord.getFcstType());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Fcst Type"))
			dataValue = getStringValue(getFcstType());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Fcst Type", CellType.STRING, getFcstType(), missingRep));
	}
	
	public String getFcstType() {
		return fcstType;
	}

	public void setFcstType(String fcstType) {
		this.fcstType = fcstType;
	}


}
