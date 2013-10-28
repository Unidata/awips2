package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FcstHorizonJTableRowData  extends AbstractJTableRowData 
{
	private String fcstHorizon;
	private String missingRep = null;
	
	public FcstHorizonJTableRowData()
	{
	}
	
	public FcstHorizonJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getFcstHorizon();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		FcstHorizonJTableRowData fcstHorizonRecord = (FcstHorizonJTableRowData) rowData;
		if(columnName.equals("Horizon"))
			ret = compareStrings(getFcstHorizon(), fcstHorizonRecord.getFcstHorizon());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Horizon"))
			dataValue = getStringValue(getFcstHorizon());
		return dataValue;
	}
*/	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Horizon", CellType.STRING, getFcstHorizon(), missingRep));
	}
	
	public String getFcstHorizon() {
		return fcstHorizon;
	}

	public void setFcstHorizon(String fcstHorizon) {
		this.fcstHorizon = fcstHorizon;
	}
}
