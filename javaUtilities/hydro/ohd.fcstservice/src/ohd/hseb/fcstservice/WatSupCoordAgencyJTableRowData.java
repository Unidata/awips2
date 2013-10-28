package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class WatSupCoordAgencyJTableRowData extends AbstractJTableRowData
{
	private String watsupCoordAgency;
	private String missingRep = null;
	
	public WatSupCoordAgencyJTableRowData()
	{
	}
	
	public WatSupCoordAgencyJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getWatsupCoordAgency();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		WatSupCoordAgencyJTableRowData watsupCoordAgencyRecord = (WatSupCoordAgencyJTableRowData) rowData;
		if(columnName.equals("Wat Sup Coord Agency"))
			ret = compareStrings(getWatsupCoordAgency(), watsupCoordAgencyRecord.getWatsupCoordAgency());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Wat Sup Coord Agency"))
			dataValue = getStringValue(getWatsupCoordAgency());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Wat Sup Coord Agency", CellType.STRING, getWatsupCoordAgency(), missingRep));
	}
	
	public String getWatsupCoordAgency() {
		return watsupCoordAgency;
	}

	public void setWatsupCoordAgency(String watsupCoordAgency) {
		this.watsupCoordAgency = watsupCoordAgency;
	}
}
