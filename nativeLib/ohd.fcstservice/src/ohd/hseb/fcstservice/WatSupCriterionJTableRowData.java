package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class WatSupCriterionJTableRowData extends AbstractJTableRowData
{
	private String watsupCriterion;
	private String missingRep = null;
	
	public WatSupCriterionJTableRowData()
	{
	}
	
	public WatSupCriterionJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getWatsupCriterion();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		WatSupCriterionJTableRowData watsupCriterionRecord = (WatSupCriterionJTableRowData) rowData;
		if(columnName.equals("Wat Sup Criterion"))
			ret = compareStrings(getWatsupCriterion(), watsupCriterionRecord.getWatsupCriterion());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Wat Sup Criterion"))
			dataValue = getStringValue(getWatsupCriterion());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Wat Sup Criterion", CellType.STRING, getWatsupCriterion(), missingRep));
	}
	
	public String getWatsupCriterion() {
		return watsupCriterion;
	}

	public void setWatsupCriterion(String watsupCriterion) {
		this.watsupCriterion = watsupCriterion;
	}

}
