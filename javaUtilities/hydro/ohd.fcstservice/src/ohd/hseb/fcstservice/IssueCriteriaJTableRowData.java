package ohd.hseb.fcstservice;  

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class IssueCriteriaJTableRowData extends AbstractJTableRowData 
{
	private String issueCriteria;
	private String missingRep = null;
	
	public IssueCriteriaJTableRowData()
	{
	}
	
	public IssueCriteriaJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getIssueCriteria();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		IssueCriteriaJTableRowData issueCriteriaMethodRecord = (IssueCriteriaJTableRowData) rowData;
		if(columnName.equals("Def Issue Criteria"))
			ret = compareStrings(getIssueCriteria(), issueCriteriaMethodRecord.getIssueCriteria());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Def Issue Criteria"))
			dataValue = getStringValue(getIssueCriteria());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Def Issue Criteria", CellType.STRING, getIssueCriteria(), missingRep));
	}
	
	public String getIssueCriteria() {
		return issueCriteria;
	}

	public void setIssueCriteria(String issueCriteria) {
		this.issueCriteria = issueCriteria;
	}
}
