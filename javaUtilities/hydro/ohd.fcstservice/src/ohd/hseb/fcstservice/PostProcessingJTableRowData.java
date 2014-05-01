package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class PostProcessingJTableRowData extends AbstractJTableRowData 
{
	private String postProcessing;
	private String missingRep = null;
	
	public PostProcessingJTableRowData()
	{
	}
	
	public PostProcessingJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getPostProcessing();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		PostProcessingJTableRowData postProcessingRecord = (PostProcessingJTableRowData) rowData;
		if(columnName.equals("Post Processing"))
			ret = compareStrings(getPostProcessing(), postProcessingRecord.getPostProcessing());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Post Processing"))
			dataValue = getStringValue(getPostProcessing());
		return dataValue;
	}
*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Post Processing", CellType.STRING, getPostProcessing(), missingRep));
	}
	
	public String getPostProcessing() {
		return postProcessing;
	}

	public void setPostProcessing(String postProcessing) {
		this.postProcessing = postProcessing;
	}

}
