package ohd.hseb.fcstservice;  

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FrequencyUpdateJTableRowData extends AbstractJTableRowData 
{
	private String frequencyUpdate;
	private String missingRep = null;
	
	public FrequencyUpdateJTableRowData()
	{
	}
	
	public FrequencyUpdateJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getFrequencyUpdate();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		FrequencyUpdateJTableRowData frequencyUpdateMethodRecord = (FrequencyUpdateJTableRowData) rowData;
		if(columnName.equals("Frequency Update"))
			ret = compareStrings(getFrequencyUpdate(), frequencyUpdateMethodRecord.getFrequencyUpdate());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Frequency Update"))
			dataValue = getStringValue(getFrequencyUpdate());
		return dataValue;
	}
*/	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Frequency Update", CellType.STRING, getFrequencyUpdate(), missingRep));
	}

	public String getFrequencyUpdate() {
		return frequencyUpdate;
	}

	public void setFrequencyUpdate(String frequencyUpdate) {
		this.frequencyUpdate = frequencyUpdate;
	}


}
