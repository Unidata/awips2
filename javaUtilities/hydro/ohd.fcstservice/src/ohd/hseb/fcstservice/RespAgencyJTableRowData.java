package ohd.hseb.fcstservice;

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class RespAgencyJTableRowData extends AbstractJTableRowData 
{
	private String respAgency;
	private String missingRep = null;
	
	public RespAgencyJTableRowData()
	{
	}
	
	public RespAgencyJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getRespAgency();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		RespAgencyJTableRowData respAgencyRecord = (RespAgencyJTableRowData) rowData;
		if(columnName.equals("Wat Sup Resp Agency"))
			ret = compareStrings(getRespAgency(), respAgencyRecord.getRespAgency());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Wat Sup Resp Agency"))
			dataValue = getStringValue(getRespAgency());
		return dataValue;
	}
*/	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Wat Sup Resp Agency", CellType.STRING, getRespAgency(), missingRep));
	}
	
	public String getRespAgency() {
		return respAgency;
	}

	public void setRespAgency(String respAgency) {
		this.respAgency = respAgency;
	}
}
