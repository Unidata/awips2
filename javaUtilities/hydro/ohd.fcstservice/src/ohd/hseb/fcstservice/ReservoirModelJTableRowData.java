package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class ReservoirModelJTableRowData  extends AbstractJTableRowData 
{
	private String reservoirModel;
	private String missingRep = null;
	
	public ReservoirModelJTableRowData()
	{
	}
	
	public ReservoirModelJTableRowData(String missingRepresentation)
	{
		//setMissingRepresentation(missingRepresentation);
		missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getReservoirModel();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		ReservoirModelJTableRowData reservoirModelRecord = (ReservoirModelJTableRowData) rowData;
		if(columnName.equals("Reservoir Model"))
			ret = compareStrings(getReservoirModel(), reservoirModelRecord.getReservoirModel());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Reservoir Model"))
			dataValue = getStringValue(getReservoirModel());
		return dataValue;
	}
*/	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Reservoir Model", CellType.STRING, getReservoirModel(), missingRep));
	}
	
	public String getReservoirModel() {
		return reservoirModel;
	}

	public void setReservoirModel(String reservoirModel) {
		this.reservoirModel = reservoirModel;
	}
}
