package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FcstPtServiceJTableRowData extends AbstractJTableRowData
{
	private String lid;
	private long implDate;
	private long webDate;
	private long startDate;
	private long endDate;
	private double floodTher;
	private double drainageArea;
	private String verifRespType;
	private int exceedProb;
	private String serviceType;
	
	//private String missingRep = null;
	
	public FcstPtServiceJTableRowData()
	{
	}
		
/*	public void setFcstPtServiceJTableRecord(FcstPtServiceRecord fcstPtServiceRecord)
	{
		setLid(fcstPtServiceRecord.getLid());
		setServiceType(fcstPtServiceRecord.getService_type());
		Double val1 = new Double(fcstPtServiceRecord.getFlood_thres());
		setFloodTher(val1.toString());
		Integer val2 = new Integer(fcstPtServiceRecord.getExceed_prob());
		setExceedProb(val2.toString());
		Long val = new Long(fcstPtServiceRecord.getImpl_date());
		setImplDate(val.toString());
		val = new Long(fcstPtServiceRecord.getWeb_date());
		setWebDate(val.toString());
		val = new Long(fcstPtServiceRecord.getAnal_start_date());
		setStartDate(val.toString());
		val = new Long(fcstPtServiceRecord.getAnal_end_date());
		setEndDate(val.toString());
	}
*/	
	public FcstPtServiceJTableRowData(String missingRepresentation)
	{
		setMissingRepresentation(missingRepresentation);
		//missingRep = missingRepresentation;
	}
	
/*	public String toString()
	{
		String str = getLid()+" "+
					 getServiceType()+" "+
					 getFloodTher()+" "+
					 getExceedProb()+" "+
	                 getImplDate()+" "+
	                 getStartDate()+" "+
	                 getEndDate()+" "+
	                 getWebDate();
		return str;
	}
	
	public int compare(String columnName, JTableRowData rowData) 
	{
		int ret = 0;
		FcstPtServiceJTableRowData fcstPtServiceRecord = (FcstPtServiceJTableRowData) rowData;
		if(columnName.equals("Location ID"))
			ret = compareStrings(getLid(), fcstPtServiceRecord.getLid());
		else if (columnName.equals("Implementation Date"))
			ret = compareStrings(getImplDate(), fcstPtServiceRecord.getImplDate());
		else if (columnName.equals("Web Date"))
			ret = compareStrings(getWebDate(), fcstPtServiceRecord.getWebDate());
		else if (columnName.equals("Analysis Start Date"))
			ret = compareStrings(getStartDate(), fcstPtServiceRecord.getStartDate());
		else if (columnName.equals("Analysis End Date"))
			ret = compareStrings(getEndDate(), fcstPtServiceRecord.getEndDate());
		else if (columnName.equals("Flood Threshold"))
			ret = compareNumbers(Double.parseDouble(getFloodTher()), Double.parseDouble(fcstPtServiceRecord.getFloodTher()));
		else if (columnName.equals("Exceed Probability %"))
			ret = compareStrings(getExceedProb(), fcstPtServiceRecord.getExceedProb());
		else if (columnName.equals("Service Type"))
			ret = compareStrings(getServiceType(), fcstPtServiceRecord.getServiceType());
		return ret;
	}
	
	public String getDataValue(String columnName) 
	{
		String dataValue = null;
		if(columnName.equals("Location ID"))
			dataValue = getStringValue(getLid());
		else if (columnName.equals("Implementation Date"))
			dataValue = getStringValue(getImplDate());
		else if (columnName.equals("Web Date"))
			dataValue = getStringValue(getWebDate());
		else if (columnName.equals("Analysis Start Date"))
			dataValue = getStringValue(getStartDate());
		else if (columnName.equals("Analysis End Date"))
			dataValue = getStringValue(getEndDate());
		else if (columnName.equals("Flood Threshold"))
			dataValue = getStringValue(getFloodTher());
		else if (columnName.equals("Exceed Probability %"))
			dataValue = getStringValue(getExceedProb());
		else if (columnName.equals("Service Type"))
			dataValue = getStringValue(getServiceType());
		return dataValue;
	}
*/
	
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Location ID", CellType.STRING, getLid(), getMissingRepresentation()));
		addCell(new BaseTableCell("Implementation Date", CellType.DATE, getImplDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Web Date", CellType.DATE, getWebDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Analysis Start Date", CellType.DATE, getStartDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Analysis End Date", CellType.DATE, getEndDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Flood Threshold", CellType.DOUBLE, getFloodTher(), getMissingRepresentation()));
		addCell(new BaseTableCell("Drainage Area", CellType.DOUBLE, getDrainageArea(), getMissingRepresentation()));
		addCell(new BaseTableCell("Exceed Probability %", CellType.INTEGER, getExceedProb(), getMissingRepresentation()));
		addCell(new BaseTableCell("Service Type", CellType.STRING, getServiceType(), getMissingRepresentation()));
		addCell(new BaseTableCell("Verification Response Type", CellType.STRING, getVerifRespType(), getMissingRepresentation()));
	}

	public String getLid() {
		return lid;
	}

	public void setLid(String lid) {
		this.lid = lid;
	}

	public long getImplDate() {
		return implDate;
	}

	public void setImplDate(long implDate) {
		this.implDate = implDate;
	}

	public long getWebDate() {
		return webDate;
	}

	public void setWebDate(long webDate) {
		this.webDate = webDate;
	}

	public long getStartDate() {
		return startDate;
	}

	public void setStartDate(long startDate) {
		this.startDate = startDate;
	}

	public long getEndDate() {
		return endDate;
	}

	public void setEndDate(long endDate) {
		this.endDate = endDate;
	}

	public double getFloodTher() {
		return floodTher;
	}

	public void setFloodTher(double floodTher) {
		this.floodTher = floodTher;
	}

	public double getDrainageArea() {
		return drainageArea;
	}

	public void setDrainageArea(double drainageArea) {
		this.drainageArea = drainageArea;
	}

	public String getVerifRespType() {
		return verifRespType;
	}

	public void setVerifRespType(String verifRespType) {
		this.verifRespType = verifRespType;
	}

	public int getExceedProb() {
		return exceedProb;
	}

	public void setExceedProb(int exceedProb) {
		this.exceedProb = exceedProb;
	}

	public String getServiceType() {
		return serviceType;
	}

	public void setServiceType(String serviceType) {
		this.serviceType = serviceType;
	}
}
	
