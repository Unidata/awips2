package ohd.hseb.fcstservice;   

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FcstPtWatSupJTableRowData extends AbstractJTableRowData 
{
	private String lid;
	private long implDate;
	private long webDate;
	private String normal;	
	private String watsupMethod;
	private String watsupCoordAgency;
	private String periodReq;
	private String watsupCrit;
	private String customerDesc;
	private String respAgency;
	
	public FcstPtWatSupJTableRowData()
	{
	}
	
	public FcstPtWatSupJTableRowData(String missingRepresentation)
	{
		setMissingRepresentation(missingRepresentation);
	}

	public String getCustomerDesc() {
		return customerDesc;
	}

	public void setCustomerDesc(String customerDesc) {
		this.customerDesc = customerDesc;
	}

	public long getImplDate() {
		return implDate;
	}

	public void setImplDate(long implDate) {
		this.implDate = implDate;
	}

	public String getLid() {
		return lid;
	}

	public void setLid(String lid) {
		this.lid = lid;
	}

	public String getNormal() {
		return normal;
	}

	public void setNormal(String normal) {
		this.normal = normal;
	}

	public String getPeriodReq() {
		return periodReq;
	}

	public void setPeriodReq(String periodReq) {
		this.periodReq = periodReq;
	}

	public String getRespAgency() {
		return respAgency;
	}

	public void setRespAgency(String respAgency) {
		this.respAgency = respAgency;
	}

	public String getWatsupCoordAgency() {
		return watsupCoordAgency;
	}

	public void setWatsupCoordAgency(String watsupCoordAgency) {
		this.watsupCoordAgency = watsupCoordAgency;
	}

	public String getWatsupCrit() {
		return watsupCrit;
	}

	public void setWatsupCrit(String watsupCrit) {
		this.watsupCrit = watsupCrit;
	}

	public String getWatsupMethod() {
		return watsupMethod;
	}

	public void setWatsupMethod(String watsupMethod) {
		this.watsupMethod = watsupMethod;
	}

	public long getWebDate() {
		return webDate;
	}

	public void setWebDate(long webDate) {
		this.webDate = webDate;
	}
	
/*	public String toString()
	{
		String str = getLid()+" "+
	                 getImplDate()+" "+
	                 getWebDate()+" "+
	                 getNormal()+" "+
	                 getWatsupMethod()+" "+
	                 getWatsupCoordAgency()+" "+
	                 getPeriodReq()+" "+
	                 getWatsupCrit()+" "+
	                 getRespAgency()+" "+
	                 getCustomerDesc();
		return str;
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
		else if (columnName.equals("Normal Upd Freq"))
			dataValue = getStringValue(getNormal());		
		else if (columnName.equals("Wat Sup Method"))
			dataValue = getStringValue(getWatsupMethod());
		else if (columnName.equals("Wat Sup Coord Agency"))
			dataValue = getStringValue(getWatsupCoordAgency());
		else if (columnName.equals("Period Req"))
			dataValue = getStringValue(getPeriodReq());
		else if (columnName.equals("Wat Sup Criteria"))
			dataValue = getStringValue(getWatsupCrit());
		else if (columnName.equals("Customer Desc"))
			dataValue = getStringValue(getCustomerDesc());
		else if (columnName.equals("Wat Sup Resp Agency"))
			dataValue = getStringValue(getRespAgency());
		return dataValue;
	}
		
	public void setFcstPtWatSupJTableRecord(FcstPtWatSupRecord fcstPtWatsupRecord)
	{
		setLid(fcstPtWatsupRecord.getLid());
		Long val = new Long(fcstPtWatsupRecord.getImpl_date());
		setImplDate(val.toString());
		val = new Long(fcstPtWatsupRecord.getWeb_date());
		setWebDate(val.toString());
		setNormal(fcstPtWatsupRecord.getFrequpd_normal());		
		setWatsupMethod(fcstPtWatsupRecord.getWatsup_method());
		setWatsupCoordAgency(fcstPtWatsupRecord.getWatsup_coord_agency());
		setPeriodReq(fcstPtWatsupRecord.getPeriod_req());
		setWatsupCrit(fcstPtWatsupRecord.getWatsup_crit());
		setCustomerDesc(fcstPtWatsupRecord.getCustomer_desc());
		setRespAgency(fcstPtWatsupRecord.getWatsup_resp_agency());
	}
	
	public int compare(String columnName, JTableRowData rowData) //To be changed in all Windows 
	{
		int ret = 0;
		FcstPtWatSupJTableRowData fcstPtWatsupRecord = (FcstPtWatSupJTableRowData) rowData;
		if(columnName.equals("Location ID"))
			ret = compareStrings(getLid(), fcstPtWatsupRecord.getLid());
		else if (columnName.equals("Implementation Date"))
			ret = compareStrings(getImplDate(), fcstPtWatsupRecord.getImplDate());
		else if (columnName.equals("Web Date"))
			ret = compareStrings(getWebDate(), fcstPtWatsupRecord.getWebDate());
		else if(columnName.equals("Normal Upd Freq"))
			ret = compareStrings(getNormal(), fcstPtWatsupRecord.getNormal());
		else if (columnName.equals("Wat Sup Method"))
			ret = compareStrings(getWatsupMethod(), fcstPtWatsupRecord.getWatsupMethod());
		else if (columnName.equals("Wat Sup Coord Agency"))
			ret = compareStrings(getWatsupCoordAgency(), fcstPtWatsupRecord.getWatsupCoordAgency());
		else if(columnName.equals("Period Req"))
			ret = compareStrings(getPeriodReq(), fcstPtWatsupRecord.getPeriodReq());
		else if (columnName.equals("Wat Sup Criteria"))
			ret = compareStrings(getWatsupCrit(), fcstPtWatsupRecord.getWatsupCrit());
		else if (columnName.equals("Customer Desc"))
			ret = compareStrings(getCustomerDesc(), fcstPtWatsupRecord.getCustomerDesc());
		else if (columnName.equals("Wat Sup Resp Agency"))
			ret = compareStrings(getRespAgency(), fcstPtWatsupRecord.getRespAgency());
		return ret;
	}*/

	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Location ID", CellType.STRING, getLid(), getMissingRepresentation()));
		addCell(new BaseTableCell("Implementation Date", CellType.DATE, getImplDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Web Date", CellType.DATE, getWebDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Wat Sup Method", CellType.STRING, getWatsupMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Wat Sup Coord Agency", CellType.STRING, getWatsupCoordAgency(), getMissingRepresentation()));
		addCell(new BaseTableCell("Normal Upd Freq", CellType.STRING, getNormal(), getMissingRepresentation()));
		addCell(new BaseTableCell("Period Req", CellType.STRING, getPeriodReq(), getMissingRepresentation()));
		addCell(new BaseTableCell("Wat Sup Criteria", CellType.STRING, getWatsupCrit(), getMissingRepresentation()));
		addCell(new BaseTableCell("Customer Desc", CellType.STRING, getCustomerDesc(), getMissingRepresentation()));
		addCell(new BaseTableCell("Wat Sup Resp Agency", CellType.STRING, getRespAgency(), getMissingRepresentation()));
	}
}
