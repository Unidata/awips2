package ohd.hseb.fcstservice;  

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FcstPtDetermJTableRowData extends AbstractJTableRowData
{
	private String lid;
	private long implDate;
	private long webDate;
	private String snowMethod;
	private String hydrolMethod;
	private String hydraulMethod;
	private String issueCriteria;
	private String normal;
	private String flood;
	private String varUsage;
	private String draught;
	private String horizon;
	private int qpf;
	private int qtf;
	private int qzf;
	private String upstreamSeg;
	private String reservoirModel;
	private String genMethod;
	private String consumptiveUse;
	private String channelLoss;
	private int numElevZones;
	
	public FcstPtDetermJTableRowData()
	{
	}
	
	public FcstPtDetermJTableRowData(String missingRepresentation)
	{
		setMissingRepresentation(missingRepresentation);
	}
	
/*	public String toString()
	{
        String str = getLid()+" "+
	                 getImplDate()+" "+
	                 getWebDate()+" "+
	                 getSnowMethod()+" "+
	                 getDraught()+" "+
	                 getFlood()+" "+
	                 getNormal()+" "+
	                 getHorizon()+" "+
	                 getHydraulMethod()+" "+
	                 getHydrolMethod()+" "+
	                 getIssueCriteria()+" "+
	                 getQpf()+" "+
	                 getQtf()+" "+
	                 getQzf()+" "+
        			 getUpstreamSeg()+" "+
        			 getReservoirModel()+" "+
        			 getGenMethod()+" "+
        			 getConsumptiveUse()+" "+
        			 getChannelLoss()+" "+
        			 getNumElevZones();
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
		else if (columnName.equals("Snow Method"))
			dataValue = getStringValue(getSnowMethod());
		else if(columnName.equals("Hydrologic Method"))
			dataValue = getStringValue(getHydrolMethod());
		else if (columnName.equals("Routing Method"))
			dataValue = getStringValue(getHydraulMethod());
		else if (columnName.equals("Def Issue Criteria"))
			dataValue = getStringValue(getIssueCriteria());
		else if(columnName.equals("Normal Upd Freq"))
			dataValue = getStringValue(getNormal());
		else if(columnName.equals("Flood Upd Freq"))
			dataValue = getStringValue(getFlood());
		else if (columnName.equals("Drought Upd Freq"))
			dataValue = getStringValue(getDraught());
		else if (columnName.equals("Forecast Period"))
			dataValue = getStringValue(getHorizon());
		else if(columnName.equals("Qpf(hours)"))
			dataValue = getStringValue(getQpf());
		else if (columnName.equals("Qtf(hours)"))
			dataValue = getStringValue(getQtf());
		else if (columnName.equals("Qzf(hours)"))
			dataValue = getStringValue(getQzf());		
		else if(columnName.equals("Upstream Segment"))
			dataValue = getStringValue(getUpstreamSeg());
		else if (columnName.equals("Reservoir Model"))
			dataValue = getStringValue(getReservoirModel());
		else if (columnName.equals("Typical Fcst Gen Method"))
			dataValue = getStringValue(getGenMethod());
		else if(columnName.equals("Consumptive Use"))
			dataValue = getStringValue(getConsumptiveUse());
		else if (columnName.equals("Channel Loss"))
			dataValue = getStringValue(getChannelLoss());
		else if (columnName.equals("Num Elev Zones"))
			dataValue = getStringValue(getNumElevZones());
		return dataValue;
	}
		
	public void setFcstPtDetermJTableRecord(FcstPtDetermRecord fcstPtDetermRecord)
	{
		setLid(fcstPtDetermRecord.getLid());
		setUpstreamSeg(fcstPtDetermRecord.getUpstream_seg());
		setReservoirModel(fcstPtDetermRecord.getReservoir_model());
		setGenMethod(fcstPtDetermRecord.getFcst_gen_method());
		setConsumptiveUse(fcstPtDetermRecord.getConsumptive_use());
		setChannelLoss(fcstPtDetermRecord.getChannel_loss());
		Long val = new Long(fcstPtDetermRecord.getImpl_date());
		setImplDate(val.toString());
		val = new Long(fcstPtDetermRecord.getWeb_date());
		setWebDate(val.toString());
		setSnowMethod(fcstPtDetermRecord.getSnow_method());
		Integer int1 = new Integer(fcstPtDetermRecord.getFrequpd_flood());
		setFlood(int1.toString());
		int1 = new Integer(fcstPtDetermRecord.getFrequpd_normal());
		setNormal(int1.toString());
		int1 = new Integer(fcstPtDetermRecord.getFrequpd_drought());
		setDraught(int1.toString());
		setHydraulMethod(fcstPtDetermRecord.getHydraul_method());
		setHydrolMethod(fcstPtDetermRecord.getHydrol_method());
		setIssueCriteria(fcstPtDetermRecord.getDef_issue_crit());
		int1 = new Integer(fcstPtDetermRecord.getFcst_horizon());
		setHorizon(int1.toString());
		int1 = new Integer(fcstPtDetermRecord.getHours_qpf());
		setQpf(int1.toString());
		int1 = new Integer(fcstPtDetermRecord.getHours_qtf());
		setQtf(int1.toString());
		int1 = new Integer(fcstPtDetermRecord.getHours_qzf());
		setQzf(int1.toString());
		int1 = new Integer(fcstPtDetermRecord.getNum_elev_zones());
		setNumElevZones(int1.toString());
	}
	
	public int compare(String columnName, JTableRowData rowData)
	{
		int ret = 0;
		FcstPtDetermJTableRowData fcstPtdetermRecord = (FcstPtDetermJTableRowData) rowData;
		if(columnName.equals("Location ID"))
			ret = compareStrings(getLid(), fcstPtdetermRecord.getLid());
		else if (columnName.equals("Implementation Date"))
			ret = compareStrings(getImplDate(), fcstPtdetermRecord.getImplDate());
		else if (columnName.equals("Web Date"))
			ret = compareStrings(getWebDate(), fcstPtdetermRecord.getWebDate());
		else if (columnName.equals("Snow Method"))
			ret = compareStrings(getSnowMethod(), fcstPtdetermRecord.getSnowMethod());
		else if(columnName.equals("Hydrologic Method"))
			ret = compareStrings(getHydrolMethod(), fcstPtdetermRecord.getHydrolMethod());
		else if (columnName.equals("Routing Method"))
			ret = compareStrings(getHydraulMethod(), fcstPtdetermRecord.getHydraulMethod());
		else if (columnName.equals("Def Issue Criteria"))
			ret = compareStrings(getIssueCriteria(), fcstPtdetermRecord.getIssueCriteria());
		else if(columnName.equals("Normal Upd Freq"))
			ret = compareStrings(getNormal(), fcstPtdetermRecord.getNormal());
		else if(columnName.equals("Flood Upd Freq"))
			ret = compareStrings(getFlood(), fcstPtdetermRecord.getFlood());
		else if (columnName.equals("Drought Upd Freq"))
			ret = compareStrings(getDraught(), fcstPtdetermRecord.getDraught());
		else if (columnName.equals("Forecast Period"))
			ret = compareStrings(getHorizon(), fcstPtdetermRecord.getHorizon());
		else if(columnName.equals("Qpf(hours)"))
			ret = compareStrings(getQpf(), fcstPtdetermRecord.getQpf());
		else if (columnName.equals("Qtf(hours)"))
			ret = compareStrings(getQtf(), fcstPtdetermRecord.getQtf());
		else if (columnName.equals("Qzf(hours)"))
			ret = compareStrings(getQzf(), fcstPtdetermRecord.getQzf());		
		else if(columnName.equals("Upstream Segment"))
			ret = compareStrings(getUpstreamSeg(), fcstPtdetermRecord.getUpstreamSeg());
		else if (columnName.equals("Reservoir Model"))
			ret = compareStrings(getReservoirModel(), fcstPtdetermRecord.getReservoirModel());
		else if (columnName.equals("Typical Fcst Gen Method"))
			ret = compareStrings(getGenMethod(), fcstPtdetermRecord.getGenMethod());
		else if(columnName.equals("Consumptive Use"))
			ret = compareStrings(getConsumptiveUse(), fcstPtdetermRecord.getConsumptiveUse());
		else if (columnName.equals("Channel Loss"))
			ret = compareStrings(getChannelLoss(), fcstPtdetermRecord.getChannelLoss());
		else if (columnName.equals("Num Elev Zones"))
			ret = compareStrings(getNumElevZones(), fcstPtdetermRecord.getNumElevZones());
		return ret;
	}*/
	
	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Location ID", CellType.STRING, getLid(), getMissingRepresentation()));
		addCell(new BaseTableCell("Implementation Date", CellType.DATE, getImplDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Web Date", CellType.DATE, getWebDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Hydrologic Method", CellType.STRING, getHydrolMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Snow Method", CellType.STRING, getSnowMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Routing Method", CellType.STRING, getHydraulMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Def Issue Criteria", CellType.STRING, getIssueCriteria(), getMissingRepresentation()));
		addCell(new BaseTableCell("Qpf(hours)", CellType.INTEGER, getQpf(), getMissingRepresentation()));
		addCell(new BaseTableCell("Upstream Segment", CellType.STRING, getUpstreamSeg(), getMissingRepresentation()));
		addCell(new BaseTableCell("Reservoir Model", CellType.STRING, getReservoirModel(), getMissingRepresentation()));
		addCell(new BaseTableCell("Normal Upd Freq", CellType.STRING, getNormal(), getMissingRepresentation()));
		addCell(new BaseTableCell("Flood Upd Freq", CellType.STRING, getFlood(), getMissingRepresentation()));
		addCell(new BaseTableCell("VAR Usage", CellType.STRING, getVarUsage(), getMissingRepresentation()));
		addCell(new BaseTableCell("Drought Upd Freq", CellType.STRING, getVarUsage(), getMissingRepresentation()));
		addCell(new BaseTableCell("Forecast Period", CellType.STRING, getHorizon(), getMissingRepresentation()));
		addCell(new BaseTableCell("Typical Fcst Gen Method", CellType.STRING, getGenMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Consumptive Use", CellType.STRING, getConsumptiveUse(), getMissingRepresentation()));
		addCell(new BaseTableCell("Qtf(hours)", CellType.INTEGER, getQtf(), getMissingRepresentation()));
		addCell(new BaseTableCell("Qzf(hours)", CellType.INTEGER, getQzf(), getMissingRepresentation()));
		addCell(new BaseTableCell("Channel Loss", CellType.STRING, getChannelLoss(), getMissingRepresentation()));
		addCell(new BaseTableCell("Num Elev Zones", CellType.INTEGER, getNumElevZones(), getMissingRepresentation()));
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

	public String getSnowMethod() {
		return snowMethod;
	}

	public void setSnowMethod(String snowMethod) {
		this.snowMethod = snowMethod;
	}

	public String getHydrolMethod() {
		return hydrolMethod;
	}

	public void setHydrolMethod(String hydrolMethod) {
		this.hydrolMethod = hydrolMethod;
	}

	public String getHydraulMethod() {
		return hydraulMethod;
	}

	public void setHydraulMethod(String hydraulMethod) {
		this.hydraulMethod = hydraulMethod;
	}

	public String getIssueCriteria() {
		return issueCriteria;
	}

	public void setIssueCriteria(String issueCriteria) {
		this.issueCriteria = issueCriteria;
	}

	public String getNormal() {
		return normal;
	}

	public void setNormal(String normal) {
		this.normal = normal;
	}

	public String getFlood() {
		return flood;
	}

	public void setFlood(String flood) {
		this.flood = flood;
	}

	public String getVarUsage() {
		return varUsage;
	}

	public void setVarUsage(String varUsage) {
		this.varUsage = varUsage;
	}

	public String getDraught() {
		return draught;
	}

	public void setDraught(String draught) {
		this.draught = draught;
	}

	public String getHorizon() {
		return horizon;
	}

	public void setHorizon(String horizon) {
		this.horizon = horizon;
	}

	public int getQpf() {
		return qpf;
	}

	public void setQpf(int qpf) {
		this.qpf = qpf;
	}

	public int getQtf() {
		return qtf;
	}

	public void setQtf(int qtf) {
		this.qtf = qtf;
	}

	public int getQzf() {
		return qzf;
	}

	public void setQzf(int qzf) {
		this.qzf = qzf;
	}

	public String getUpstreamSeg() {
		return upstreamSeg;
	}

	public void setUpstreamSeg(String upstreamSeg) {
		this.upstreamSeg = upstreamSeg;
	}

	public String getReservoirModel() {
		return reservoirModel;
	}

	public void setReservoirModel(String reservoirModel) {
		this.reservoirModel = reservoirModel;
	}

	public String getGenMethod() {
		return genMethod;
	}

	public void setGenMethod(String genMethod) {
		this.genMethod = genMethod;
	}

	public String getConsumptiveUse() {
		return consumptiveUse;
	}

	public void setConsumptiveUse(String consumptiveUse) {
		this.consumptiveUse = consumptiveUse;
	}

	public String getChannelLoss() {
		return channelLoss;
	}

	public void setChannelLoss(String channelLoss) {
		this.channelLoss = channelLoss;
	}

	public int getNumElevZones() {
		return numElevZones;
	}

	public void setNumElevZones(int numElevZones) {
		this.numElevZones = numElevZones;
	}


}
