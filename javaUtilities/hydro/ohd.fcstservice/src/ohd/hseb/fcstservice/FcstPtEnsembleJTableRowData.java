package ohd.hseb.fcstservice;  

import ohd.hseb.util.gui.jtable.*;
import ohd.hseb.ihfsdb.generated.*;

public class FcstPtEnsembleJTableRowData extends AbstractJTableRowData
{
	private String lid;
	private long implDate;
	private long webDate;
	private String snowMethod;
	private String hydrolMethod;
	private String hydraulMethod;
	private String flood;
	private String draught;
	private String horizon;	
	private String flowType;
	private String fcstType;
	private String normal;
	private int numMonClim;
	private String postProcessing;
	private int numDayHyd;
	private long externalDate;
	private String upstreamSeg;
	private String reservoirModel;
	private String consumptiveUse;
	private String channelLoss;
	private int numElevZones;
	private String varUsage;
	
	public FcstPtEnsembleJTableRowData()
	{
	}
	
	public FcstPtEnsembleJTableRowData(String missingRepresentation)
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
	                 getHorizon()+" "+
	                 getHydraulMethod()+" "+
	                 getHydrolMethod()+" "+
	                 getFlowType()+" "+
	                 getFcstType()+" "+
	                 getNormal()+" "+
	                 getNumMonClim()+" "+
	                 getPostProcessing()+" "+
	                 getNumDayHyd()+" "+
	                 getExternalDate()+" "+
		 			 getUpstreamSeg()+" "+
		 			 getReservoirModel()+" "+
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
		else if(columnName.equals("Flood Upd Freq"))
			dataValue = getStringValue(getFlood());
		else if (columnName.equals("Drought Upd Freq"))
			dataValue = getStringValue(getDraught());
		else if (columnName.equals("Forecast Period"))
			dataValue = getStringValue(getHorizon());		
		else if (columnName.equals("Flow Type"))
			dataValue = getStringValue(getFlowType());
		else if(columnName.equals("Fcst Type"))
			dataValue = getStringValue(getFcstType());
		else if (columnName.equals("Normal Upd Freq"))
			dataValue = getStringValue(getNormal());
		else if(columnName.equals("Num Mon Clim Fcst"))
			dataValue = getStringValue(getNumMonClim());
		else if(columnName.equals("Post Processor"))
			dataValue = getStringValue(getPostProcessing());
		else if (columnName.equals("Num Day Hydromet Fcst"))
			dataValue = getStringValue(getNumDayHyd());
		else if (columnName.equals("External Date"))
			dataValue = getStringValue(getExternalDate());
		else if(columnName.equals("Upstream Segment"))
			dataValue = getStringValue(getUpstreamSeg());
		else if (columnName.equals("Reservoir Model"))
			dataValue = getStringValue(getReservoirModel());
		else if(columnName.equals("Consumptive Use"))
			dataValue = getStringValue(getConsumptiveUse());
		else if (columnName.equals("Channel Loss"))
			dataValue = getStringValue(getChannelLoss());
		else if (columnName.equals("Num Elev Zones"))
			dataValue = getStringValue(getNumElevZones());
		return dataValue;
	}
		
	public void setFcstPtEnsembleJTableRecord(FcstPtESPRecord fcstPtEnsembleRecord)
	{
		setLid(fcstPtEnsembleRecord.getLid());
		setUpstreamSeg(fcstPtEnsembleRecord.getUpstream_seg());
		setReservoirModel(fcstPtEnsembleRecord.getReservoir_model());
		setConsumptiveUse(fcstPtEnsembleRecord.getConsumptive_use());
		setChannelLoss(fcstPtEnsembleRecord.getChannel_loss());
		setFlowType(fcstPtEnsembleRecord.getFlowtype());
		setFcstType(fcstPtEnsembleRecord.getFcsttype());
		setSnowMethod(fcstPtEnsembleRecord.getSnow_method());
		setPostProcessing(fcstPtEnsembleRecord.getPost_processor());
		Long val = new Long(fcstPtEnsembleRecord.getImpl_date());
		setImplDate(val.toString());
		val = new Long(fcstPtEnsembleRecord.getWeb_date());
		setWebDate(val.toString());
		val = new Long(fcstPtEnsembleRecord.getExternal_date());
		setExternalDate(val.toString());
		Integer int1 = new Integer(fcstPtEnsembleRecord.getFrequpd_flood());
		setFlood(int1.toString());
		int1 = new Integer(fcstPtEnsembleRecord.getFrequpd_drought());
		setNormal(int1.toString());
		int1 = new Integer(fcstPtEnsembleRecord.getFrequpd_normal());
		setDraught(int1.toString());
		setHydraulMethod(fcstPtEnsembleRecord.getHydraul_method());
		setHydrolMethod(fcstPtEnsembleRecord.getHydrol_method());
		int1 = new Integer(fcstPtEnsembleRecord.getFcst_horizon());
		setHorizon(int1.toString());
		int1 = new Integer(fcstPtEnsembleRecord.getNummonclim());
		setNumMonClim(int1.toString());
		int1 = new Integer(fcstPtEnsembleRecord.getNumdayhyd());
		setNumDayHyd(int1.toString());
		int1 = new Integer(fcstPtEnsembleRecord.getNum_elev_zones());
		setNumElevZones(int1.toString());
	}
	
	public int compare(String columnName, JTableRowData rowData)
	{
		int ret = 0;
		FcstPtEnsembleJTableRowData fcstPtEnsembleRecord = (FcstPtEnsembleJTableRowData) rowData;
		if(columnName.equals("Location ID"))
			ret = compareStrings(getLid(), fcstPtEnsembleRecord.getLid());
		else if (columnName.equals("Implementation Date"))
			ret = compareStrings(getImplDate(), fcstPtEnsembleRecord.getImplDate());
		else if (columnName.equals("Web Date"))
			ret = compareStrings(getWebDate(), fcstPtEnsembleRecord.getWebDate());
		else if (columnName.equals("Snow Method"))
			ret = compareStrings(getSnowMethod(), fcstPtEnsembleRecord.getSnowMethod());
		else if(columnName.equals("Hydrologic Method"))
			ret = compareStrings(getHydrolMethod(), fcstPtEnsembleRecord.getHydrolMethod());
		else if (columnName.equals("Routing Method"))
			ret = compareStrings(getHydraulMethod(), fcstPtEnsembleRecord.getHydraulMethod());
		else if (columnName.equals("External Date"))
			ret = compareStrings(getExternalDate(), fcstPtEnsembleRecord.getExternalDate());
		else if(columnName.equals("Flood Upd Freq"))
			ret = compareStrings(getFlood(), fcstPtEnsembleRecord.getFlood());
		else if (columnName.equals("Drought Upd Freq"))
			ret = compareStrings(getDraught(), fcstPtEnsembleRecord.getDraught());
		else if (columnName.equals("Forecast Period"))
			ret = compareStrings(getHorizon(), fcstPtEnsembleRecord.getHorizon());
		else if(columnName.equals("Flow Type"))
			ret = compareStrings(getFlowType(), fcstPtEnsembleRecord.getFlowType());
		else if (columnName.equals("Fcst Type"))
			ret = compareStrings(getFcstType(), fcstPtEnsembleRecord.getFcstType());
		else if (columnName.equals("Normal Upd Freq"))
			ret = compareStrings(getNormal(), fcstPtEnsembleRecord.getNormal());
		else if (columnName.equals("Num Mon Clim Fcst"))
			ret = compareStrings(getNumMonClim(), fcstPtEnsembleRecord.getNumMonClim());
		else if (columnName.equals("Post Processor"))
			ret = compareStrings(getPostProcessing(), fcstPtEnsembleRecord.getPostProcessing());
		else if (columnName.equals("Num Day Hydromet Fcst"))
			ret = compareStrings(getNumDayHyd(), fcstPtEnsembleRecord.getNumDayHyd());
		else if(columnName.equals("Upstream Segment"))
			ret = compareStrings(getUpstreamSeg(), fcstPtEnsembleRecord.getUpstreamSeg());
		else if (columnName.equals("Reservoir Model"))
			ret = compareStrings(getReservoirModel(), fcstPtEnsembleRecord.getReservoirModel());
		else if(columnName.equals("Consumptive Use"))
			ret = compareStrings(getConsumptiveUse(), fcstPtEnsembleRecord.getConsumptiveUse());
		else if (columnName.equals("Channel Loss"))
			ret = compareStrings(getChannelLoss(), fcstPtEnsembleRecord.getChannelLoss());
		else if (columnName.equals("Num Elev Zones"))
			ret = compareStrings(getNumElevZones(), fcstPtEnsembleRecord.getNumElevZones());
		return ret;
	}*/

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

	public String getFlood() {
		return flood;
	}

	public void setFlood(String flood) {
		this.flood = flood;
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

	public String getFlowType() {
		return flowType;
	}

	public void setFlowType(String flowType) {
		this.flowType = flowType;
	}

	public String getFcstType() {
		return fcstType;
	}

	public void setFcstType(String fcstType) {
		this.fcstType = fcstType;
	}

	public String getNormal() {
		return normal;
	}

	public void setNormal(String normal) {
		this.normal = normal;
	}

	public int getNumMonClim() {
		return numMonClim;
	}

	public void setNumMonClim(int numMonClim) {
		this.numMonClim = numMonClim;
	}

	public String getPostProcessing() {
		return postProcessing;
	}

	public void setPostProcessing(String postProcessing) {
		this.postProcessing = postProcessing;
	}

	public int getNumDayHyd() {
		return numDayHyd;
	}

	public void setNumDayHyd(int numDayHyd) {
		this.numDayHyd = numDayHyd;
	}

	public long getExternalDate() {
		return externalDate;
	}

	public void setExternalDate(long externalDate) {
		this.externalDate = externalDate;
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

	public String getVarUsage() {
		return varUsage;
	}

	public void setVarUsage(String varUsage) {
		this.varUsage = varUsage;
	}

	public void addAllCellsToMap()
	{
		addCell(new BaseTableCell("Location ID", CellType.STRING, getLid(), getMissingRepresentation()));
		addCell(new BaseTableCell("Implementation Date", CellType.DATE, getImplDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Web Date", CellType.DATE, getWebDate(), getMissingRepresentation()));
		addCell(new BaseTableCell("Hydrologic Method", CellType.STRING, getHydrolMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Snow Method", CellType.STRING, getSnowMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Routing Method", CellType.STRING, getHydraulMethod(), getMissingRepresentation()));
		addCell(new BaseTableCell("Upstream Segment", CellType.STRING, getUpstreamSeg(), getMissingRepresentation()));
		addCell(new BaseTableCell("Reservoir Model", CellType.STRING, getReservoirModel(), getMissingRepresentation()));
		addCell(new BaseTableCell("Normal Upd Freq", CellType.STRING, getNormal(), getMissingRepresentation()));
		addCell(new BaseTableCell("Flood Upd Freq", CellType.STRING, getFlood(), getMissingRepresentation()));
		addCell(new BaseTableCell("Drought Upd Freq", CellType.STRING, getDraught(), getMissingRepresentation()));		
		addCell(new BaseTableCell("Forecast Period", CellType.STRING, getHorizon(), getMissingRepresentation()));		
		addCell(new BaseTableCell("Consumptive Use", CellType.STRING, getConsumptiveUse(), getMissingRepresentation()));
		addCell(new BaseTableCell("Channel Loss", CellType.STRING, getChannelLoss(), getMissingRepresentation()));
		addCell(new BaseTableCell("Num Elev Zones", CellType.INTEGER, getNumElevZones(), getMissingRepresentation()));
		addCell(new BaseTableCell("Num Mon Clim Fcst", CellType.INTEGER, getNumMonClim(), getMissingRepresentation()));
		addCell(new BaseTableCell("Num Day Hydromet Fcst", CellType.INTEGER, getNumDayHyd(), getMissingRepresentation()));
		addCell(new BaseTableCell("Post Processor", CellType.STRING, getPostProcessing(), getMissingRepresentation()));		
		addCell(new BaseTableCell("External Date", CellType.DATE, getExternalDate(), getMissingRepresentation()));		
		addCell(new BaseTableCell("Flow Type", CellType.STRING, getFlowType(), getMissingRepresentation()));
		addCell(new BaseTableCell("Fcst Type", CellType.STRING, getFcstType(), getMissingRepresentation()));
		addCell(new BaseTableCell("VAR Usage", CellType.STRING, getVarUsage(), getMissingRepresentation()));
	}
	
}
