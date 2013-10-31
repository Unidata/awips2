package ohd.hseb.monitor.river;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.VTECeventRecord;
import ohd.hseb.monitor.MonitorCell;
import ohd.hseb.monitor.ThreatLevel;
import ohd.hseb.monitor.river.RiverMonitorDataManager.SshpFcstDetails;
import ohd.hseb.monitor.river.settings.RiverMonitorMenuSettings;
import ohd.hseb.monitor.derivedcolumns.DerivedColumn;
import ohd.hseb.monitor.precip.PrecipColumns;
import ohd.hseb.monitor.precip.PrecipData;
import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.CellType;

public class RiverMonitorJTableRowData extends AbstractJTableRowData /*implements JTableRowData */
{
    private int _defaultDecimalPoints = 2;

    private static Map _derivedReturnTypeToCellTypeMap = new HashMap();

    private String _groupName;
    private String _lid;
    private String _name;
    private String _county;
    private String _state;
    private String _hsa;
    private String _stream;
    private String  _riverBasin;
    private double _bankFull;
    private double _actionStage;
    private double _floodStage;
    private double _foodFlow;
    private double _actionFlow;
    private String _groupId;
    private double _latestObsValue;
    private long _latestObsTime;
    private double _latestStgValue;
    private long _latestStgTime;
    private double _latestFlowValue;
    private long _latestFlowTime;
    private double _maxFcstValue;
    private long _maxFcstValidTime;
    private long _maxFcstBasisTime;
    private double _sshpMaxFcstValue;
    private long _sshpMaxFcstTime;
    private long _sshpFcstBasisTime;
    private long _sshpFcstFloodTime;
    private int _groupOrdinal;
    private int _locationOrdinal;
    private double _obsFcstMax;
    private double _fldStgDeparture;
    private double _actStgDeparture;
    private double _minorStage;
    private double _majorStage;
    private double _moderateStage;
    private List _derivedColumnsDetailsList;
    private List _vtecEventList;
    private boolean   _alertUpperLimit = false;
    private boolean   _alarmUpperLimit = false;
    private boolean   _alertLowerLimit = false;
    private boolean   _alarmLowerLimit = false;
    private boolean   _alertRocLimit = false;
    private boolean   _alarmRocLimit = false;
    private boolean   _alertDiffLimit = false;
    private boolean   _alarmDiffLimit = false;
    private String _primaryRiverPe;
    private String _obsTypeSource;
    private String _fcstTypeSource;
    protected RiverMonitorDataManager _riverMonitorDataManager;
    protected RiverMonitorVtecEventDataManager _riverMonitorVtecEventDataManager;
    private String _toolTipTextForAlertAlarmSummary;
    private String _toolTipTextForThreatSummary;
    private String _toolTipTextForPrecipThreatSummary;
    private SshpFcstDetails _sshpFcstDetails;

    private PrecipData _precipData;  
    private SessionLogger _logger;
    
    private static Set _ignoreColumnNameForThreatSet = new HashSet();
    
    private RiverMonitorMenuSettings _menuSettings;
 
    static
    {
        _derivedReturnTypeToCellTypeMap.put("boolean", CellType.BOOLEAN);
        _derivedReturnTypeToCellTypeMap.put("double", CellType.DOUBLE);
        _derivedReturnTypeToCellTypeMap.put("float", CellType.FLOAT);
        _derivedReturnTypeToCellTypeMap.put("long", CellType.LONG);
        _derivedReturnTypeToCellTypeMap.put("int", CellType.INTEGER);
        _derivedReturnTypeToCellTypeMap.put("string", CellType.STRING);
        
        _ignoreColumnNameForThreatSet.add(RiverColumns.LAT_FLOW_TIME);
        _ignoreColumnNameForThreatSet.add(RiverColumns.LAT_FLOW_VALUE);
        _ignoreColumnNameForThreatSet.add(RiverColumns.LAT_STG_TIME);
        _ignoreColumnNameForThreatSet.add(RiverColumns.LAT_STG_VALUE);

    }
    
    public RiverMonitorJTableRowData()
    {

    }

    public RiverMonitorJTableRowData(String missingRepresentation, RiverMonitorDataManager riverMonitorDataManager, 
                                    SessionLogger logger, RiverMonitorMenuSettings menuSettings)
    {
        setMissingRepresentation(missingRepresentation);
        _riverMonitorDataManager = riverMonitorDataManager;
        _logger = logger;

        _menuSettings = menuSettings;
    }

    //-----------------------------------------------------------------
    // Copy constructor
    //-----------------------------------------------------------------
    public RiverMonitorJTableRowData(RiverMonitorJTableRowData origRecord)
    {
        setLid(origRecord.getLid());
        setName(origRecord.getName());
        setCounty(origRecord.getCounty());
        setState(origRecord.getState());
        setHsa(origRecord.getHsa());
        setStream(origRecord.getStream());
        setRiverBasin(origRecord.getRiverBasin());
        setBankFull(origRecord.getBankFull());
        setActionStage(origRecord.getActionStage());
        setFloodStage(origRecord.getFloodStage());
        setFloodFlow(origRecord.getFloodFlow());
        setActionFlow(origRecord.getActionFlow());
        setGroupId(origRecord.getGroupId());
        setGroupName(origRecord.getGroupName());
    }

    public String getLid()
    {
        return _lid;
    }

    public void setLid(String lid)
    {
        this._lid = lid ;
    }

    public String getName()
    {
        return _name;
    }

    public void setName(String name)
    {
        this._name = name ;
    }

    public String getCounty()
    {
        return _county;
    }

    public void setCounty(String county)
    {
        this._county = county ;
    }

    public String getState()
    {
        return _state;
    }

    public void setState(String state)
    {
        this._state = state ;
    }

    public String getHsa()
    {
        return _hsa;
    }

    public void setHsa(String hsa)
    {
        this._hsa = hsa ;
    }

    public String getStream()
    {
        return _stream;
    }

    public void setStream(String stream)
    {
        this._stream = stream ;
    }

    public double getBankFull()
    {
        return _bankFull;
    }

    public void setBankFull(double bankFull)
    {
        this._bankFull = bankFull ;
    }

    public double getActionStage()
    {
        return _actionStage;
    }

    public void setActionStage(double actionStage)
    {
        this._actionStage = actionStage ;
    }

    public double getFloodStage()
    {
        return _floodStage;
    }

    public void setFloodStage(double floodStage)
    {
        this._floodStage = floodStage ;
    }

    public double getFloodFlow()
    {
        return _foodFlow;
    }

    public void setFloodFlow(double floodFlow)
    {
        this._foodFlow = floodFlow ;
    }

    public double getActionFlow()
    {
        return _actionFlow;
    }

    public void setActionFlow(double actionFlow)
    {
        this._actionFlow = actionFlow ;
    }

    public String getGroupName()
    {
        return _groupName;
    }

    public void setGroupName(String groupName)
    {
        this._groupName = groupName ;
    }

    public String getGroupId()
    {
        return _groupId;
    }

    public void setGroupId(String groupId)
    {
        this._groupId = groupId ;
    }

    public double getLatestObsValue()
    {
        return _latestObsValue;
    }

    public void setLatestObsValue(double latestObsValue)
    {
        this._latestObsValue = latestObsValue ;
    }

    public long getLatestObsTime()
    {
        return _latestObsTime;
    }

    public void setLatestObsTime(long latestObsTime)
    {
        this._latestObsTime = latestObsTime ;
    }

    public double getLatestStgValue()
    {
        return _latestStgValue;
    }

    public void setLatestStgValue(double latestStgValue)
    {
        this._latestStgValue = latestStgValue ;
    }

    public long getLatestStgTime()
    {
        return _latestStgTime;
    }

    public void setLatestStgTime(long latestStgTime)
    {
        this._latestStgTime = latestStgTime ;
    }

    public double getLatestFlowValue()
    {
        return _latestFlowValue;
    }

    public void setLatestFlowValue(double latestFlowValue)
    {
        this._latestFlowValue = latestFlowValue ;
    }

    public long getLatestFlowTime()
    {
        return _latestFlowTime;
    }

    public void setLatestFlowTime(long latestFlowTime)
    {
        this._latestFlowTime = latestFlowTime ;
    }

    public double getMaxFcstValue()
    {
        return _maxFcstValue;
    }

    public void setMaxFcstValue(double maxFcstValue)
    {
        this._maxFcstValue = maxFcstValue ;
    }

    public long getMaxFcstValidTime()
    {
        return _maxFcstValidTime;
    }

    public void setMaxFcstValidTime(long maxFcstValidTime)
    {
        this._maxFcstValidTime = maxFcstValidTime;
    }

    public long getMaxFcstBasisTime()
    {
        return _maxFcstBasisTime;
    }

    public void setMaxFcstBasisTime(long maxFcstBasisTime)
    {
        this._maxFcstBasisTime = maxFcstBasisTime;
    }

    public long getSshpFcstBasisTime() 
    {
        if(_sshpFcstDetails != null)
            _sshpFcstBasisTime = _sshpFcstDetails.getBasisTime();
        else
            _sshpFcstBasisTime = DbTable.getNullLong();
        return _sshpFcstBasisTime;
    }

    public void setSshpFcstBasisTime(long sshpFcstBasisTime) 
    {
        this._sshpFcstBasisTime = sshpFcstBasisTime;
    }

    public long getSshpFcstFloodTime() 
    {
        if(_sshpFcstDetails != null)
            _sshpFcstFloodTime = _sshpFcstDetails.getTimeAboveFldStg();
        else
            _sshpFcstFloodTime = DbTable.getNullLong();
        return _sshpFcstFloodTime;
    }

    public void setSshpFcstFloodTime(long sshpFcstFloodTime) 
    {
        this._sshpFcstFloodTime = sshpFcstFloodTime;
    }

    public long getSshpMaxFcstValidTime() 
    {
        if(_sshpFcstDetails != null)
            _sshpMaxFcstTime = _sshpFcstDetails.getCrestTime();
        else
            _sshpMaxFcstTime =  DbTable.getNullLong();
        return _sshpMaxFcstTime;
    }

    public void setSshpMaxFcstValidTime(long sshpMaxFcstTime) 
    {
        this._sshpMaxFcstTime = sshpMaxFcstTime;
    }

    public double getSshpMaxFcstValue() 
    {
        if(_sshpFcstDetails != null)
            _sshpMaxFcstValue = _sshpFcstDetails.getCrestValue();
        else
            _sshpMaxFcstValue = DbTable.getNullDouble();
        return _sshpMaxFcstValue;
    }

    public void setSshpMaxFcstValue(double sshpMaxFcstValue) 
    {
        this._sshpMaxFcstValue = sshpMaxFcstValue;
    }

    public int getGroupOrdinal()
    {
        return _groupOrdinal;
    }

    public void setGroupOrdinal(int groupOrdinal)
    {
        this._groupOrdinal = groupOrdinal ;
    }

    public int getLocationOrdinal()
    {
        return _locationOrdinal;
    }

    public void setLocationOrdinal(int locationOrdinal)
    {
        this._locationOrdinal = locationOrdinal ;
    }

    public void setObsFcstMax(double obsFcstMax)
    {
        this._obsFcstMax = obsFcstMax;
    }

    public double getObsFcstMax()
    {
        return _obsFcstMax;
    }

    public void setFldStgDeparture(double fldStgDeparture)
    {
        this._fldStgDeparture = fldStgDeparture;
    }

    public double getFldStgDeparture()
    {
        return _fldStgDeparture;
    }

    public void setActStgDeparture(double actStgDeparture)
    {
        this._actStgDeparture = actStgDeparture;
    }

    public double getActStgDeparture()
    {
        return _actStgDeparture;
    }

    public void setMinorStage(double minorStage)
    {
        this._minorStage = minorStage;
    }

    public double getMinorStage()
    {
        return _minorStage;
    }

    public void setMajorStage(double majorStage)
    {
        this._majorStage = majorStage;
    }

    public double getMajorStage()
    {
        return _majorStage;
    }

    public void setModerateStage(double moderateStage)
    {
        this._moderateStage = moderateStage;
    }

    public double getModerateStage()
    {
        return _moderateStage;
    }

    public void setDerivedColumnsDetailsList(List list)
    {
        _derivedColumnsDetailsList = list; 
    }

    public List getDerivedColumnsDetailsList()
    {
        return _derivedColumnsDetailsList;
    }

    private List getVtecEventList()
    {
        return _vtecEventList;
    }

    public void setVtecEventList(List list)
    {
        _vtecEventList = list; 
        _riverMonitorVtecEventDataManager = new RiverMonitorVtecEventDataManager(_vtecEventList);
    }

    public RiverMonitorVtecEventDataManager getVtecEventDataManager()
    {
        return _riverMonitorVtecEventDataManager;
    }

    public boolean getAlertUpperLimit()
    {
        return _alertUpperLimit;
    }

    public void setAlertUpperLimit(boolean alertUpperLimit)
    {
        _alertUpperLimit = alertUpperLimit;
    }

    public boolean getAlarmUpperLimit()
    {
        return _alarmUpperLimit;
    }

    public void setAlarmUpperLimit(boolean alarmUpperLimit)
    {
        _alarmUpperLimit = alarmUpperLimit;
    }

    public boolean getAlertLowerLimit()
    {
        return _alertLowerLimit;
    }

    public void setAlertLowerLimit(boolean alertLowerLimit)
    {
        _alertLowerLimit = alertLowerLimit;
    }

    public boolean getAlarmLowerLimit()
    {
        return _alarmLowerLimit;
    }

    public void setAlarmLowerLimit(boolean alarmLowerLimit)
    {
        _alarmLowerLimit = alarmLowerLimit;
    }

    public boolean getAlertRocLimit()
    {
        return _alertRocLimit;
    }

    public void setAlertRocLimit(boolean alertRocLimit)
    {
        _alertRocLimit = alertRocLimit;
    }

    public boolean getAlarmRocLimit()
    {
        return _alarmRocLimit;
    }

    public void setAlarmRocLimit(boolean alarmRocLimit)
    {
        _alarmRocLimit = alarmRocLimit;
    }

    public boolean getAlertDiffLimit()
    {
        return _alertDiffLimit;
    }

    public void setAlertDiffLimit(boolean alertDiffLimit)
    {
        _alertDiffLimit = alertDiffLimit;
    }

    public boolean getAlarmDiffLimit()
    {
        return _alarmDiffLimit;
    }

    public void setAlarmDiffLimit(boolean alarmDiffLimit)
    {
        _alarmDiffLimit = alarmDiffLimit;
    }

    public String getPrimaryRiverPe()
    {
        return _primaryRiverPe;
    }

    public void setPrimaryRiverPe(String pe)
    {
        _primaryRiverPe = pe;
    }

    public PrecipData getPrecipData()
    {
        return _precipData;
    }

    public void setPrecipData(PrecipData precipData)
    {
        _precipData = precipData;
    }

    public String getToolTipTextForAlertAlarmSummary()
    {
        return _toolTipTextForAlertAlarmSummary;
    }

    public void setToolTipTextForAlertAlarmSummary(String toolTipText)
    {
        _toolTipTextForAlertAlarmSummary = toolTipText;
    }

    public String getObsTypeSource() 
    {
        return _obsTypeSource;
    }

    public void setObsTypeSource(String obsTypeSource) 
    {
        _obsTypeSource = obsTypeSource;
    }

    public String getFcstTypeSource() 
    {
        return _fcstTypeSource;
    }

    public void setFcstTypeSource(String fcstTypeSource) 
    {
        _fcstTypeSource = fcstTypeSource;
    }

    public VTECeventRecord getVtecEventRecordForEndTime()
    {
        return _riverMonitorVtecEventDataManager.getVtecEventRecordForEndTime();    
    }

    public VTECeventRecord getVtecEventRecordForExpireTime()
    {
        return _riverMonitorVtecEventDataManager.getVtecEventRecordForExpireTime();    
    }

    public SshpFcstDetails getSshpFcstDetails() 
    {
        return _sshpFcstDetails;
    }

    public void setSshpFcstDetails(SshpFcstDetails sshpFcstDetails) 
    {
        this._sshpFcstDetails = sshpFcstDetails;
    }

    public String getStringValue(long value, long missingValue)
    {
        String result = super.getStringValue(value, missingValue);
        if(result != super.getMissingRepresentation())
        {
            String tempStr = result.substring(5, result.length()-3);
            tempStr = tempStr.replace('-','/');
            tempStr = tempStr.replace(' ', '-');
            result = tempStr;
        }
        return result;
    }

    // If primary pe is null assume it to be stage.
    protected boolean isPrimaryPeStage()
    {
        boolean result = true;
        if(_primaryRiverPe != null)
        {
            if(_primaryRiverPe.length() != 2)
            {
                System.out.println("Primarype:["+ _primaryRiverPe+"]"+ "lid:"+getLid()+ "len:"+ _primaryRiverPe.length());
            }
            if(_primaryRiverPe.charAt(0) != 'H')
                result = false;
        }
        return result;
    }

    public String getVtecSummary()
    {
        return (_riverMonitorVtecEventDataManager.getVtecSummary());    
    }

    public long getEventEndTime()
    {
        int numOfHrsForVtecEventProductTimeLookBack = _menuSettings.getRiverSettings().getVtecEventProductTimeLookBack();
        long earliestAcceptableTimeForVtecEventProductTimeLookBack = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsForVtecEventProductTimeLookBack;
        
        return (_riverMonitorVtecEventDataManager.getEventEndTime(earliestAcceptableTimeForVtecEventProductTimeLookBack));    
    }

    public long getUGCExpireTime()
    {
        int numOfHrsForVtecEventProductTimeLookBack = _menuSettings.getRiverSettings().getVtecEventProductTimeLookBack();
        long earliestAcceptableTimeForVtecEventProductTimeLookBack = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsForVtecEventProductTimeLookBack;
        
        return (_riverMonitorVtecEventDataManager.getUGCExpireTime(earliestAcceptableTimeForVtecEventProductTimeLookBack));    
    }

    public String getAlertAlarmSummary()
    {
        String summary = "";
        StringBuffer summaryStringBufffer = new StringBuffer("");
        StringBuffer toolTipStringBuffer = new StringBuffer("");
        if(_alertUpperLimit || _alarmUpperLimit || _alertLowerLimit || _alarmLowerLimit|| _alertRocLimit ||
                _alarmRocLimit || _alertDiffLimit || _alarmDiffLimit)
        {
            toolTipStringBuffer.append("<HTML><BODY>");
        }
        _toolTipTextForAlertAlarmSummary = "";
        if(_alertUpperLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("u");
            toolTipStringBuffer.append("u: Value Exceeded Alert Upper Limit\n<BR>");
        }
        else if(_alarmUpperLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("U");
            toolTipStringBuffer.append("U: Value Exceeded Alarm Upper Limit\n<BR>");
        }
        if(_alertLowerLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("l");
            toolTipStringBuffer.append("l: Value Exceeded Alert Lower Limit\n<BR>");
        }
        else if(_alarmLowerLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("L");
            toolTipStringBuffer.append("L: Value Exceeded Alarm Lower Limit\n<BR>");
        }
        if(_alertRocLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("c");
            toolTipStringBuffer.append("c: Value Exceeded Alert ROC Limit<BR>");
        }
        else if(_alarmRocLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("C");
            toolTipStringBuffer.append("C: Value Exceeded Alarm ROC Limit<BR>");
        }
        if(_alertDiffLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("d");
            toolTipStringBuffer.append("d: Value Exceeded Alert Diff Limit\n<BR>");
        }
        else if(_alarmDiffLimit)
        {
            summaryStringBufffer = summaryStringBufffer.append("D");
            toolTipStringBuffer.append("D: Value Exceeded Alarm Diff Limit\n<BR>");
        }

        summary = summaryStringBufffer.toString();
        _toolTipTextForAlertAlarmSummary = toolTipStringBuffer.toString();
        return summary;
    }

    public String getToolTipTextForThreatSummary()
    {
        return _toolTipTextForThreatSummary;
    }

    public void setToolTipTextForThreatSummary(String toolTipText)
    {
        _toolTipTextForThreatSummary = toolTipText;
    }

    public String getToolTipTextForPrecipThreatSummary()
    {
        return _toolTipTextForPrecipThreatSummary;
    }

    public void setToolTipTextForPrecipThreatSummary(String toolTipText)
    {
        _toolTipTextForPrecipThreatSummary = toolTipText;
    }


    public void addAllCellsToMap()
    {
        ThreatLevel defaultThreatLevel = ThreatLevel.NO_THREAT;
        MonitorCell cell = null;
        String header = "RiverMonitorJTableRowData2.addAllCellsToMap(): ";

        String dateTimeFormatString = "MM/dd - HH:mm";

        //static data

        addToCellMap(RiverColumns.PRIMARY_RIVER_PE, CellType.STRING, getPrimaryRiverPe() );
        
        addToCellMap(RiverColumns.GROUP_NAME, CellType.STRING, getGroupName() );  
        addToCellMap(RiverColumns.GROUP_ID, CellType.STRING, getGroupId() );
        addToCellMap(RiverColumns.GROUP_ORDINAL, CellType.INTEGER, getGroupOrdinal());

        addToCellMap(RiverColumns.LOCATION_ID, CellType.STRING, getLid() );
        addToCellMap(RiverColumns.LOCATION_NAME, CellType.STRING, getName() );
        addToCellMap(RiverColumns.LOCATION_ORDINAL, CellType.INTEGER, getLocationOrdinal());

        addToCellMap(RiverColumns.COUNTY, CellType.STRING, getCounty() );

        addToCellMap(RiverColumns.STATE, CellType.STRING, getState() );
        addToCellMap(RiverColumns.STREAM, CellType.STRING, getStream() );
        addToCellMap(RiverColumns.RIVER_BASIN, CellType.STRING, getRiverBasin() );
        addToCellMap(RiverColumns.HSA,    CellType.STRING, getHsa() );
        addToCellMap(RiverColumns.BANK_FULL, CellType.DOUBLE, getBankFull() );
        addToCellMap(RiverColumns.FLD_STG, CellType.DOUBLE, getFloodStage() );      
        addToCellMap(RiverColumns.ACT_STG, CellType.DOUBLE, getActionStage() );

        addToCellMap(RiverColumns.FLD_FLOW, CellType.DOUBLE, getFloodFlow(), defaultThreatLevel, getMissingRepresentation(), 0);    
        addToCellMap(RiverColumns.ACT_FLOW, CellType.DOUBLE, getActionFlow(), defaultThreatLevel, getMissingRepresentation(), 0);

        addToCellMap(RiverColumns.MIN_STG, CellType.DOUBLE, getMinorStage());
        addToCellMap(RiverColumns.MAJ_STG, CellType.DOUBLE, getMajorStage());
        addToCellMap(RiverColumns.MOD_STG, CellType.DOUBLE, getModerateStage());

        //dynamic data
        int numOfHrsForObsLookBack = _menuSettings.getRiverSettings().getLatestObsLookBack();
        long earliestAcceptableTimeForObs = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsForObsLookBack;

        // latest obs value and time
        ThreatLevel latestObsValueThreatLevel = getThreatLevelForLatestObsValue(earliestAcceptableTimeForObs);
        addToCellMap(RiverColumns.LAT_OBS_VALUE, CellType.DOUBLE, 
            getLatestObsValue(), latestObsValueThreatLevel,
            getMissingRepresentation()); 

        ThreatLevel latestObsTimeThreatLevel = getThreatLevelForThisTime(getLatestObsTime(), earliestAcceptableTimeForObs);
        addToCellMap(RiverColumns.LAT_OBS_TIME, CellType.DATE_TIME,
            getLatestObsTime(), latestObsTimeThreatLevel,
            getMissingRepresentation(), dateTimeFormatString );


        // latest stage value and time
        ThreatLevel latestStageValueThreatLevel = getThreatLevelForStage(getLatestStgValue(),getLatestStgTime(), earliestAcceptableTimeForObs);
        addToCellMap(RiverColumns.LAT_STG_VALUE, CellType.DOUBLE, getLatestStgValue(), latestStageValueThreatLevel, getMissingRepresentation(), 2);

        ThreatLevel latestStageTimeThreatLevel = getThreatLevelForThisTime(getLatestStgTime(), earliestAcceptableTimeForObs);
        addToCellMap(RiverColumns.LAT_STG_TIME, CellType.DATE_TIME, getLatestStgTime(), latestStageTimeThreatLevel, getMissingRepresentation(), dateTimeFormatString);


        // latest flow value and time
        ThreatLevel latestFlowValueThreatLevel = getThreatLevelForFlow(getLatestFlowValue(), getLatestFlowTime(), earliestAcceptableTimeForObs  );
        addToCellMap(RiverColumns.LAT_FLOW_VALUE, CellType.DOUBLE, getLatestFlowValue(), latestFlowValueThreatLevel);

        ThreatLevel latestFlowTimeThreatLevel = getThreatLevelForThisTime(getLatestFlowTime(), earliestAcceptableTimeForObs);
        addToCellMap(RiverColumns.LAT_FLOW_TIME, CellType.DATE_TIME, getLatestFlowTime(), latestFlowTimeThreatLevel, getMissingRepresentation(), dateTimeFormatString);


        int numOfHrsForFcstLookBack = _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack();
        long earliestAcceptableTimeForFcst = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsForFcstLookBack;

        //max forecast
        ThreatLevel maxForecastThreatLevel = getThreatLevelForMaxFcstValue(earliestAcceptableTimeForFcst);
        addToCellMap(RiverColumns.MAX_FCST_VALUE, CellType.DOUBLE, getMaxFcstValue(), maxForecastThreatLevel);

        addToCellMap(RiverColumns.MAX_FCST_VALID_TIME, CellType.DATE_TIME, getMaxFcstValidTime(), defaultThreatLevel, getMissingRepresentation(), dateTimeFormatString);
        addToCellMap(RiverColumns.MAX_FCST_BASIS_TIME, CellType.DATE_TIME, getMaxFcstBasisTime(), defaultThreatLevel, getMissingRepresentation(), dateTimeFormatString);

        //sshp forecast
        addToCellMap(RiverColumns.SSHP_MAX_FCST_VALID_TIME, CellType.DATE_TIME, getSshpMaxFcstValidTime(), defaultThreatLevel, getMissingRepresentation(), dateTimeFormatString);
        addToCellMap(RiverColumns.SSHP_FCST_BASIS_TIME, CellType.DATE_TIME, getSshpFcstBasisTime(), defaultThreatLevel, getMissingRepresentation(), dateTimeFormatString);

        ThreatLevel maxSshpForecastThreatLevel = getThreatLevelForSshpFcstValue(earliestAcceptableTimeForFcst);
        addToCellMap(RiverColumns.SSHP_MAX_FCST_VALUE, CellType.DOUBLE, getSshpMaxFcstValue(), maxSshpForecastThreatLevel);

        ThreatLevel maxSshpFcstFloodTimeThreatLevel = getThreatLevelForSshpTimeAboveFloodLevel(earliestAcceptableTimeForFcst);
        addToCellMap(RiverColumns.SSHP_FCST_FLD_TIME, CellType.DATE_TIME, getSshpFcstFloodTime(), maxSshpFcstFloodTimeThreatLevel, getMissingRepresentation(), dateTimeFormatString);

        ThreatLevel obsFcstMaxThreatLevel = getThreatLevelForObsFcstMax();
        addToCellMap(RiverColumns.OBSFCST_MAX, CellType.DOUBLE, getObsFcstMax(), obsFcstMaxThreatLevel, getMissingRepresentation(), 2);

        //stg departures
        addToCellMap(RiverColumns.FLD_STG_DEP, CellType.DOUBLE, getFldStgDeparture());
        addToCellMap(RiverColumns.ACT_STG_DEP, CellType.DOUBLE, getActStgDeparture());

        addToCellMap(PrecipColumns.LATEST_PRECIP_PARAM_CODE,  CellType.STRING, getPrecipData().getLatestPrecipParamCodeString() );
           
        addToCellMap(PrecipColumns.LATEST_30MIN, CellType.DOUBLE, getPrecipData().getLatestPrecip30Min());
        addToCellMap(PrecipColumns.LATEST_1HR, CellType.DOUBLE, getPrecipData().getLatestPrecip1Hr(), getThreatLevelForPrecipColumn("INSTANT", 1));
        addToCellMap(PrecipColumns.LATEST_3HR, CellType.DOUBLE, getPrecipData().getLatestPrecip3Hr(), getThreatLevelForPrecipColumn("INSTANT", 3));
        addToCellMap(PrecipColumns.LATEST_6HR, CellType.DOUBLE, getPrecipData().getLatestPrecip6Hr(), getThreatLevelForPrecipColumn("INSTANT", 6));
        
        addToCellMap(PrecipColumns.LATEST_12HR, CellType.DOUBLE, getPrecipData().getLatestPrecip12Hr());
        addToCellMap(PrecipColumns.LATEST_18HR, CellType.DOUBLE, getPrecipData().getLatestPrecip18Hr());
        addToCellMap(PrecipColumns.LATEST_24HR, CellType.DOUBLE, getPrecipData().getLatestPrecip24Hr());
        
        addToCellMap(PrecipColumns.TOH_PRECIP_1HR_PARAM_CODE,  CellType.STRING, getPrecipData().getTohPrecip1HrParamCodeString() );
   
        addToCellMap(PrecipColumns.TOH_PRECIP_1HR, CellType.DOUBLE, getPrecipData().getTohPrecip1Hr(), getThreatLevelForPrecipColumn("PRECIP", 1));
          
        addToCellMap(PrecipColumns.FFG_1HR, CellType.DOUBLE, getPrecipData().getFFG1Hr());
        addToCellMap(PrecipColumns.FFG_3HR, CellType.DOUBLE, getPrecipData().getFFG3Hr());
        addToCellMap(PrecipColumns.FFG_6HR, CellType.DOUBLE, getPrecipData().getFFG6Hr());

        addToCellMap(PrecipColumns.DIFF_1HR, CellType.DOUBLE, getPrecipData().getDiff1Hr(), getThreatLevelForPrecipColumn("DIFF", 1));
        addToCellMap(PrecipColumns.DIFF_3HR, CellType.DOUBLE, getPrecipData().getDiff3Hr(), getThreatLevelForPrecipColumn("DIFF", 3));
        addToCellMap(PrecipColumns.DIFF_6HR, CellType.DOUBLE, getPrecipData().getDiff6Hr(), getThreatLevelForPrecipColumn("DIFF", 6));

        addToCellMap(PrecipColumns.RATIO_1HR, CellType.INTEGER, getPrecipData().getRatio1Hr(), getThreatLevelForPrecipColumn("RATIO", 1));
        addToCellMap(PrecipColumns.RATIO_3HR, CellType.INTEGER, getPrecipData().getRatio3Hr(), getThreatLevelForPrecipColumn("RATIO", 3));
        addToCellMap(PrecipColumns.RATIO_6HR, CellType.INTEGER, getPrecipData().getRatio6Hr(), getThreatLevelForPrecipColumn("RATIO", 6));

        addToCellMap(RiverColumns.ALERT_ALARM, CellType.STRING, getAlertAlarmSummary(), getThreatLevelForAlertAlarmSummary());

        int numOfHrsForVtecEventEndTimeLookAhead = 1;
        numOfHrsForVtecEventEndTimeLookAhead = _menuSettings.getRiverSettings().getVtecEventEndTimeApproach();
        ThreatLevel vtecEventTimeThreatLevel = getThreatLevelForVtecTimeCell(getEventEndTime(), numOfHrsForVtecEventEndTimeLookAhead);

        addToCellMap(RiverColumns.VTEC_SUMMARY, CellType.STRING, getVtecSummary(),vtecEventTimeThreatLevel) ;

        // use a different missing representation
        String vtecTimeMissingRepresentation = getEventEndTimeOrUgcExpireTimeMissingRepresentation(getMissingRepresentation());
        addToCellMap(RiverColumns.EVENT_END_TIME, CellType.DATE_TIME, getEventEndTime(), vtecEventTimeThreatLevel, vtecTimeMissingRepresentation, dateTimeFormatString );

        int numOfHrsForExpireTimeLookAhead = 1;
        numOfHrsForExpireTimeLookAhead = _menuSettings.getRiverSettings().getUgcExpireTimeApproach();
        ThreatLevel vtecUgcExpireTimeThreatLevel = getThreatLevelForVtecTimeCell(getUGCExpireTime(), numOfHrsForExpireTimeLookAhead);

        addToCellMap(RiverColumns.UGC_EXPIRE_TIME, CellType.DATE_TIME, getUGCExpireTime(), vtecUgcExpireTimeThreatLevel, vtecTimeMissingRepresentation, dateTimeFormatString);

        ThreatLevel threatLevel = getThreatLevelForPrecipThreatSummary();    
        cell = addToCellMap(PrecipColumns.PRECIP_THREAT, CellType.EXTENSION,
            getPrecipThreatSummary(), threatLevel,
            getMissingRepresentation());
        
        threatLevel = getThreatLevelForThreatSummary();    
        cell = addToCellMap(RiverColumns.THREAT, CellType.EXTENSION,
            getThreatSummary(), threatLevel,
            getMissingRepresentation());


        //  addCell(cell);

    } //end addAllCellsToMap

    // -----------------------------------------------------------------------------------
    public double round(double value, int decimalPlacesToMaintain)
    {
        return MathHelper.roundToNDecimalPlaces(value, decimalPlacesToMaintain);
    }
    
    // -----------------------------------------------------------------------------------

    public String getEventEndTimeOrUgcExpireTimeMissingRepresentation(String standardMissingRepresentation)
    {
        String missingRepresentationString = standardMissingRepresentation;

        if(_riverMonitorVtecEventDataManager.thereAreAnyActiveEventsForThisLocation())
        {
            missingRepresentationString = "TBD";
        }

        return missingRepresentationString;
    }
    
 // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value,  ThreatLevel threatLevel,Color cellBackgroundColor,
            String missingRepresentation,
            int decimalPointsForDisplay)
    {
        MonitorCell cell = new MonitorCell (columnName,  cellType,
            value, threatLevel, cellBackgroundColor,
            missingRepresentation, decimalPointsForDisplay);
        addCell(cell);

        return cell;
    }

 // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, Color cellBackgroundColor)
    {

        MonitorCell cell =  addToCellMap(columnName,  cellType,
            value, ThreatLevel.NO_THREAT, cellBackgroundColor, getMissingRepresentation(), _defaultDecimalPoints);   

        return cell;
    }
    
 // -----------------------------------------------------------------------------------
    
    protected void addDerivedDataToCellMap()
    {
        //   add all derived columns as well        
        List derivedColumnsDetailsList = this.getDerivedColumnsDetailsList();
        for(int i=0 ; i < derivedColumnsDetailsList.size(); i++)
        {
            DerivedColumn desc1 = (DerivedColumn) derivedColumnsDetailsList.get(i);

            CellType cellType = getCellTypeFromReturnType(desc1.getReturnType());

            addToCellMap(desc1.getColumnName(), cellType, desc1.getValue(), desc1.getCellBackgroundColor());       
        }
    }
    // -----------------------------------------------------------------------------------

    public Color getCellBackgroundColorForThreatSummary()
    {  
        return getCellBackgroundColor("Threat");
    }

    private CellType getCellTypeFromReturnType(String derivedColumnReturnType)
    {
        CellType cellType = null;

        cellType = (CellType) _derivedReturnTypeToCellTypeMap.get(derivedColumnReturnType.toLowerCase());

        return cellType;
    }

    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType,
            Object value)
    {
        MonitorCell cell = addToCellMap(columnName,  cellType, 
            value,  ThreatLevel.NO_THREAT, getMissingRepresentation());       

        return cell;
    }
    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel)
    {

        MonitorCell cell =  addToCellMap(columnName,  cellType,
            value, threatLevel, getMissingRepresentation(), _defaultDecimalPoints);   

        return cell;
    }
//  -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel,
            String missingRepresentation)
    {

        MonitorCell cell =  addToCellMap(columnName,  cellType,
            value, threatLevel, missingRepresentation, _defaultDecimalPoints);   

        return cell;
    }
    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value,  ThreatLevel threatLevel,
            String missingRepresentation,
            int decimalPointsForDisplay)
    {
        MonitorCell cell = new MonitorCell (columnName,  cellType,
            value, threatLevel,
            missingRepresentation, decimalPointsForDisplay);
        addCell(cell);

        return cell;
    }

    // -----------------------------------------------------------------------------------

    private MonitorCell addToCellMap(String columnName, CellType cellType, 
            Object value, ThreatLevel threatLevel,
            String missingRepresentation,
            String dateFormatString)
    {        

        MonitorCell cell = new MonitorCell (columnName,  cellType, value, threatLevel,
            missingRepresentation, dateFormatString);


        // addToCellMap(cell);
        addCell(cell);


        return cell;
    }


    // -----------------------------------------------------------------------------------

    public Color getCellBackgroundColor(String columnName)
    {
        Color color = Color.white;

        MonitorCell cell = (MonitorCell) getCell(columnName);

        if (cell != null)
        {   
            color = cell.getCellBackgroundColor();
        }

        return color;
    }

    // -----------------------------------------------------------------------------------

    public Color getCellForegroundColor(String columnName)
    {
        Color color = Color.black;

        MonitorCell cell = (MonitorCell) getCell(columnName);

        if (cell != null)
        {
            color = cell.getCellForegroundColor();
        }
        return color;
    }


    // helper methods

    // -----------------------------------------------------------------------------------
    public Color getCriticalColorForThisRow()
    {
        Color color = Color.WHITE;
        ThreatLevel level = getRiverMonitorThreatLevel();
        if(level == ThreatLevel.ALERT)
        {
            color = Color.RED;
        }
        else if(level == ThreatLevel.CAUTION)
        {
            color = Color.YELLOW;
        }
        else if(level == ThreatLevel.AGED_DATA || 
                level == ThreatLevel.MISSING_DATA)
        {
            color = Color.GRAY;
        }
        else 
        {
            boolean flowLevelIsPresent = false;
            boolean stageLevelIsPresent = false;

            if( !  DbTable.isNull(getFloodFlow()) && ! DbTable.isNull(getActionFlow()) )
            {
                flowLevelIsPresent = true;
            }
            if( ! DbTable.isNull(getFloodStage()) && ! DbTable.isNull(getActionStage()) )
            {
                stageLevelIsPresent = true;
            }
            if(flowLevelIsPresent || stageLevelIsPresent)
            {
                color = new Color(0, 153, 0);        // Dark green
            }
        }

        return color;    
    }

    //  -----------------------------------------------------------------------------------
    public ThreatLevel getThreatLevelForThreatSummary()
    {
        ThreatLevel level = getRiverMonitorThreatLevel();

        //Threat cell should be color red/yellow/white. So skip threat aged / missing threat level 
        if(level == ThreatLevel.AGED_DATA ||
                level == ThreatLevel.MISSING_DATA)
        {
            level = ThreatLevel.NO_THREAT;
        }

        return level;
    }
    
     //  -----------------------------------------------------------------------------------
    public ThreatLevel getThreatLevelForPrecipThreatSummary()
    {
        ThreatLevel level = getPrecipThreatLevelForPrecipThreatSummary();

        //Threat cell should be color red/yellow/white. So skip threat aged / missing threat level 
        if(level == ThreatLevel.AGED_DATA ||
                level == ThreatLevel.MISSING_DATA)
        {
            level = ThreatLevel.NO_THREAT;
        }

        return level;
    }

    // -----------------------------------------------------------------------------------

    public ThreatLevel getRiverMonitorThreatLevel()
    {
        ThreatLevel cellThreatLevel = ThreatLevel.NO_THREAT;

        List cellList = new ArrayList( getCellMap().values());
        cellThreatLevel = getMaxRowThreatLevel(cellList);

        return cellThreatLevel;   
    }
//  -----------------------------------------------------------------------------------
    public ThreatLevel getMaxRowThreatLevel(List cellList)
    {
        String header = "RiverMonitorJTableRowData.getMaxRowThreatLevel(): ";
        //this method assumes that the threat column has not yet been added to the cell map, when this
        // method is invoked
        ThreatLevel level = ThreatLevel.NO_THREAT;
        ThreatLevel maxLevel = ThreatLevel.ALERT;

        //make sure the threat cell's threat level does not get used to calculate row's threat level, so clear it to zero
        ThreatLevel cellThreatLevel = ThreatLevel.NO_THREAT;

        //iterate through each cell, and set the level to the max threat level found
        for (int i = 0; i < cellList.size(); i++)
        {

            MonitorCell cell = (MonitorCell) cellList.get(i);

            if (cell == null)
            {
                continue;
            }
            else
            {
                cellThreatLevel = cell.getThreatLevel();
                
                
                if ( _ignoreColumnNameForThreatSet.contains(cell.getColumnName()) )
                {
                    //ignore this column
                }

                else if (cellThreatLevel.isGreater(level))
                {
                    //System.out.println(header + "Cell " + cell.getColumnName() + " cellThreatLevel = " + cellThreatLevel);
                    level = cellThreatLevel;
                    if (level == maxLevel)
                    {
                        break; //quit looking, because we have determined it is maxed out
                    }
                }
            }
        }
        return level;
    }
    
    // -----------------------------------------------------------------------------------
    
    public List getPrecipCells()
    {
        List cellList = new ArrayList();

        addCell(cellList, PrecipColumns.LATEST_30MIN);
        addCell(cellList, PrecipColumns.LATEST_1HR);
        addCell(cellList, PrecipColumns.LATEST_3HR );
        addCell(cellList, PrecipColumns.LATEST_6HR );
        
        addCell(cellList, PrecipColumns.TOH_PRECIP_1HR );
        
        addCell(cellList, PrecipColumns.DIFF_1HR );
        addCell(cellList, PrecipColumns.DIFF_3HR );
        addCell(cellList, PrecipColumns.DIFF_6HR );
        
        addCell(cellList, PrecipColumns.RATIO_1HR );
        addCell(cellList, PrecipColumns.RATIO_3HR );
        addCell(cellList, PrecipColumns.RATIO_6HR );
 
        return cellList;
    }
    
    // -----------------------------------------------------------------------------------
    
    private void  addCell(List cellList, String columnName )
    {
        MonitorCell cell = null;
        
        cell = (MonitorCell) getCellMap().get(columnName);
        if(cell != null)
        {
            cellList.add(cell);
        }
        
        return;
    }
    
    // -----------------------------------------------------------------------------------

    public ThreatLevel getPrecipThreatLevelForPrecipThreatSummary()
    {
        ThreatLevel cellThreatLevel = ThreatLevel.NO_THREAT;

        List cellList = getPrecipCells();
        cellThreatLevel = getMaxRowThreatLevel(cellList);

        return cellThreatLevel;   
    }
    
    //----------------------------------------------------------------------------------

    public ThreatLevel getThreatLevelForValueAtThisTime(double value, long time, long earliestAcceptableTime,
            double actionValue, double floodValue)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;

        if (DbTable.isNull(value))
        {
            level = ThreatLevel.MISSING_DATA;
            //level = ThreatLevel.NO_THREAT;
        }
        else if(isDataAged(time, earliestAcceptableTime))
        {
            level = ThreatLevel.AGED_DATA;
            //level = ThreatLevel.NO_THREAT;
        }
        else if( ( ! DbTable.isNull(floodValue)) && value >= floodValue)
        {
            level = ThreatLevel.ALERT;
        }
        else if(( ! DbTable.isNull(actionValue))  && value >= actionValue)
        {
            level = ThreatLevel.CAUTION;
        }

        return level;
    }
    // -------------------------------------------------------------------------------------

    public ThreatLevel getThreatLevelForThisTime(long time, long earliestAcceptableTime)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        if (DbTable.isNull(time))
        {
            level = ThreatLevel.MISSING_DATA;
        }
        else if(isDataAged(time, earliestAcceptableTime))
        {
            level = ThreatLevel.AGED_DATA;
        }
        return level;
    }
    // -------------------------------------------------------------------------------------
    /*
     * The sshp time above flood level cell is colored red if there is any value in it, 
     * or it is colored white if it is blank or if the basis time is outside the window
     */
    public ThreatLevel getThreatLevelForSshpTimeAboveFloodLevel(long earliestAcceptableTime)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        if(isDataAged(getSshpFcstBasisTime(), earliestAcceptableTime))
        {
            level = ThreatLevel.NO_THREAT;
        }
        else if (! DbTable.isNull(getSshpFcstFloodTime()))
        {
            level = ThreatLevel.ALERT;
        }
        return level;
    }

    //--------------------------------------------------------------------------------------
    /*
     * Returns true if the data is aged, by checking the time with earliestAcceptableTime
     */
    protected boolean isDataAged(long  time, long earliestAcceptableTime)
    {
        boolean result = false;

        if (time < earliestAcceptableTime)
        {
            result = true;
        }

        return result;
    }

    // -------------------------------------------------------------------------------------
    protected ThreatLevel getThreatLevelBasedOnPrimaryPe(double value, long time, long earliestAcceptableTime)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        if(isPrimaryPeStage())
        {
            level = getThreatLevelForStage( value,  time,  earliestAcceptableTime);
        }
        else // 'Q'-- flow
        {
            level = getThreatLevelForFlow( value,  time,  earliestAcceptableTime);

        }
        return level;
    }
    // -------------------------------------------------------------------------------------
    protected ThreatLevel getThreatLevelForLatestObsValue(long earliestAcceptableTime)
    {
        ThreatLevel level = getThreatLevelBasedOnPrimaryPe(getLatestObsValue(), getLatestObsTime(), earliestAcceptableTime);
        return level;
    }

    // -------------------------------------------------------------------------------------

    protected ThreatLevel getThreatLevelForStage(double value, long time, long earliestAcceptableTime)
    {
        ThreatLevel level =  getThreatLevelForValueAtThisTime(value,  time,  earliestAcceptableTime,
            getActionStage(),  getFloodStage());

        return level;
    }
//  -------------------------------------------------------------------------------------
    protected ThreatLevel getThreatLevelForFlow(double value, long time, long earliestAcceptableTime)
    {
        ThreatLevel level =  getThreatLevelForValueAtThisTime(value,  time,  earliestAcceptableTime,
            getActionFlow(),  getFloodFlow());
        return level;
    }
    // -------------------------------------------------------------------------------------

    protected ThreatLevel getThreatLevelForMaxFcstValue(long earliestAcceptableTime)
    {
        ThreatLevel level = getThreatLevelBasedOnPrimaryPe(getMaxFcstValue(), getMaxFcstBasisTime(), earliestAcceptableTime);
        if(level == ThreatLevel.MISSING_DATA)
        {
            level = ThreatLevel.NO_THREAT;
        }
        return level;
    }

//  -------------------------------------------------------------------------------------

    protected ThreatLevel getThreatLevelForSshpFcstValue(long earliestAcceptableTime)
    {
        ThreatLevel level = getThreatLevelForStage(getSshpMaxFcstValue(), getSshpFcstBasisTime(), earliestAcceptableTime);
        if(level == ThreatLevel.MISSING_DATA || level == ThreatLevel.AGED_DATA)
        {
            level = ThreatLevel.NO_THREAT;
        }
        return level;
    }
//  -------------------------------------------------------------------------------------

    protected ThreatLevel getThreatLevelBasedOnValue(double value, double floodLevel, double actionLevel)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;

        if(DbTable.isNull(value))
        {
            level = ThreatLevel.MISSING_DATA;
        }
        else if(floodLevel != DbTable.getNullDouble() && value > floodLevel)
        {
            level = ThreatLevel.ALERT;
        }
        else if( actionLevel != DbTable.getNullDouble() && value > actionLevel)
        {
            level = ThreatLevel.CAUTION;
        }
        return level;
    }


//  -------------------------------------------------------------------------------------
    protected ThreatLevel getThreatLevelForObsFcstMax()
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        int numOfHrsToLookBack ;
        long earliestAcceptableTime ;
        
        if(getMaxFcstValue() == getLatestObsValue())
        {
            numOfHrsToLookBack = _menuSettings.getRiverSettings().getLatestObsLookBack();
            earliestAcceptableTime = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsToLookBack;
            ThreatLevel obslevel = getThreatLevelForLatestObsValue(earliestAcceptableTime);
            
            numOfHrsToLookBack = _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack();
            earliestAcceptableTime = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsToLookBack;
            ThreatLevel fcstlevel = getThreatLevelForMaxFcstValue(earliestAcceptableTime);
            
            if(obslevel == fcstlevel && obslevel == ThreatLevel.AGED_DATA)
            {
                level = obslevel;
            }
            else if(obslevel.isGreater(fcstlevel))
            {
                if(obslevel != ThreatLevel.AGED_DATA)
                    level = obslevel;
            }
            else
            {
                if(fcstlevel != ThreatLevel.AGED_DATA)
                  level = fcstlevel;
            }
        }
        else if(getObsFcstMax() == getMaxFcstValue()) // Fcst data
        {
            numOfHrsToLookBack = _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack();
            earliestAcceptableTime = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsToLookBack;
            level = getThreatLevelForMaxFcstValue(earliestAcceptableTime);
        }
        else // obs data
        {
            numOfHrsToLookBack = _menuSettings.getRiverSettings().getLatestObsLookBack();
            earliestAcceptableTime = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsToLookBack;
            level = getThreatLevelForLatestObsValue(earliestAcceptableTime);
        }
        return level;
    } 
//  -------------------------------------------------------------------------------------  
    protected ThreatLevel getThreatLevelForAlertAlarmSummary()
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;

        String summary = getAlertAlarmSummary();

        int indexOfL, indexOfU, indexOfC, indexOfR, indexOfD;
        indexOfL = -1; indexOfU = -1; indexOfC = -1; indexOfD = -1; indexOfR = -1;

        if(summary.length() > 0)
        {
            indexOfL = summary.indexOf('L');
            indexOfU = summary.indexOf('U');
            indexOfC = summary.indexOf('C');
            indexOfD = summary.indexOf('D');
            indexOfR = summary.indexOf('R');
            if(indexOfL != -1 || indexOfU != -1 || indexOfC != -1 || indexOfD != -1 || indexOfR != -1 )
            {
                level = ThreatLevel.ALERT;
            }
            else
            {
                level = ThreatLevel.CAUTION;
            }
        }

        return level;
    }
//  -------------------------------------------------------------------------------------  
    private ThreatLevel getPrecipThreatLevel(double value, double alert, double caution)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        
        if(! DbTable.isNull(value))
        {
            if(value >= alert)
                level = ThreatLevel.ALERT;
            else if(value >= caution)
                level = ThreatLevel.CAUTION;
        }

        return level;
    }
//  -------------------------------------------------------------------------------------  
    protected ThreatLevel getPrecipValueThreatLevelForHrMentioned(int hour, PrecipColumnDataSettings precipSettings, String type)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        double missing = DbTable.getNullDouble();
        double value;
        switch(hour)
        {

            case 1:
                if(type.compareTo("LATEST") == 0)
                    value = _precipData.getLatestPrecip1Hr();
                else
                    value = _precipData.getTohPrecip1Hr();
                
                level = getPrecipThreatLevel( value,  precipSettings.getPrecipAlert1Hr(),  precipSettings.getPrecipCaution1Hr());
                break;
                
            case 3:
                if(type.compareTo("LATEST") == 0)
                    value = _precipData.getLatestPrecip3Hr();
                else
                  //  value = _precipData.getTohPrecip3Hr();
                   value =  missing;
                
                level = getPrecipThreatLevel( value,  precipSettings.getPrecipAlert3Hr(),  precipSettings.getPrecipCaution3Hr());
                break;
            case 6:
                if(type.compareTo("LATEST") == 0)
                    value = _precipData.getLatestPrecip6Hr();
                else
                    //value = _precipData.getTohPrecip6Hr();
                    value =  missing;

                level = getPrecipThreatLevel( value,  precipSettings.getPrecipAlert6Hr(),  precipSettings.getPrecipCaution6Hr());
                break;

        }
        return level;

    }
//  -------------------------------------------------------------------------------------  
    protected ThreatLevel getPrecipRatioThreatLevelForHrMentioned(int hour, PrecipColumnDataSettings precipSettings)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        int value;
        switch(hour)
        {

            case 1:
                value = _precipData.getRatio1Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getRatioAlert1Hr(),  precipSettings.getRatioCaution1Hr());
                break;
            case 3:
                value = _precipData.getRatio3Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getRatioAlert3Hr(),  precipSettings.getRatioCaution6Hr());
                break;
            case 6:
                value = _precipData.getRatio6Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getRatioAlert6Hr(),  precipSettings.getRatioCaution6Hr());
                break;

        }
        return level;

    }
//  -------------------------------------------------------------------------------------  
    protected ThreatLevel getPrecipDiffThreatLevelForHrMentioned(int hour, PrecipColumnDataSettings precipSettings)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;
        double value;

        switch(hour)
        {
            case 1:
                value = _precipData.getDiff1Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getDiffAlert1Hr(),  precipSettings.getDiffCaution1Hr());
                break;
            case 3:
                value = _precipData.getDiff3Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getDiffAlert3Hr(),  precipSettings.getDiffCaution3Hr());
                break;
            case 6:
                value = _precipData.getDiff6Hr();
                level = getPrecipThreatLevel( value,  precipSettings.getDiffAlert6Hr(),  precipSettings.getDiffCaution6Hr());
                break;
        }
        return level;
    }
   
//  -------------------------------------------------------------------------------------  
  
    protected ThreatLevel getThreatLevelForPrecipColumn(String type, int hour)
    {

        ThreatLevel level = ThreatLevel.NO_THREAT;

        if(type.compareTo("INSTANT") == 0) // instant precip
        {
            level = getPrecipValueThreatLevelForHrMentioned(hour, _menuSettings.getPrecipSettings(), "INSTANT");
        }
        else if(type.compareTo("TOH") == 0) // toh precip
        {
            level = getPrecipValueThreatLevelForHrMentioned(hour, _menuSettings.getPrecipSettings(), "TOH");
        }
        else if(type.compareTo("RATIO") == 0)
        {
            level = getPrecipRatioThreatLevelForHrMentioned(hour, _menuSettings.getPrecipSettings());
        }
        else //type is DIFF
        {
            level = getPrecipDiffThreatLevelForHrMentioned(hour, _menuSettings.getPrecipSettings());
        }
        return level;
    }

//  -------------------------------------------------------------------------------------

    protected ThreatLevel getThreatLevelForVtecTimeCell(long time, long lookAheadHrs)
    {
        ThreatLevel level = ThreatLevel.NO_THREAT;

        long curTime = System.currentTimeMillis();
        String summary = getVtecSummary();

        if(summary.length() > 0)
        {
            if(time != DbTable.getNullLong())
            {
                if((curTime + (TimeHelper.MILLIS_PER_HOUR*lookAheadHrs)) >= time)
                {
                    level = ThreatLevel.ALERT;
                }
                else
                {
                    level = ThreatLevel.CAUTION;
                }
            }
            else
            {
                level = ThreatLevel.CAUTION;
            }
        }
        return level;
    }

//  -------------------------------------------------------------------------------------

    public ThreatLevel getCellThreatLevelForColumn(String columnName)
    {
        ThreatLevel level;

        MonitorCell cell = (MonitorCell) getCellMap().get(columnName);

        level = cell.getThreatLevel();

        return level;
    }

//  -------------------------------------------------------------------------------------


    public String getThreatSummary()
    {
        String summary = "";
        StringBuffer summaryStringBuffer = new StringBuffer("");
        StringBuffer toolTipStringBuffer = new StringBuffer("<HTML><BODY>");

        ThreatLevel level;
        level = getCellThreatLevelForColumn(RiverColumns.ALERT_ALARM);
        if(level == ThreatLevel.ALERT || level == ThreatLevel.CAUTION)
        {
            summaryStringBuffer = summaryStringBuffer.append("A");
            toolTipStringBuffer.append(_toolTipTextForAlertAlarmSummary);
        }
        
        level = getThreatLevelForPrecipThreatSummary();
        if(level == ThreatLevel.ALERT || level == ThreatLevel.CAUTION)
        {
            summaryStringBuffer = summaryStringBuffer.append("P");
            String precipThreatSummary = getPrecipThreatSummary();
            if(precipThreatSummary.contains("L"))
            {
                toolTipStringBuffer.append("P: Latest Precip has exceeded threat level<BR>");
            }
            if(precipThreatSummary.contains("T"))
            {
                toolTipStringBuffer.append("P: Top of the Hour Precip has exceeded threat level<BR>");
            }
            if(precipThreatSummary.contains("D"))
            {
                toolTipStringBuffer.append("P: TOH Precip - FFG diff has exceeded threat level<BR>");
            }
            if(precipThreatSummary.contains("R"))
            {
                toolTipStringBuffer.append("P: TOH Precip / FFG ratio has exceeded threat level<BR>");
            }
        }

        level = getCellThreatLevelForColumn(RiverColumns.VTEC_SUMMARY);
        if( level == ThreatLevel.ALERT || level == ThreatLevel.CAUTION)
        {
            summaryStringBuffer = summaryStringBuffer.append("V");
            int lookBackHours = _menuSettings.getRiverSettings().getVtecEventEndTimeApproach();
            String tempStr;
            String eventEndTimeString = getDataValue(RiverColumns.EVENT_END_TIME);
            if(level == ThreatLevel.ALERT)
            {
                tempStr = "V: VTEC EndTime["+ eventEndTimeString +"] is within " + lookBackHours+ "hrs<BR>";
            }
            else
            {
                tempStr = "V: VTEC EndTime["+ eventEndTimeString +"] is not within " + lookBackHours+ "hrs<BR>";
            }
            toolTipStringBuffer.append(tempStr);
        }

        level = getCellThreatLevelForColumn(RiverColumns.UGC_EXPIRE_TIME);
        if( level == ThreatLevel.ALERT || level == ThreatLevel.CAUTION )
        {
            summaryStringBuffer = summaryStringBuffer.append("X");
            int lookBackHours = _menuSettings.getRiverSettings().getUgcExpireTimeApproach();
            String tempStr ;
            String ugcExpireTimeString = getDataValue(RiverColumns.UGC_EXPIRE_TIME);
            if(level ==  ThreatLevel.ALERT)
            {
                tempStr = "X: UGC ExpireTime["+ ugcExpireTimeString +"] is within " + lookBackHours + "hrs or in the past<BR>";
            }
            else
            {
                tempStr = "X: UGC ExpireTime["+ ugcExpireTimeString +"] is not within " + lookBackHours+ "hrs<BR>";
            }
            toolTipStringBuffer.append(tempStr);
        }


        level = getCellThreatLevelForColumn(RiverColumns.MAX_FCST_VALUE);
        if( level == ThreatLevel.ALERT || level == ThreatLevel.CAUTION) 
        {
            summaryStringBuffer = summaryStringBuffer.append("F");
            if(level == ThreatLevel.ALERT)
            {
                toolTipStringBuffer.append("F: FcstValue exceeded the flood level<BR>");
            }
            else
            {
                toolTipStringBuffer.append("F: FcstValue exceeded the action level<BR>");
            }

        }

        level = getCellThreatLevelForColumn(RiverColumns.SSHP_MAX_FCST_VALUE);
        if( level == ThreatLevel.ALERT || level == ThreatLevel.CAUTION) 
        {
            summaryStringBuffer = summaryStringBuffer.append("S");
            if(level == ThreatLevel.ALERT)
            {
                toolTipStringBuffer.append("S: Sshp FcstValue exceeded the flood level<BR>");
            }
            else
            {
                toolTipStringBuffer.append("S: Sshp FcstValue exceeded the action level<BR>");
            }

        }

        level = ThreatLevel.NO_THREAT; // reset the threat level

        //check if any of the obs column are in alert threat
        if((getCellThreatLevelForColumn(RiverColumns.LAT_OBS_VALUE) == ThreatLevel.ALERT)
                || (getCellThreatLevelForColumn(RiverColumns.LAT_STG_VALUE) == ThreatLevel.ALERT)
                || (getCellThreatLevelForColumn(RiverColumns.LAT_FLOW_VALUE) == ThreatLevel.ALERT))
        {
            summaryStringBuffer = summaryStringBuffer.append("O");
            level = ThreatLevel.ALERT;
        }
        //check if any of the obs column are in caution threat
        else if((getCellThreatLevelForColumn(RiverColumns.LAT_OBS_VALUE) == ThreatLevel.CAUTION)
                || (getCellThreatLevelForColumn(RiverColumns.LAT_STG_VALUE) == ThreatLevel.CAUTION)
                || (getCellThreatLevelForColumn(RiverColumns.LAT_FLOW_VALUE) == ThreatLevel.CAUTION))
        {
            summaryStringBuffer = summaryStringBuffer.append("O");
            level = ThreatLevel.CAUTION;
        }

        if(level == ThreatLevel.ALERT )
        {
            toolTipStringBuffer.append("O: ObsValue exceeded the flood level<BR>");
        }
        else if(level == ThreatLevel.CAUTION)
        {
            toolTipStringBuffer.append("O: ObsValue exceeded the action level<BR>");
        }

        summary = summaryStringBuffer.toString();
        _toolTipTextForThreatSummary = toolTipStringBuffer.toString();    
        return summary;
    }


//  -------------------------------------------------------------------------------------


    public String getPrecipThreatSummary()
    {
        String summary = "";
        StringBuffer summaryStringBuffer = new StringBuffer("");
        StringBuffer toolTipStringBuffer = new StringBuffer("<HTML><BODY>");

        ThreatLevel level;
        ThreatLevel level1Hr = getCellThreatLevelForColumn(PrecipColumns.LATEST_1HR);
        ThreatLevel level3Hr = getCellThreatLevelForColumn(PrecipColumns.LATEST_3HR);
        ThreatLevel level6Hr = getCellThreatLevelForColumn(PrecipColumns.LATEST_6HR);
        if( ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION)) )
        {
            summaryStringBuffer = summaryStringBuffer.append("L");
            toolTipStringBuffer.append("L: Latest Precip Exceeded the threshold<BR>");
        }

        level1Hr = getCellThreatLevelForColumn(PrecipColumns.TOH_PRECIP_1HR);
      //  level3Hr = getCellThreatLevelForColumn(PrecipColumns.TOH_PRECIP_3HR);
      //  level6Hr = getCellThreatLevelForColumn(PrecipColumns.TOH_PRECIP_6HR);
        if( ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) 
               /* 
                ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION))
                */
              )
        {
            summaryStringBuffer = summaryStringBuffer.append("T");
            toolTipStringBuffer.append("T: Top Of The Hour Precip Exceeded the threshold<BR>");
        }
        
        level1Hr = getCellThreatLevelForColumn(PrecipColumns.DIFF_1HR);
        level3Hr = getCellThreatLevelForColumn(PrecipColumns.DIFF_3HR);
        level6Hr = getCellThreatLevelForColumn(PrecipColumns.DIFF_6HR);
        if( ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION)) )
        {
            summaryStringBuffer = summaryStringBuffer.append("D");
            toolTipStringBuffer.append("D: TOH Precip - FFG  Difference Exceeded the threshold<BR>");
        }

        level1Hr = getCellThreatLevelForColumn(PrecipColumns.RATIO_1HR);
        level3Hr = getCellThreatLevelForColumn(PrecipColumns.RATIO_3HR);
        level6Hr = getCellThreatLevelForColumn(PrecipColumns.RATIO_6HR);
        if( ((level1Hr == ThreatLevel.ALERT || level1Hr == ThreatLevel.CAUTION)) ||
                ((level3Hr == ThreatLevel.ALERT || level3Hr == ThreatLevel.CAUTION)) ||
                ((level6Hr == ThreatLevel.ALERT || level6Hr == ThreatLevel.CAUTION)) )
        {
            summaryStringBuffer = summaryStringBuffer.append("R");
            toolTipStringBuffer.append("R: TOH Precip / FFG Ratio Exceeded the threshold<BR>");
        }

        summary = summaryStringBuffer.toString();
        _toolTipTextForPrecipThreatSummary = toolTipStringBuffer.toString();    
        return summary;
    }
//  -------------------------------------------------------------------------------------

    public void setRiverBasin(String riverBasin)
    {
        _riverBasin = riverBasin;
    }

    public String getRiverBasin()
    {
        return _riverBasin;
    }

    
} //end class 
