package ohd.hseb.monitor.river;

import static ohd.hseb.util.TimeHelper.MILLIS_PER_HOUR;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.ihfsdb.generated.AdminRecord;
import ohd.hseb.ihfsdb.generated.AdminTable;
import ohd.hseb.ihfsdb.generated.AlertAlarmValRecord;
import ohd.hseb.ihfsdb.generated.AlertAlarmValTable;
import ohd.hseb.ihfsdb.generated.IngestFilterRecord;
import ohd.hseb.ihfsdb.generated.IngestFilterTable;
import ohd.hseb.ihfsdb.generated.LocRiverMonRecord;
import ohd.hseb.ihfsdb.generated.LocRiverMonView;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.ihfsdb.generated.RiverStatusRecord;
import ohd.hseb.ihfsdb.generated.RiverStatusTable;
import ohd.hseb.ihfsdb.generated.ShefPeRecord;
import ohd.hseb.ihfsdb.generated.ShefPeTable;
import ohd.hseb.ihfsdb.generated.VTECeventRecord;
import ohd.hseb.ihfsdb.generated.VTECeventTable;
import ohd.hseb.measurement.AbsTimeMeasurement;
import ohd.hseb.measurement.MeasuringUnit;
import ohd.hseb.measurement.RegularTimeSeries;
import ohd.hseb.monitor.LocationDataFilter;
import ohd.hseb.monitor.TreeFilterManager;
import ohd.hseb.monitor.derivedcolumns.DerivedColumn;
import ohd.hseb.monitor.derivedcolumns.DerivedColumnsFileManager;
import ohd.hseb.monitor.precip.PrecipData;
import ohd.hseb.monitor.precip.PrecipDataManager;
import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.officenotes.OfficeNotesDataManager;
import ohd.hseb.monitor.river.RiverMonitorJTableRowData;
import ohd.hseb.monitor.river.settings.RiverMonitorMenuSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.rivermonlocgroup.RiverMonLocGroupDataManager;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.TimeHelper;
import ohd.hseb.util.TimeSeriesFileManager;

import org.apache.bsf.BSFEngine;
import org.apache.bsf.BSFManager;

public class RiverMonitorDataManager
{
    private Database _db;

    private AppsDefaults _appsDefaults;

    private SessionLogger _logger;

    private String _defaultHsa;
    private Set<String> _riverLidSet = null;

    private String _peConfigFileName;
    private List _validHeightAndDischargePEList;

    private DerivedColumnsFileManager _derivedColumnsFileManager;
    private List _derivedColumnsList;
    private BSFManager _bsfManager;
    private BSFEngine _beanShellEngine;

    private String _missingRepresentation;

    private List<RiverMonitorJTableRowData> _riverMonitorRowDataList;
    private List<RiverMonitorJTableRowData> _missingRiverMonitorRowDataList = new ArrayList<RiverMonitorJTableRowData>();
    private Map<String, String> _lidToRiverBasinMap = new HashMap<String, String>();

    private Map<String, String> _lidDescDetailsMap;

    private TreeFilterManager _treeFilterManager;

    private String _lidOfCurrentInterest;

    private RiverMonitorMenuSettings _menuSettings;

    private RiverMonLocGroupDataManager _riverMonLocGroupDataManager;
    private OfficeNotesDataManager _officeNotesDataManager;

    private Map _sshpLidFcstMap;
    private String _sshpFcstDataDir;

    private PrecipDataManager _precipDataManager;

    private Map _lidHeightPEMap = null;
    private Map _lidDischargePEMap = null;

    public RiverMonitorDataManager(Database db, AppsDefaults appsDefaults, String missingRepresentation, 
            SessionLogger logger, RiverMonitorMenuSettings menuSettings,
            DerivedColumnsFileManager derivedColumnsFileManager)
    {
        _appsDefaults = appsDefaults;
        _menuSettings = menuSettings;
        _db = db;
        _missingRepresentation = missingRepresentation;
        _logger = logger;

        readValidHeightAndDischargePE();
        _precipDataManager = new PrecipDataManager(_db, _appsDefaults, _missingRepresentation, _logger);

        _sshpFcstDataDir = _appsDefaults.getToken("sshp_background_forecast_output_dir", 
        	HydroappsDefaultDirs.SSHP_BACKGROUND_FORECAST_OUTPUT_DIR);
        String settingsDir = _appsDefaults.getToken("rivermon_config_dir", "awips/hydroapps/whfs/local/data/app/rivermon");

        _peConfigFileName = settingsDir + "/" + "RiverMonitorPEConfig.txt";

        readAdminTable();
        _derivedColumnsFileManager = derivedColumnsFileManager; 

        _riverMonLocGroupDataManager = new RiverMonLocGroupDataManager(db, _logger, missingRepresentation, getDefaultHsa());

        _officeNotesDataManager = new OfficeNotesDataManager(db,_logger, missingRepresentation);

        try
        {
            initializeBSF();
        }
        catch (Exception e)
        {
            _logger.log("PrecipMonitorDataManager.PrecipMonitorDataManager(): Error while initialize BSF..." + e);
        }
    }

    public RiverMonLocGroupDataManager getRiverMonLocGroupDataManager()
    {
        return _riverMonLocGroupDataManager;
    }

    public OfficeNotesDataManager getOfficeNotesDataManager()
    {
        return _officeNotesDataManager;
    }

    public TreeFilterManager getTreeFilterManager()
    {
        if(_treeFilterManager == null)
        {
            _treeFilterManager = new TreeFilterManager(_logger, getLocationDataFilter());
        }
        return _treeFilterManager;
    }

    public void setLidOfCurrentInterest(String lid)
    {
        _lidOfCurrentInterest = lid;    
    }

    public String getLidOfCurrentInterest()
    {
        return _lidOfCurrentInterest;
    }
    
    public long getPdcUpdateTime()
    {
        return _precipDataManager.getPdcFileUpdateLongTime();
    }
    
    public long getDisplayedRecordCount()
    {
 
        List selectedRowDataList = getTreeFilterManager().filter(_riverMonitorRowDataList);
        long displayedCount = selectedRowDataList.size();
        
        return displayedCount;
    }

    public void disconnect()
    {
        _db.disconnect();
    }

    private void printLocationCounts(String header)
    {
        System.out.println(header + "There are "  + _riverMonitorRowDataList.size() + 
                " displayed with " + _missingRiverMonitorRowDataList.size() + " non-displayed missing locations." );

    }
    
    
    public Map<String, Color> getLidToCriticalColorMap()
    {
        String header = "RiverMonitorDataManager.getLidToCriticalColorMap(): " ;
  
        Map<String, Color> lidToCriticalColorMap = new HashMap<String, Color>();
        
        for(RiverMonitorJTableRowData rowData:_riverMonitorRowDataList)
        {
            String lid = rowData.getLid();
            Color criticalColor = rowData.getCriticalColorForThisRow();
            lidToCriticalColorMap.put(lid, criticalColor);
        }
        
        for(RiverMonitorJTableRowData rowData:_missingRiverMonitorRowDataList)
        {
            String lid = rowData.getLid();
            Color criticalColor = rowData.getCriticalColorForThisRow();
            lidToCriticalColorMap.put(lid, criticalColor);
        }
        
        return lidToCriticalColorMap;
    }

    private void readAdminTable()
    {
        AdminTable adminTable = new AdminTable(_db);
        List adminList = null;
        try
        {
            adminList = adminTable.select("");
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
        _defaultHsa = ((AdminRecord) adminList.get(0)).getHsa();
    }

    protected void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " + exception.getErrorCode() + " " + exception.getMessage());

        exception.printStackTrace(_logger.getPrintWriter());

        _logger.log("End of stack trace");

    }

    private LocationDataFilter getLocationDataFilter()
    {
        RiverLocationDataFilter locationDataFilter;
        List<String> lidList = getLidList();
        Map<String, Boolean> locationDisplayMap = new HashMap<String, Boolean>();

        for(String lid:lidList)
        {
            locationDisplayMap.put(lid, false);
        }

        locationDataFilter = new RiverLocationDataFilter(locationDisplayMap, _menuSettings.getShowMissingDataBooleanHolder());
        return locationDataFilter;
    }

    public List<String> getLidList()
    {
        List<String> lidList = new ArrayList<String>();
        for(RiverMonitorJTableRowData rowData:_riverMonitorRowDataList)
        {
            String lid = rowData.getLid();
            lidList.add(lid);
        }
        return lidList;
    }

//  ---------------------------------------------------------------------------------------------------

    private void initializeBSF() throws Exception
    {
        _bsfManager = new BSFManager();
        BSFManager.registerScriptingEngine("beanshell", "bsh.util.BeanShellBSFEngine", null);
        _beanShellEngine = _bsfManager.loadScriptingEngine("beanshell");
    }
//  ---------------------------------------------------------------------------------------------------

    protected void getDerivedColumnsInfo(RiverMonitorJTableRowData riverMonitorRowData)
    {
        
        String header = "RiverMonitorDataManger.getDerivedColumnsInfo():" ;
        
        if(_derivedColumnsList != null)
        {
            try
            {
                _bsfManager.declareBean("row", riverMonitorRowData, RiverMonitorJTableRowData.class);
                _bsfManager.declareBean("precip", riverMonitorRowData.getPrecipData(), PrecipData.class);
                for (int j = _derivedColumnsList.size() - 1; j >= 0; j--)
                {
                    DerivedColumn derivedColumnDesc = (DerivedColumn) _derivedColumnsList.get(j);
                    String equationForCellValue = ((String) derivedColumnDesc.getEquationForCellValue());
             
                    Object returnValueForCell = _beanShellEngine.eval("rowtest", 1, 1, equationForCellValue);
                    if (derivedColumnDesc.getReturnType().equalsIgnoreCase("double"))
                    {
                        double value = roundTo2DecimalPlaces(Double.parseDouble(returnValueForCell.toString()));
                        derivedColumnDesc.setvalue(Double.valueOf(value));
                    }
                    else
                    {
                        derivedColumnDesc.setvalue(returnValueForCell);
                    }
                    
                    String equationForCellColor = ((String) derivedColumnDesc.getEquationForCellColor());
                    Object returnColorForCell = _beanShellEngine.eval("rowtest", 1, 1, equationForCellColor);
                    if (returnColorForCell != null)
                    {
                        derivedColumnDesc.setCellBackgroundColor(returnColorForCell.toString());
                    }
                    else
                    {
                        System.out.println(header + " equationForCellColor = " + equationForCellColor +
                                                    " returnColorForCell = " + returnColorForCell);
                        derivedColumnDesc.setCellBackgroundColor("WHITE");
                    }
                    
                }
                _bsfManager.undeclareBean("row");
                _bsfManager.undeclareBean("precip");
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
        riverMonitorRowData.setDerivedColumnsDetailsList(_derivedColumnsList);
    }

    public void alterRiverMonitorRowDataListBasedOnMissingPrecipFilter()
    {
        _missingRiverMonitorRowDataList.clear();
        
        if(! _menuSettings.shouldShowMissingRiver() ) // show missing river is set to false(ie., skip missing river)
        {
            List<RiverMonitorJTableRowData> finalRiverMonitorRowDataList = new ArrayList<RiverMonitorJTableRowData>();

            //for each point, check to see if it has some data, if so, then add it to the list.
            //at the end, replace the  _riverMonitorRowDataList with the filtered list.
            for(RiverMonitorJTableRowData riverMonitorRowData: _riverMonitorRowDataList)
            {
                if(riverMonitorRowData.getLatestObsValue() != DbTable.getNullDouble() ||
                        riverMonitorRowData.getMaxFcstValue() != DbTable.getNullDouble())
                {
                    finalRiverMonitorRowDataList.add(riverMonitorRowData);
                }
                else
                {
                    //add to the missing list so that the RiverMonitorDataManager.getLidToCriticalColorMap() method can 
                    //give a complete list
                    _missingRiverMonitorRowDataList.add(riverMonitorRowData);
                }
            }
            _riverMonitorRowDataList = finalRiverMonitorRowDataList;
            finalRiverMonitorRowDataList = null;
            Runtime.getRuntime().gc();
        }
    }
    
    /**
     * Reads the locrivermon view and creates the list of RiverMonitorJtableRowdata objects for each location
     * and fills in its name, state , county info
     * @return
     */
    public List<RiverMonitorJTableRowData> readRiverLocationInfo()
    {
        if(_riverLidSet == null)
            _riverLidSet = new HashSet<String> ();
        else
            _riverLidSet.clear();

        if(_riverMonitorRowDataList != null)
        {
            for(RiverMonitorJTableRowData riverMonitorRowData: _riverMonitorRowDataList)
            {
                riverMonitorRowData.resetCellMap();
            }
            _riverMonitorRowDataList.clear();
        }

        Runtime.getRuntime().gc();
        List<RiverMonitorJTableRowData> riverMonitorRowDataList = new ArrayList<RiverMonitorJTableRowData>();

        LocRiverMonView locRivermonView = new LocRiverMonView(_db);
        List<LocRiverMonRecord> locRiverMonRecordList;
        
        loadRiverBasinData();

        try
        {
            String whereClause = "order by lid";
            locRiverMonRecordList = locRivermonView.select(whereClause);
            if(locRiverMonRecordList != null)
            {
                for(LocRiverMonRecord locRiverMonRecord : locRiverMonRecordList)
                {
                    RiverMonitorJTableRowData riverMonitorRowData = new RiverMonitorJTableRowData(_missingRepresentation, 
                        this, _logger, _menuSettings);
                    riverMonitorRowData.setLid(locRiverMonRecord.getLid());
                    
                    riverMonitorRowData.setName(locRiverMonRecord.getName());
                    riverMonitorRowData.setCounty(locRiverMonRecord.getCounty());
                    riverMonitorRowData.setState(locRiverMonRecord.getState());
                    riverMonitorRowData.setStream(locRiverMonRecord.getStream());
                    riverMonitorRowData.setHsa(locRiverMonRecord.getHsa());

                    String primaryRiverPe = locRiverMonRecord.getPrimary_pe();
                    if (primaryRiverPe == null || primaryRiverPe.length() < 2)
                    {
                        primaryRiverPe = "HG";
                    }
                    riverMonitorRowData.setPrimaryRiverPe(primaryRiverPe);
                    
                  
                    riverMonitorRowData.setBankFull(roundTo2DecimalPlaces(locRiverMonRecord.getBankfull()));
                    
                    double processedValue = 0.0;
                     
                    processedValue =  getRiverLevelProcessedValue (locRiverMonRecord.getAction_stage());
                    riverMonitorRowData.setActionStage( processedValue );
                   
                    processedValue =  getRiverLevelProcessedValue (locRiverMonRecord.getFlood_stage());
                    riverMonitorRowData.setFloodStage( processedValue );
                    
                    processedValue =  getRiverLevelProcessedValue (locRiverMonRecord.getAction_flow());
                    riverMonitorRowData.setActionFlow( processedValue );
                   
                    processedValue =  getRiverLevelProcessedValue (locRiverMonRecord.getFlood_flow());
                    riverMonitorRowData.setFloodFlow( processedValue );
                  
                    riverMonitorRowData.setMinorStage(roundTo2DecimalPlaces(locRiverMonRecord.getMinor()));
                    riverMonitorRowData.setMajorStage(roundTo2DecimalPlaces(locRiverMonRecord.getMajor()));
                    riverMonitorRowData.setModerateStage(roundTo2DecimalPlaces(locRiverMonRecord.getModerate()));

                    setRiverBasinData(riverMonitorRowData);
                    
                    riverMonitorRowDataList.add(riverMonitorRowData);
                    
           
                    _riverLidSet.add(locRiverMonRecord.getLid());
                }
            }
            
          


        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        return riverMonitorRowDataList;
    }
    
    
    // -----------------------------------------------------------------------------------------
    private double getRiverLevelProcessedValue(double value)
    {
        double returnValue = value;
        
        if(value == 0.0)
        {
            returnValue = DbTable.getNullDouble();
        }
        else
        {
            returnValue = roundTo2DecimalPlaces(value);
        }
        
        return returnValue;
    }
    
    // -----------------------------------------------------------------------------------------

    private void setRiverBasinData(RiverMonitorJTableRowData rowData)
    {
         String riverBasin = _lidToRiverBasinMap.get(rowData.getLid());
        
         rowData.setRiverBasin(riverBasin);
         
         return;
    }
    
    private void loadRiverBasinData()
    {
        LocationTable table = new LocationTable(_db);
        String whereClause = "order by lid";
        List<LocationRecord> recordList = null;
        
        _lidToRiverBasinMap.clear();
        try
        {
            recordList = (List<LocationRecord>) table.select(whereClause);
            for (LocationRecord record : recordList)
            {
                _lidToRiverBasinMap.put(record.getLid(), record.getRb());
            }
        }
        catch (SQLException e)
        {
            
        }
        
    }
    
    public boolean isRiverLocation(String lid)
    {
        boolean result = false;

        if(_riverLidSet != null)
        {
            if(_riverLidSet.contains(lid))
            {
                result = true;
            }
        }
        return result;
    }

    protected void readCurrentRiverMonTableData()
    {
       /* String header = "RiverMonitorDataManager.readCurrentRiverMonTableData(): ";
        _riverMonLocGroupDataManager.readLocationHsaMap();
        _riverMonLocGroupDataManager.readGroupInfo();
        _riverMonLocGroupDataManager.readLocationInfo();
        _riverMonLocGroupDataManager.readGroupHsaMap();
        _riverMonLocGroupDataManager.updateRiverMonitorGroupWithAppropriateHsa();
        */
        _riverMonLocGroupDataManager.initializeRiverMonTableData();
    }

    public List<RiverMonitorJTableRowData> getRiverMonitorRowDataList()
    {
        return _riverMonitorRowDataList;
    }

    public void createRiverMonitorRowDataList()
    {
        String header = "RiverMonitorDataManager.createRiverMonitorRowDataList(): ";

        _logger.log(header +" Memory Info before creating rowdata list:" + printMemoryInfo());
        Runtime.getRuntime().gc();
        _riverMonitorRowDataList = readRiverLocationInfo();

        String fcstTs = _menuSettings.getRiverSettings().getFcstTypeSource();

        readConfigFileToFormLidHeightPEAndLidDischargePEMap();

        Map lidRowDataMap = new HashMap();
        if(_riverMonitorRowDataList != null)
        {
            String dischargePE = null;
            String heightPE = null;

            readCurrentRiverMonTableData();
            
            Runtime.getRuntime().gc();
            _logger.log(header + " Here 1: " + printMemoryInfo());
            _precipDataManager.readPrecipData(_riverMonitorRowDataList, "RIVER");
            Runtime.getRuntime().gc();
            _logger.log(header + " Here 2: "+ printMemoryInfo());

            Map vtecMap = createGeoIdToVtecEventListMap();
            Map alertAlarmMap = createAlertAlarmInfoToLidMap();
            determineLidToSshpFcstMap();

            _derivedColumnsList = _derivedColumnsFileManager.getDerivedColumns();

            for(RiverMonitorJTableRowData riverMonitorRowData: _riverMonitorRowDataList)
            {
                String lid = riverMonitorRowData.getLid();
                String groupId = _riverMonLocGroupDataManager.getGroupId(lid);
                String groupName = _riverMonLocGroupDataManager.getGroupName(groupId);
                int groupOrdinal = _riverMonLocGroupDataManager.getGroupOrdinal(groupId);
                //String hsa = _riverMonLocGroupDataManager.getHsa(groupId);
                int locationOrdinal = _riverMonLocGroupDataManager.getLocationOrdinal(lid);

                riverMonitorRowData.setLid(riverMonitorRowData.getLid());
                riverMonitorRowData.setLocationOrdinal(locationOrdinal);
                //riverMonitorRowData.setHsa(hsa);
                riverMonitorRowData.setGroupName(groupName);
                riverMonitorRowData.setGroupId(groupId);
                riverMonitorRowData.setGroupOrdinal(groupOrdinal);

                // Read the latest obs from riverstatus for this lid
                RiverStatusRecord riverStatusRecordForObs = null;
                riverStatusRecordForObs = determineObsOrFcstRecordFromRiverStatus(lid,
                    riverMonitorRowData.getPrimaryRiverPe(), "OBS", null);

                if (riverStatusRecordForObs == null)
                {
                    riverMonitorRowData.setLatestObsValue(DbTable.getNullDouble());
                    riverMonitorRowData.setLatestObsTime(DbTable.getNullLong());
                    riverMonitorRowData.setObsTypeSource(null);
                }
                else
                {
                    riverMonitorRowData
                    .setLatestObsValue(roundTo2DecimalPlaces(riverStatusRecordForObs.getValue()));
                    riverMonitorRowData.setLatestObsTime(riverStatusRecordForObs.getValidtime());
                    riverMonitorRowData.setObsTypeSource(riverStatusRecordForObs.getTs());
                }

                // Read the max future forecast from riverstatus for this
                // lid
                RiverStatusRecord riverStatusRecordForFcst = determineObsOrFcstRecordFromRiverStatus(lid, 
                    riverMonitorRowData.getPrimaryRiverPe(),"FCST",  fcstTs);
                if (riverStatusRecordForFcst == null)
                {
                    riverMonitorRowData.setMaxFcstValidTime(DbTable.getNullLong());
                    riverMonitorRowData.setMaxFcstBasisTime(DbTable.getNullLong());
                    riverMonitorRowData.setMaxFcstValue(DbTable.getNullDouble());
                    if (!fcstTs.equalsIgnoreCase("IngestFilter"))
                        riverMonitorRowData.setFcstTypeSource(fcstTs);
                    else
                        riverMonitorRowData.setFcstTypeSource(null);
                }
                else
                {
                    riverMonitorRowData.setMaxFcstValue(roundTo2DecimalPlaces(riverStatusRecordForFcst.getValue()));
                    riverMonitorRowData.setMaxFcstValidTime(riverStatusRecordForFcst.getValidtime());
                    riverMonitorRowData.setMaxFcstBasisTime(riverStatusRecordForFcst.getBasistime());
                    riverMonitorRowData.setFcstTypeSource(riverStatusRecordForFcst.getTs());
                }

                dischargePE = null;
                heightPE = null;

                // heightPE = ""; dischargePE = "";
                if (_lidHeightPEMap != null && _lidDischargePEMap != null)
                {
                    if (_lidHeightPEMap.size() > 0 && _lidDischargePEMap.size() > 0)
                    {
                        heightPE = (String) _lidHeightPEMap.get(lid);
                        dischargePE = (String) _lidDischargePEMap.get(lid);
                    }
                }
                if (heightPE == null || dischargePE == null) // No entry
                    // found in
                    // file
                {
                    char firstLetterInPrimaryPE = riverMonitorRowData.getPrimaryRiverPe().charAt(0);

                    if (firstLetterInPrimaryPE == 'H')
                    {
                        heightPE = riverMonitorRowData.getPrimaryRiverPe();
                        dischargePE = getEquivalentDischargePE(riverMonitorRowData.getPrimaryRiverPe());
                    }
                    else
                        // firstLetterInPrimaryPE is 'Q'
                    {
                        dischargePE = riverMonitorRowData.getPrimaryRiverPe();
                        heightPE = getEquivalentHeightPE(riverMonitorRowData.getPrimaryRiverPe());
                    }
                }

                riverStatusRecordForObs = null;
                if (heightPE != null) // could be null if the user hasn't
                    // configured and if the primary pe
                    // is not QR, QT
                    riverStatusRecordForObs = determineObsOrFcstRecordFromRiverStatus(lid, heightPE, "OBS", null);

                if (riverStatusRecordForObs == null)
                {
                    riverMonitorRowData.setLatestStgValue(DbTable.getNullDouble());
                    riverMonitorRowData.setLatestStgTime(DbTable.getNullLong());
                }
                else
                {
                    riverMonitorRowData
                    .setLatestStgValue(roundTo2DecimalPlaces(riverStatusRecordForObs.getValue()));
                    riverMonitorRowData.setLatestStgTime(riverStatusRecordForObs.getValidtime());
                }

                riverStatusRecordForObs = null;
                if (dischargePE != null) // could be null if the user
                    // hasn't configured and if the
                    // primary pe is not HG, HT
                    riverStatusRecordForObs = determineObsOrFcstRecordFromRiverStatus(lid, dischargePE, "OBS", null);

                if (riverStatusRecordForObs == null)
                {
                    riverMonitorRowData.setLatestFlowValue(DbTable.getNullDouble());
                    riverMonitorRowData.setLatestFlowTime(DbTable.getNullLong());
                }
                else
                {
                    riverMonitorRowData
                    .setLatestFlowValue(roundTo2DecimalPlaces(riverStatusRecordForObs.getValue()));
                    riverMonitorRowData.setLatestFlowTime(riverStatusRecordForObs.getValidtime());
                }

                SshpFcstDetails sshpFcstDetails = getSshpFcstDetailsForLid(riverMonitorRowData.getLid(),
                    riverMonitorRowData.getFloodStage());
                riverMonitorRowData.setSshpFcstDetails(sshpFcstDetails);

                int numOfHrsForObsLookBack = _menuSettings.getRiverSettings().getLatestObsLookBack();
                long earliestAcceptableTimeForObs = System.currentTimeMillis() - TimeHelper.MILLIS_PER_HOUR * numOfHrsForObsLookBack;

                if (riverMonitorRowData.getLatestObsTime() > earliestAcceptableTimeForObs)
                {
                    riverMonitorRowData.setObsFcstMax(roundTo2DecimalPlaces(Math.max(riverMonitorRowData
                        .getLatestObsValue(), riverMonitorRowData.getMaxFcstValue())));
                }
                else
                {
                   riverMonitorRowData.setObsFcstMax(roundTo2DecimalPlaces(riverMonitorRowData.getMaxFcstValue()));
                }
                
                if ((riverMonitorRowData.getObsFcstMax() != DbTable.getNullDouble())
                        && (riverMonitorRowData.getFloodStage() != DbTable.getNullDouble()))
                {
                    riverMonitorRowData.setFldStgDeparture(roundTo2DecimalPlaces(riverMonitorRowData.getObsFcstMax()
                                            - riverMonitorRowData.getFloodStage()));
                }
                else
                {
                    riverMonitorRowData.setFldStgDeparture(roundTo2DecimalPlaces(DbTable.getNullDouble()));
                }

                if ((riverMonitorRowData.getObsFcstMax() != DbTable.getNullDouble())
                        && (riverMonitorRowData.getActionStage() != DbTable.getNullDouble()))
                    riverMonitorRowData.setActStgDeparture(roundTo2DecimalPlaces(riverMonitorRowData
                        .getObsFcstMax()
                        - riverMonitorRowData.getActionStage()));
                else
                    riverMonitorRowData.setActStgDeparture(roundTo2DecimalPlaces(DbTable.getNullDouble()));

                riverMonitorRowData.setVtecEventList(getVtecEventList(lid, vtecMap));
                
                setAlertAlarmDetails(riverMonitorRowData, alertAlarmMap);

                /*     _logger.log(precipMonitorRowData.getLid() + "|" + precipMonitorRowData.getName() + "|"
                    + precipMonitorRowData.getGroupId() + "|" + precipMonitorRowData.getGroupName() + "|"
                    + precipMonitorRowData.getLocationOrdinal() + "|"
                    + precipMonitorRowData.getGroupOrdinal() );*/


                riverMonitorRowData.addAllCellsToMap();
                
                getDerivedColumnsInfo(riverMonitorRowData);
                riverMonitorRowData.addDerivedDataToCellMap();
                
                lidRowDataMap.put(riverMonitorRowData.getLid(), riverMonitorRowData);

            }
            
            /*
            _logger.log(header + "Before missing precip filter is applied, alllocationinfo list size:" + _riverMonitorRowDataList.size());
            
            alterRiverMonitorRowDataListBasedOnMissingPrecipFilter();
            
            _logger.log(header + "After missing precip filter is applied, alllocationinfo list size:" + _riverMonitorRowDataList.size());
            */
        }
       
         Runtime.getRuntime().gc();
        _logger.log(header +" Memory Info after creating rowdata list:" + printMemoryInfo());

    }

//  ---------------------------------------------------------------------------------------------------
    
    public String printMemoryInfo()
    {
        String str = "Free:" + Runtime.getRuntime().freeMemory()+ " Max:"+ Runtime.getRuntime().maxMemory()+ " total:"+ Runtime.getRuntime().totalMemory();
        return str;
    }

    public PrecipColumnDataSettings getPrecipSettings()
    {
        return _menuSettings.getPrecipSettings();
    }
    
//  ---------------------------------------------------------------------------------------------------
    
    private List getListOfLocationIds()
    {
        List<String> locationIdList = new ArrayList<String>();
        
        for (RiverMonitorJTableRowData rowData : _riverMonitorRowDataList)
        {
            locationIdList.add(rowData.getLid());
        }
        
        return locationIdList;
        
    }
//  ---------------------------------------------------------------------------------------------------
    
    public List createStringArrayListOfFilterItems()
    {
        String header = "RiverMonitorDataManager.createStringArrayListOfFilterItems(): ";
        
        List<String> locationIdList = getListOfLocationIds();
        
        List<String[]> listOfNodePathStringArray = 
                    _riverMonLocGroupDataManager.createStringArrayListOfFilterItems(locationIdList);
        
        return listOfNodePathStringArray;
    }
    
//  ---------------------------------------------------------------------------------------------------
/*
    private List createStringArrayListOfFilterItemsOld()
    {
        List<String[]> listOfStringArray = new ArrayList<String[]>();

        List<String> locationIdList = getListOfLocationIds();
        
        
        String rootItem = "HSA";
        String firstLevelFilterItems[] = _riverMonLocGroupDataManager.getAllHsa();
        String[] currentStringArray;

        if(firstLevelFilterItems != null && firstLevelFilterItems.length > 0)
        {
            String currentFirstLevelFilterItem = null;
            for(int i=0; i < firstLevelFilterItems.length; i++)
            {
                currentFirstLevelFilterItem = firstLevelFilterItems[i];
                String secondLevelFilterItems[] = _riverMonLocGroupDataManager.getGroupsForHsa(firstLevelFilterItems[i]);
                if(secondLevelFilterItems != null && secondLevelFilterItems.length > 0)
                {
                    String currentSecondLevelFilterItem = null;
                    for(int j=0; j < secondLevelFilterItems.length; j++)
                    {
                        currentSecondLevelFilterItem = _riverMonLocGroupDataManager.getGroupName(secondLevelFilterItems[j]);
                        String thirdLevelFilterItems[] = _riverMonLocGroupDataManager.getAllLocationsForGroup(secondLevelFilterItems[j], locationIdList);
                        if(thirdLevelFilterItems != null && thirdLevelFilterItems.length > 0)
                        {
                            for(int k=0; k < thirdLevelFilterItems.length; k++)
                            {
                                if(isRiverLocation(thirdLevelFilterItems[k]))
                                {
                                    currentStringArray = new String[4];
                                    currentStringArray[0] = rootItem;
                                    currentStringArray[1] = currentFirstLevelFilterItem;
                                    currentStringArray[2] = currentSecondLevelFilterItem;
                                    currentStringArray[3] = thirdLevelFilterItems[k];
                                    listOfStringArray.add(currentStringArray);
                                }
                            }
                        }
                        else
                        {
                            currentStringArray = new String[3];
                            currentStringArray[0] = rootItem;
                            currentStringArray[1] = currentFirstLevelFilterItem;
                            currentStringArray[2] = currentSecondLevelFilterItem;
                            listOfStringArray.add(currentStringArray);
                        }
                    }
                }
                else
                {
                    currentStringArray = new String[2];
                    currentStringArray[0] = rootItem;
                    currentStringArray[1] = currentFirstLevelFilterItem;
                    listOfStringArray.add(currentStringArray);
                }
            }
        }
        else
        {
            currentStringArray = new String[1];
            currentStringArray[0] = rootItem;
            listOfStringArray.add(currentStringArray);
        }

        return listOfStringArray;
    }

*/
    
    private Map createGeoIdToVtecEventListMap()
    {
        String header = "RiverMonitorDataManager.createGeoIdToVtecEventListMap():";
        Map vtecMap = new HashMap();
        VTECeventTable vtecEventTable = new VTECeventTable(_db);
        String whereClause = null;
        CodeTimer timer = new CodeTimer();
        try
        {
            timer.start();

            // retrieve data for VTEC events which are not statements and the
            // end time has been set
            whereClause = " where signif != 'S' and endtime is not null order by geoid, endtime asc";
            _logger.log(header + " where clause:" + whereClause);
            List tempVtecEventWhichArentStatementAndEndTimeIsNotNull = vtecEventTable.select(whereClause);

            // retrieve data for VTEC events which are not statements and the
            // end time has NOT been set
            whereClause = " where signif != 'S' and endtime is null order by geoid ";
            _logger.log(header + " where clause:" + whereClause);
            List tempVtecEventWhichArentStatementAndEndTimeIsNull = vtecEventTable.select(whereClause);

            // merge the two lists
            List allVtecEventWhichArentStatementsList = new ArrayList();
            allVtecEventWhichArentStatementsList.addAll(tempVtecEventWhichArentStatementAndEndTimeIsNotNull);
            allVtecEventWhichArentStatementsList.addAll(tempVtecEventWhichArentStatementAndEndTimeIsNull);

            timer.stop("Rivermonitor data mgr- read vtec event time elapsed:");

            // assign each record to the map
            for (int i = 0; i < allVtecEventWhichArentStatementsList.size(); i++)
            {
                VTECeventRecord record = (VTECeventRecord) allVtecEventWhichArentStatementsList.get(i);
                String geoId = record.getGeoid();

                List eventList = (List) vtecMap.get(geoId);
                if (eventList == null)
                {
                    eventList = new ArrayList();
                    vtecMap.put(geoId, eventList);
                }
                eventList.add(record);
            }
        }
        catch (SQLException e)
        {
            _logger.log(header);
            logSQLException(e);
        }

        return vtecMap;
    }

    private List getVtecEventList(String lid, Map vtecMap)
    {
        List finalVtecEventList = null;

        if (vtecMap != null)
        {
            if (vtecMap.size() > 0)
            {
                finalVtecEventList = (List) vtecMap.get(lid);
            }
        }
        return finalVtecEventList;
    }

    protected String getEquivalentHeightPE(String pe)
    {
        String heightPE = null;
        if (pe.equalsIgnoreCase("QR"))
            heightPE = "HG";
        else if (pe.equalsIgnoreCase("QT"))
            heightPE = "HT";
        return heightPE;
    }

    protected String getEquivalentDischargePE(String pe)
    {
        String dischargePE = null;
        if (pe.equalsIgnoreCase("HG"))
            dischargePE = "QR";
        else if (pe.equalsIgnoreCase("HT"))
            dischargePE = "QT";
        return dischargePE;
    }

    private void readValidHeightAndDischargePE()
    {
        List shefPEInfoList = null;

        ShefPeTable shefPeTable = new ShefPeTable(_db);
        String whereClause = null;
        String header = "RiverMonitorDataManager.readValidHeightAndDischargePE(): ";

        CodeTimer timer = new CodeTimer();
        try
        {
            whereClause = "where pe like 'H%' or pe like 'Q%'";
            timer.start();
            shefPEInfoList = shefPeTable.select(whereClause);
            timer.stop("Read from shefpe time elapsed:");
            _logger.log(header + " Where Clause:" + whereClause);
            if (shefPEInfoList == null)
            {
                _logger.log(header + " height and discharge pe list is null");
            }
            else
            {
                if (shefPEInfoList.size() > 0)
                {
                    _validHeightAndDischargePEList = new ArrayList();
                    for (int i = 0; i < shefPEInfoList.size(); i++)
                    {

                        ShefPeRecord record = (ShefPeRecord) shefPEInfoList.get(i);
                        _validHeightAndDischargePEList.add(record.getPe());
                    }
                }
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
    }


    private boolean validatePE(String pe)
    {
        boolean result = false;

        if (_validHeightAndDischargePEList != null)
        {
            for (int i = 0; i < _validHeightAndDischargePEList.size(); i++)
            {
                String tempStr = (String) _validHeightAndDischargePEList.get(i);
                if (pe.equals(tempStr))
                {
                    result = true;
                    break;
                }
            }
        }
        return result;
    }

    private void readConfigFileToFormLidHeightPEAndLidDischargePEMap()
    {
        Map lidHeightPEMap = null;
        Map lidDischargePEMap = null;

        String header = "RiverMonitorDataManager.readConfigFileAndFormConfigMap(): ";
        File fileHandler = new File(_peConfigFileName);

        if (fileHandler.exists())
        {
            BufferedReader in = null;
            String str;
            List stringList = new ArrayList();

            try
            {
                in = new BufferedReader(new FileReader(fileHandler));
                while ((str = in.readLine()) != null)
                {
                    stringList.add(str);
                }
            }
            catch (IOException exception)
            {
                printFileException(exception, header);
            }
            if (stringList.size() > 0)
            {
                for (int i = 0; i < stringList.size(); i++)
                {
                    String tempString = (String) stringList.get(i);
                    if (tempString.length() == 0)
                        continue;
                    if (tempString.charAt(0) == '#')
                        continue;
                    String splitStr[] = null;
                    splitStr = tempString.split("\\|");
                    if (splitStr == null)
                        continue;
                    String lid, heightPE, dischargePE;
                    if (splitStr.length == 3)
                    {
                        splitStr[0] = splitStr[0].trim();
                        splitStr[1] = splitStr[1].trim();
                        splitStr[2] = splitStr[2].trim();
                        if (splitStr[0].length() > 0 && splitStr[1].length() == 2 && splitStr[2].length() == 2)
                        {
                            lid = splitStr[0];
                            heightPE = splitStr[1];
                            dischargePE = splitStr[2];
                            if (heightPE.charAt(0) != 'H' || dischargePE.charAt(0) != 'Q')
                            {
                                _logger.log(header + "Invalid height and discharge PE [" + heightPE + "|" + dischargePE
                                    + "]");
                                continue;
                            }
                            if ((!validatePE(heightPE)) || (!validatePE(dischargePE)))
                            {
                                _logger.log(header + "Invalid height and discharge PE [" + heightPE + "|" + dischargePE
                                    + "]");
                                continue;
                            }
                            else
                            {
                                if (lidHeightPEMap == null)
                                {
                                    lidHeightPEMap = new HashMap();
                                }
                                lidHeightPEMap.put(lid, heightPE);
                                if (lidDischargePEMap == null)
                                {
                                    lidDischargePEMap = new HashMap();
                                }
                                lidDischargePEMap.put(lid, dischargePE);
                            }

                        }
                        else
                            // invalid entry
                        {
                            _logger.log(header + "1. Invalid entry[" + tempString + "] in ConfigFile ");
                            continue;
                        }

                    }
                    else
                        // line not having the format lid|PETS|PETS
                    {
                        _logger.log(header + "2. Invalid entry[" + tempString + "] in ConfigFile ");
                        continue;
                    }
                }
            }
        }
        else
        {
            _logger.log(header + "ConfigFile [" + _peConfigFileName + "] doesn't exist");
        }

        _lidHeightPEMap = lidHeightPEMap;
        _lidDischargePEMap = lidDischargePEMap;
    }

    protected void printFileException(IOException exception, String header)
    {
        header = header+".printFileException(): ";
        _logger.log(header+"ERROR = " + exception.getMessage());
        exception.printStackTrace(_logger.getPrintWriter());
        _logger.log(header+"End of stack trace");
    }
//  ---------------------------------------------------------------------------------------------------

    private double roundTo2DecimalPlaces(double number)
    {
        double result = DbTable.getNullDouble();
        if (number != DbTable.getNullDouble())
            result = MathHelper.roundToNDecimalPlaces(number, 2);

        return result;
    }

    public String getDefaultHsa()
    {
        return _defaultHsa;
    }

    public Map getLidDescDetails()
    {
        if (_lidDescDetailsMap == null)
        {
            LocRiverMonView riverMonView = new LocRiverMonView(_db);
            try
            {
                List riverMonViewList = (List) riverMonView.select("");
                if (riverMonViewList != null)
                {
                    if (riverMonViewList.size() > 0)
                    {
                        _lidDescDetailsMap = new HashMap<String, String>();
                        for (int i = 0; i < riverMonViewList.size(); i++)
                        {
                            LocRiverMonRecord record = (LocRiverMonRecord) riverMonViewList.get(i);
                            String name = record.getName();
                            if (name != null)
                            {
                                if (name.length() > 0)
                                {
                                    if (name.length() > 50)
                                    {
                                        name = name.substring(0, 50);
                                    }
                                }
                            }
                            String desc = name + ";" + record.getState() + ";" + record.getCounty();
                            _lidDescDetailsMap.put(record.getLid(), desc);
                        }
                    }
                }
            }
            catch (SQLException e)
            {
                logSQLException(e);
            }
        }
        return _lidDescDetailsMap;
    }

    private void setAlertAlarmDetails(RiverMonitorJTableRowData rowData, Map<String, AlertAlarmValRecord> alertAlarmMap)
    {
        String lid = rowData.getLid();
        AlertAlarmValRecord alertAlarmValRecord = (AlertAlarmValRecord) alertAlarmMap.get(lid);
        if(alertAlarmValRecord != null)
        {
            if (alertAlarmValRecord.getPe().equals(rowData.getPrimaryRiverPe())
                    && alertAlarmValRecord.getLid().equals(rowData.getLid()))
            {
                String categ = alertAlarmValRecord.getAa_categ();
                String check = alertAlarmValRecord.getAa_check();

                if ((check.equalsIgnoreCase("Upper")) && categ.equalsIgnoreCase("alert"))
                {
                    rowData.setAlertUpperLimit(true);
                }
                else if ((check.equalsIgnoreCase("Upper")) && categ.equalsIgnoreCase("alarm"))
                {
                    rowData.setAlarmUpperLimit(true);
                }
                else if ((check.equalsIgnoreCase("Lower")) && categ.equalsIgnoreCase("alert"))
                {
                    rowData.setAlertLowerLimit(true);
                }
                else if ((check.equalsIgnoreCase("Lower")) && categ.equalsIgnoreCase("alarm"))
                {
                    rowData.setAlarmLowerLimit(true);
                }
                else if ((check.equalsIgnoreCase("Diff")) && categ.equalsIgnoreCase("alert"))
                {
                    rowData.setAlertDiffLimit(true);
                }
                else if ((check.equalsIgnoreCase("Diff")) && categ.equalsIgnoreCase("alarm"))
                {
                    rowData.setAlarmDiffLimit(true);
                }
                else if ((check.equalsIgnoreCase("Roc")) && categ.equalsIgnoreCase("alert"))
                {
                    rowData.setAlertRocLimit(true);
                }
                else if ((check.equalsIgnoreCase("Roc")) && categ.equalsIgnoreCase("alarm"))
                {
                    rowData.setAlarmRocLimit(true);
                }
            }
        }
    }

    private Map createAlertAlarmInfoToLidMap()
    {
        String header = "RiverMonitorDataManager.addAlertAlarmInfoToRowDataList(): ";
        List alertAlarmList;
        Map<String, AlertAlarmValRecord> alertAlarmMap = new HashMap<String, AlertAlarmValRecord>();
        AlertAlarmValTable alertAlarmValTable = new AlertAlarmValTable(_db);
        int numOfHrsForAlertAlarmValidTimeLookBack = 1;
        numOfHrsForAlertAlarmValidTimeLookBack = _menuSettings.getRiverSettings().getAlertAlarmLookBack();

        _logger.log(header + "numOfHrsForValidTimeLookBack:" + numOfHrsForAlertAlarmValidTimeLookBack);

        String whereClause = null;
        long tempTime = System.currentTimeMillis() - MILLIS_PER_HOUR * numOfHrsForAlertAlarmValidTimeLookBack;
        String validTime = DbTimeHelper.getDateTimeStringFromLongTime(tempTime);

        try
        {
            whereClause = " where validtime >" + "'" + validTime + "'" + " order by lid, pe";
            _logger.log(header + "Where clause:" + whereClause);

            alertAlarmList = alertAlarmValTable.select(whereClause);

            for (int i = 0; i < alertAlarmList.size(); i++)
            {
                AlertAlarmValRecord alertAlarmValRecord = (AlertAlarmValRecord) alertAlarmList.get(i);
                alertAlarmMap.put(alertAlarmValRecord.getLid(), alertAlarmValRecord);
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
        
        return alertAlarmMap;
    }

//  -----------------------------------------------------------------------------------------------------------------------

    private SshpFcstDetails getSshpFcstDetailsForLid(String lid, double floodStage)
    {

        if (_sshpLidFcstMap == null)
            determineLidToSshpFcstMap();
        SshpFcstDetails sshpFcstDetails = new SshpFcstDetails();
        RegularTimeSeries fcstRegularTimeSeries = (RegularTimeSeries) _sshpLidFcstMap.get(lid);
        if (fcstRegularTimeSeries != null)
        {
            AbsTimeMeasurement maxMeasurement = (AbsTimeMeasurement) fcstRegularTimeSeries.getMaxMeasurement();
            if(maxMeasurement != null)
            {
                sshpFcstDetails.setCrestValue(roundTo2DecimalPlaces(maxMeasurement.getValue()));
                sshpFcstDetails.setCrestTime(maxMeasurement.getTime());

                for (int index = 0; index < fcstRegularTimeSeries.getMeasurementCount(); index++)
                {
                    AbsTimeMeasurement forecastMeasurement = fcstRegularTimeSeries.getAbsTimeMeasurementByIndex(index);

                    if (forecastMeasurement.getValue() >= floodStage)
                    {
                        sshpFcstDetails.setTimeAboveFldStg(fcstRegularTimeSeries.getMeasurementTimeByIndex(index));
                        break;
                    }
                }
                // basis time will be the last modified time of the file
                // basinid_fcst_stage.dat
                sshpFcstDetails.setBasisTime(new File(_sshpFcstDataDir + "/" + lid + "_fcst_stage.dat").lastModified());
            }

        }
        return sshpFcstDetails;
    }

//  ---------------------------------------------------------------------------------------------------------------
    private RegularTimeSeries loadForecastFileMapTimeSeries(String filePath)
    {
        TimeSeriesFileManager reader = new TimeSeriesFileManager();

        RegularTimeSeries timeSeries = null;

        timeSeries = reader.readRegularTimeSeries(filePath, MeasuringUnit.inches);

        return timeSeries;
    }

//  -----------------------------------------------------------------------------------------------------------------------

    private void determineLidToSshpFcstMap()
    {
        _sshpLidFcstMap = new HashMap();

        File sshpFcstDataPath = new File(_sshpFcstDataDir);

        if (!sshpFcstDataPath.exists())
        {
            _logger.log("Sshp fcst data dir:[" + _sshpFcstDataDir
                + "] doesn't exist...Check the token value for sshp_background_forecast_output_dir");
        }
        else
        {
            File[] fcstFilesArray = sshpFcstDataPath.listFiles();

            if (fcstFilesArray != null)
            {
                if (fcstFilesArray.length > 0)
                {
                    // basis time look back window filter value
                    int numOfHrsForLatestFcstLookBack = _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack();
                    long lookBackWindowTimeLong = System.currentTimeMillis() - MILLIS_PER_HOUR
                    * numOfHrsForLatestFcstLookBack;

                    for (int i = 0; i < fcstFilesArray.length; i++)
                    {
                        // if file's last modified time is within the basis time
                        // look back window
                        if (fcstFilesArray[i].lastModified() >= lookBackWindowTimeLong)
                        {
                            String fileName = fcstFilesArray[i].getName();
                            if (fileName.endsWith(".dat"))
                            {
                                RegularTimeSeries fcstRegularTimeSeries = loadForecastFileMapTimeSeries(fcstFilesArray[i]
                                                                                                                       .getAbsolutePath());

                                if (fcstRegularTimeSeries == null)
                                    continue;

                                int indexOfUnderScore = fileName.indexOf('_');
                                String lid = fileName.substring(0, indexOfUnderScore);
                                _sshpLidFcstMap.put(lid, fcstRegularTimeSeries);

                                // System.out.println("Read the sshp forecast
                                // file["+ fcstFilesArray[i].getAbsolutePath()
                                // +"] and added the time series to the map");
                            }
                        }
                    }
                }
                else
                {
                    _logger.log("Sshp fcst files are not available in the sshpfcst dir:[" + _sshpFcstDataDir + "]");
                }
            }
        }
    }

//  -----------------------------------------------------------------------------------------------------------------------

    private RiverStatusRecord determineObsOrFcstRecordFromRiverStatus(String lid, String primaryPe, String tag,
            String fcstTs)
    {
        List riverStatusRecordList = new ArrayList();
        RiverStatusTable riverStatusTable = new RiverStatusTable(_db);
        RiverStatusRecord riverStatusRecord = null;
        Map tsRankMap = null;
        List tsDurExtValTimeList = new ArrayList();
        String whereClause = null;
        
        if (tag.compareTo("OBS") == 0)
        {
            whereClause = " where lid = " + "'" + lid + "'" + " and pe='" + primaryPe + "' and (ts like 'R%')"
            + " order by validtime desc";
        }
        else
            // Tag is FCST
        {
            int numOfHrsForBasisTimeLookBack = 1;
            numOfHrsForBasisTimeLookBack = _menuSettings.getRiverSettings().getLatestFcstBasisTimeLookBack();
            
            long allowedBasisTime = System.currentTimeMillis() - (MILLIS_PER_HOUR * numOfHrsForBasisTimeLookBack);

            String allowedBasisTimeString = DbTimeHelper.getDateTimeStringFromLongTime(allowedBasisTime);
            
            if (fcstTs.compareTo("IngestFilter") == 0)
            {
                whereClause = " where lid = " + "'" + lid + "'" + " and pe='" + primaryPe + "' and (ts like 'F%')"
                + " and basistime >= '" + allowedBasisTimeString + "' order by value desc";
            }
            else
                // TS for fcst recs is chosen to be 'FF' or 'FZ'
            {
                whereClause = " where lid = " + "'" + lid + "'" + " and pe='" + primaryPe + "' and ts='" + fcstTs + "'"
                + " and basistime >= '" + allowedBasisTimeString + "'";
            }
        }

        // ("Tag:"+tag +" clause:"+ whereClause);
        try
        {
            riverStatusRecordList = riverStatusTable.select(whereClause);
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

        if (riverStatusRecordList.size() == 0)
            return riverStatusRecord;

        // Key for riverstatus table are lid, pe, ts. So we will get only one
        // record if
        // fcstTs is either FF or FZ .Hence the list size will be 1. So move to
        // else part
        // The below If logic will be used only if fcsTs has value IngestFilter.
        // If more than one rec is found then use ingest filter decide based on
        // ts rank
        // Else return the single record in the list.
        if (riverStatusRecordList.size() > 1)
        {
            for (int loopcnt = 0; loopcnt < riverStatusRecordList.size(); loopcnt++)
            {
                riverStatusRecord = (RiverStatusRecord) riverStatusRecordList.get(loopcnt);
                TsDurExtValTime tsDurExtValTime = new TsDurExtValTime();

                /*
                 * System.out.println("statusrec
                 * has:"+riverStatusRecord.getTs()+"|"+
                 * riverStatusRecord.getDur()+"|"+
                 * riverStatusRecord.getExtremum()+"|"+
                 * riverStatusRecord.getValidtime()+"|"+
                 * riverStatusRecord.getValue());
                 */

                tsDurExtValTime.setTs(riverStatusRecord.getTs());
                tsDurExtValTime.setDuration(riverStatusRecord.getDur());
                tsDurExtValTime.setExtremum(riverStatusRecord.getExtremum());
                tsDurExtValTime.setTime(riverStatusRecord.getValidtime());
                tsDurExtValTime.setValue(riverStatusRecord.getValue());
                tsDurExtValTimeList.add(tsDurExtValTime);

                tsDurExtValTime = (TsDurExtValTime) tsDurExtValTimeList.get(loopcnt);
                /*
                 * System.out.println("After add In list
                 * loopcnt:"+loopcnt+"|"+tsDurExtValTime.getTs()+"|"+tsDurExtValTime.getDuration()+"|"+
                 * tsDurExtValTime.getExtremum());
                 */

            }
            tsRankMap = createIngestFilterTsRankMap(lid, tsDurExtValTimeList, primaryPe);
            riverStatusRecord = compareTsRankAndRiverStatusInfo(tsRankMap, riverStatusRecordList, tsDurExtValTimeList);

        }
        else
        {
            riverStatusRecord = null;
            riverStatusRecord = (RiverStatusRecord) riverStatusRecordList.get(0);
        }
        /*
         * System.out.println("Rec is:"+ riverStatusRecord.getLid()+"|"+
         * riverStatusRecord.getPe()+"|"+riverStatusRecord.getTs()+"|"+
         * riverStatusRecord.getValue());
         */

        return riverStatusRecord;
    }

    private Map createIngestFilterTsRankMap(String lid, List tsDurExtValTimeList, String pe)
    {
        Map tsRankMap = new HashMap();
        IngestFilterTable ingestFilterTable = new IngestFilterTable(_db);
        IngestFilterRecord ingestFilterRecord = null;
        List ingestFilterRecordList = new ArrayList();
        String whereClause = null;
        String ts = null;
        int duration = 0;
        String extremum = null;
        TsDurExtValTime tsDurExt = null;
        Integer curTsRank = null;

        for (int loopcnt = 0; loopcnt < tsDurExtValTimeList.size(); loopcnt++)
        {
            tsDurExt = (TsDurExtValTime) tsDurExtValTimeList.get(loopcnt);
            ts = tsDurExt.getTs();
            duration = tsDurExt.getDuration();
            extremum = tsDurExt.getExtremum();
            // System.out.println("For:ts|dur|ext:"+ts+"|"+duration+"|"+extremum);
            try
            {
                whereClause = " where lid='" + lid + "' and pe='" + pe + "' and ts='" + ts + "' and dur=" + duration
                + " and extremum='" + extremum + "'";

                // System.out.println("ingestfilter whereclause:"+whereClause);
                ingestFilterRecordList = ingestFilterTable.select(whereClause);
                // System.out.println("after ingestfilter
                // whereclause:"+whereClause);
                if (ingestFilterRecordList.size() > 0)
                {
                    ingestFilterRecord = (IngestFilterRecord) ingestFilterRecordList.get(0);
                    curTsRank = new Integer(ingestFilterRecord.getTs_rank());
                }
                else
                {
                    curTsRank = new Integer(-1);
                }
                /*
                 * System.out.println("Before add to map ts|dur|ext|curtsrank: "+
                 * ts+"|"+duration+"|"+extremum+"|"+ curTsRank);
                 */

                tsRankMap.put(new Integer(loopcnt), curTsRank);

                // System.out.println("After Add to rankmap");
            }
            catch (SQLException e)
            {
                logSQLException(e);
            }
        }

        return tsRankMap;
    }

    private RiverStatusRecord compareTsRankAndRiverStatusInfo(Map tsRankMap, List riverStatusRecordList,
            List tsDurExtValTimeList)
    {
        RiverStatusRecord matchRiverStatusRecord = null;
        int prevTsRank = -1;
        int bestTsRank = -1;
        int matchIndex = -1;
        int cntOfSameTsRank = 0;
        List indexOfSameTsRankList = new ArrayList();

        // System.out.println("In compare function");
        for (int loopcnt = 0; loopcnt < riverStatusRecordList.size(); loopcnt++)
        {
            int curTsRank = new Integer(tsRankMap.get(new Integer(loopcnt)).toString()).intValue();
            if (curTsRank == -1)
                continue;
            if (bestTsRank == -1)
            {
                prevTsRank = curTsRank;
                bestTsRank = curTsRank;
                matchIndex = loopcnt;
                // System.out.println("loop:[ "+ loopcnt+"]:"+ bestTsRank+ " "+
                // matchIndex);
            }
            else
            {
                // System.out.println("Comparing curTsRank < bestTsRank" +
                // curTsRank + bestTsRank);
                if (curTsRank < bestTsRank)
                {
                    prevTsRank = curTsRank;
                    bestTsRank = curTsRank;
                    matchIndex = loopcnt;
                    // System.out.println("loop:[ "+ loopcnt+"]:"+ bestTsRank+ "
                    // "+ matchIndex);
                }
                else
                {
                    bestTsRank = prevTsRank;
                    prevTsRank = curTsRank;
                    // System.out.print("False");
                    // System.out.println("loop:[ "+ loopcnt+"]:"+ bestTsRank+ "
                    // "+ matchIndex);
                }
            }
        }

        // Match is found for highest ts_rank. Now check if there are multiple
        // ts with same ts rank.
        // If so consider all the recs in riverstatus that have the same ts rank
        // and decide as below.
        // For obs data consider the rec that has latest valid time.
        // For fcst data consider the rec that has max value.
        if (matchIndex != -1)
        {
            int highestTsRank = bestTsRank;
            int curTsRank;

            // System.out.println("riverstatsrecscnt:"+riverStatusRecordList.size());
            // System.out.println("tsranksize:"+tsRankMap.size());
            for (int loopcnt = 0; loopcnt < tsRankMap.size(); loopcnt++)
            {
                // System.out.println("here3:"+Integer.valueOf(tsRankMap.get(Integer.valueOf(loopcnt)).toString()).intValue());
                curTsRank = new Integer(tsRankMap.get(new Integer(loopcnt)).toString()).intValue();

                if (curTsRank == highestTsRank)
                {
                    cntOfSameTsRank++;
                    indexOfSameTsRankList.add(new Integer(loopcnt));
                }
            }
            if (cntOfSameTsRank == 1)
                matchRiverStatusRecord = (RiverStatusRecord) riverStatusRecordList.get(matchIndex);
        }

        // At this point more than 1 rec is got from riverstatus and
        // either no match is found in ingestfilter and since
        // Or multiple ts having same ts rank then do the following
        if (matchIndex == -1 || cntOfSameTsRank > 1)
        {
            // since we query the riverstatus with valid time desc/ value desc
            // for fcst/obs
            // respectively we will use the
            // first rec that match the highest rank ts amongst the recs
            // obtained
            int index = new Integer(indexOfSameTsRankList.get(0).toString()).intValue();
            matchRiverStatusRecord = (RiverStatusRecord) riverStatusRecordList.get(index);
        }

        /*
         * System.out.println("Final best match:"+
         * matchRiverStatusRecord.getLid()+"|"+
         * matchRiverStatusRecord.getPe()+"|"+ matchRiverStatusRecord.getTs()+
         * "|"+ matchRiverStatusRecord.getExtremum()+
         * "|"+matchRiverStatusRecord.getDur()+
         * "|"+matchRiverStatusRecord.getValue()+"|"+ "Rank: "+bestTsRank);
         */
        return matchRiverStatusRecord;
    }

    private class TsDurExtValTime
    {
        private String ts = null;

        private int duration = 0;

        private String extremum = null;

        private long time;

        private double value;

        protected void setTs(String ts)
        {
            this.ts = ts;
        }

        protected String getTs()
        {
            return this.ts;
        }

        protected void setDuration(int dur)
        {
            this.duration = dur;
        }

        protected int getDuration()
        {
            return this.duration;
        }

        protected void setExtremum(String ext)
        {
            this.extremum = ext;
        }

        protected String getExtremum()
        {
            return this.extremum;
        }

        protected long getTime()
        {
            return time;
        }

        protected void setTime(long time)
        {
            this.time = time;
        }

        protected double getValue()
        {
            return value;
        }

        protected void setValue(double value)
        {
            this.value = value;
        }
    }

    public class SshpFcstDetails
    {
        private double _crestValue;

        private long _crestTime;

        private long _timeAboveFldStg;

        private long _basisTime;

        public SshpFcstDetails()
        {
            _crestValue = DbTable.getNullDouble();
            _crestTime = DbTable.getNullLong();
            _timeAboveFldStg = DbTable.getNullLong();
            _basisTime = DbTable.getNullLong();
        }

        public long getBasisTime()
        {
            return _basisTime;
        }

        public void setBasisTime(long basisTime)
        {
            this._basisTime = basisTime;
        }

        public long getCrestTime()
        {
            return _crestTime;
        }

        public void setCrestTime(long crestTime)
        {
            this._crestTime = crestTime;
        }

        public double getCrestValue()
        {
            return _crestValue;
        }

        public void setCrestValue(double crestValue)
        {
            this._crestValue = crestValue;
        }

        public long getTimeAboveFldStg()
        {
            return _timeAboveFldStg;
        }

        public void setTimeAboveFldStg(long timeAboveFldStg)
        {
            this._timeAboveFldStg = timeAboveFldStg;
        }
    }


}
