package ohd.hseb.monitor.precip;

import java.awt.Color;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.bsf.BSFEngine;
import org.apache.bsf.BSFManager;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.AdminRecord;
import ohd.hseb.ihfsdb.generated.AdminTable;
import ohd.hseb.ihfsdb.generated.LocRiverMonRecord;
import ohd.hseb.ihfsdb.generated.LocRiverMonView;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.officenotes.OfficeNotesDataManager;
import ohd.hseb.rivermonlocgroup.RiverMonLocGroupDataManager;
import ohd.hseb.monitor.LocationDataFilter;
import ohd.hseb.monitor.TreeFilterManager;
import ohd.hseb.monitor.derivedcolumns.DerivedColumn;
import ohd.hseb.monitor.derivedcolumns.DerivedColumnsFileManager;
import ohd.hseb.monitor.precip.settings.PrecipMonitorMenuSettings;
import ohd.hseb.monitor.precip.settings.PrecipColumnDataSettings;
import ohd.hseb.monitor.util.HydroappsDefaultDirs;
import ohd.hseb.util.AppsDefaults;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.MathHelper;
import ohd.hseb.util.SessionLogger;

public class PrecipMonitorDataManager
{

    private Database _db;

    private AppsDefaults _appsDefaults;

    private SessionLogger _logger;

    private String _defaultHsa;

    private Set<String> _precipLidSet = null;

    private DerivedColumnsFileManager _derivedColumnsFileManager;
    private List _derivedColumnsList;
    private BSFManager _bsfManager;
    private BSFEngine _beanShellEngine;

    private String _missingRepresentation;
 
    private List<PrecipMonitorJTableRowData> _precipMonitorRowDataList;
    private List<PrecipMonitorJTableRowData> _missingPrecipMonitorRowDataList = new ArrayList<PrecipMonitorJTableRowData>();

    private Map<String, String> _lidDescDetailsMap;
    private Map<String, String> _lidToRiverBasinMap = new HashMap<String, String>();
    private TreeFilterManager _treeFilterManager;

    private String _lidOfCurrentInterest;

    private PrecipMonitorMenuSettings _menuSettings;

    private RiverMonLocGroupDataManager _riverMonLocGroupDataManager;
    private OfficeNotesDataManager _officeNotesDataManager;
    private PrecipDataManager _precipDataManager;

    public PrecipMonitorDataManager(Database db, AppsDefaults appsDefaults, String missingRepresentation, 
            SessionLogger logger, PrecipMonitorMenuSettings menuSettings,
            DerivedColumnsFileManager derivedColumnsFileManager)
    {
        _appsDefaults = appsDefaults;
        _menuSettings = menuSettings;

        _db = db;
        _missingRepresentation = missingRepresentation;
        _logger = logger;
        
        _precipDataManager = new PrecipDataManager(_db, _appsDefaults, _missingRepresentation, _logger);
        readAdminTable();

        _riverMonLocGroupDataManager = new RiverMonLocGroupDataManager(db, _logger, missingRepresentation, getDefaultHsa());

        _derivedColumnsFileManager = derivedColumnsFileManager; 

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
        String header = "PrecipMonitorDataManager.getDisplayedRecordCount(): ";
        printLocationCounts(header + "_precipMonitorRowDataList ");

        
      //  System.out.println(header + " _precipMonitorRowDataList count = " + _precipMonitorRowDataList.size());
        
        List selectedRowDataList = getTreeFilterManager().filter(_precipMonitorRowDataList);
        long displayedCount = selectedRowDataList.size();
        
     //   printLocationCounts(header + " after filter applied ");

       // System.out.println(header + " selectedRowDataList count = " + displayedCount);

        return displayedCount;

    }
    
    public void disconnect()
    {
        _db.disconnect();
    }

    public boolean  getShowMissing()
    {
        return _menuSettings.shouldShowMissingPrecip();
    }
    
    public PrecipColumnDataSettings getPrecipSettings()
    {
        return _menuSettings.getPrecipSettings();
    }

    private void printLocationCounts(String header)
    {
        System.out.println(header + "There are "  + _precipMonitorRowDataList.size() + 
                " displayed with " + _missingPrecipMonitorRowDataList.size() + " non-displayed missing locations." );

        writeLidsToFile();
        
    }
    
    private void writeLidsToFile()
    {
        FileWriter writer = null;
    	String whfsBinDir =
    		this._appsDefaults.getToken("whfs_bin_dir", 
    			HydroappsDefaultDirs.WHFS_BIN_DIR);
        
        try
        {
            writer = new FileWriter(
            	whfsBinDir + "/PrecipMonitorLocations.txt");
            for (PrecipMonitorJTableRowData row : _precipMonitorRowDataList)
            {
                writer.write(row.getLid() + "\n");
            }
            
            writer.close();
        }

        catch (IOException e)
        {
            e.printStackTrace();
        }
        
    }
    
    public Map<String, Color> getLidToCriticalColorMap()
    {
        String header = "PrecipMonitorDataManager.getLidToCriticalColorMap(): ";
        
        Map<String, Color> lidToCriticalColorMap = new HashMap<String, Color>();
        for(PrecipMonitorJTableRowData rowData : _precipMonitorRowDataList)
        {
            String lid = rowData.getLid();
            Color criticalColor = rowData.getCriticalColorForThisRow();
            lidToCriticalColorMap.put(lid, criticalColor);
        }
        
        for(PrecipMonitorJTableRowData rowData : _missingPrecipMonitorRowDataList)
        {
            String lid = rowData.getLid();
            Color criticalColor = rowData.getCriticalColorForThisRow();
            lidToCriticalColorMap.put(lid, criticalColor);
        }
        
        printLocationCounts(header);
        
        return lidToCriticalColorMap;
    }

    public List<String> getLidList()
    {
        String header = "PrecipMonitorDataManager.getLidList(): ";
        List<String> lidList = new ArrayList<String>();
        for(PrecipMonitorJTableRowData rowData: _precipMonitorRowDataList)
        {
            String lid = rowData.getLid();
            lidList.add(lid);
        }
        
        System.out.println(header + "_precipMonitorRowDataList.size() =  " + _precipMonitorRowDataList.size());
        
        return lidList;
    }

    
    private List getFullListOfLocationIds()
    {
        List<String> locationIdList = new ArrayList<String>();
        
        for (PrecipMonitorJTableRowData rowData : _precipMonitorRowDataList)
        {
            locationIdList.add(rowData.getLid());
        }
        
        return locationIdList;
        
    }

//  ---------------------------------------------------------------------------------------------------

    
    private LocationDataFilter getLocationDataFilter()
    {
        PrecipLocationDataFilter locationDataFilter;
        List<String> lidList = getFullListOfLocationIds();
        Map<String, Boolean> locationDisplayMap = new HashMap<String, Boolean>();
      
        for(String lid:lidList)
        {
            locationDisplayMap.put(lid, false);
        }

        locationDataFilter = new PrecipLocationDataFilter(locationDisplayMap, _menuSettings.getShowMissingPrecipBooleanHolder());
        return locationDataFilter;
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

    protected void readCurrentRiverMonTableData()
    {
        /**
         * Delegate to the RiverMonLocGroupDataManager the initialization of the location and group definitions and ordering.
         */
        _riverMonLocGroupDataManager.initializeRiverMonTableData();

    }

    /**
     * Reads the location table and creates the list of PrecipMonitorJtableRowdata objects for each location
     * and fills in its name, state , county info
     * @return
     */
    private List<PrecipMonitorJTableRowData> readPrecipLocationInfo()
    {
        if(_precipLidSet == null)
            _precipLidSet = new HashSet<String> ();
        else
            _precipLidSet.clear();

        if(_precipMonitorRowDataList != null)
        {
            for(PrecipMonitorJTableRowData precipMonitorRowData: _precipMonitorRowDataList)
            {
                precipMonitorRowData.resetCellMap();
            }
            _precipMonitorRowDataList.clear();
        }
        
        Runtime.getRuntime().gc();
        List<PrecipMonitorJTableRowData> precipMonitorRowDataList = new ArrayList<PrecipMonitorJTableRowData>();

        LocationTable locationTable = new LocationTable(_db);

        List<LocationRecord> locationRecordList;

        loadRiverBasinData();
        
        try
        {
            String whereClause = "where lid in (select distinct(lid) from ingestfilter where pe in('PC', 'PP') and ingest='T')";
            locationRecordList = locationTable.select(whereClause);
            if(locationRecordList != null)
            {
                for(LocationRecord locationRecord : locationRecordList)
                {
                    PrecipMonitorJTableRowData precipMonitorRowData = new PrecipMonitorJTableRowData(_missingRepresentation, this, _logger);
              
                    precipMonitorRowData.setLid(locationRecord.getLid());
                    precipMonitorRowData.setName(locationRecord.getName());
                    precipMonitorRowData.setCounty(locationRecord.getCounty());
                    precipMonitorRowData.setState(locationRecord.getState());
                    precipMonitorRowData.setHsa(locationRecord.getHsa());
                    
                     setRiverBasinData(precipMonitorRowData);
           
                     precipMonitorRowDataList.add(precipMonitorRowData);
                    _precipLidSet.add(locationRecord.getLid());
                }
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        return precipMonitorRowDataList;
    }

    public List<PrecipMonitorJTableRowData> getPrecipMonitorRowDataList()
    {
        return _precipMonitorRowDataList;
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
    
    private void setRiverBasinData(PrecipMonitorJTableRowData rowData)
    {
         String riverBasin = _lidToRiverBasinMap.get(rowData.getLid());
        
         rowData.setRiverBasin(riverBasin);
         
         return;
    }
    
    
    public boolean isPrecipLocation(String lid)
    {
        boolean result = false;

        if(_precipLidSet != null)
        {
            if(_precipLidSet.contains(lid))
            {
                result = true;
            }
        }
        return result;
    }

    public void alterPrecipMonitorRowDataListBasedOnMissingPrecipFilter()
    {
        
        _missingPrecipMonitorRowDataList.clear();
        
   //     _fullPrecipMonitorRowDataList = _precipMonitorRowDataList;
        
        if(! _menuSettings.shouldShowMissingPrecip() ) // show missing precip is set to false(ie., skip missing precip)
        {
            System.out.println("Show missing is false");
            List<PrecipMonitorJTableRowData> finalPrecipMonitorRowDataList = new ArrayList<PrecipMonitorJTableRowData>();

            for(PrecipMonitorJTableRowData precipMonitorRowData: _precipMonitorRowDataList)
            {
                PrecipData precipData = precipMonitorRowData.getPrecipData();

                // precip is available add this lid
                if( precipData.isDataAvailable() )    
                {
                    finalPrecipMonitorRowDataList.add(precipMonitorRowData);
                }
                else
                {
                    _missingPrecipMonitorRowDataList.add(precipMonitorRowData);
                }
            }
            _precipMonitorRowDataList = finalPrecipMonitorRowDataList;
            finalPrecipMonitorRowDataList = null;
            Runtime.getRuntime().gc();
        }
    }
    
    public void createPrecipMonitorRowDataList()
    {
        
        CodeTimer methodTimer = new CodeTimer();
        methodTimer.start();
        CodeTimer timer = new CodeTimer();
        
        
        String header = "PrecipMonitorDataManager.createPrecipMonitorRowDataList(): ";

        _logger.log(header +" Memory Info before creating rowdata list:" + printMemoryInfo());
        Runtime.getRuntime().gc();
        
        timer.start();
        _precipMonitorRowDataList = readPrecipLocationInfo();
        timer.stop(header + "readPrecipLocationInfo took: ");
        
        if(_precipMonitorRowDataList != null)
        {
            timer.start();
            readCurrentRiverMonTableData();
            timer.stop(header + "readCurrentRiverMonTableData took: ");
            
            _logger.log(header +" 1:" + printMemoryInfo());
            Runtime.getRuntime().gc();
            
            timer.start();
            _precipDataManager.readPrecipData(_precipMonitorRowDataList, "PRECIP");
            timer.stop(header + "readPrecipData took: ");
            
            Runtime.getRuntime().gc();
            _logger.log(header +" 2:" + printMemoryInfo());
            _derivedColumnsList = _derivedColumnsFileManager.getDerivedColumns();
            
            timer.start();
            
            for(PrecipMonitorJTableRowData precipMonitorRowData: _precipMonitorRowDataList)
            {
                String groupId = _riverMonLocGroupDataManager.getGroupId(precipMonitorRowData.getLid());
                String groupName = _riverMonLocGroupDataManager.getGroupName(groupId);
                int groupOrdinal = _riverMonLocGroupDataManager.getGroupOrdinal(groupId);
                //String hsa = _riverMonLocGroupDataManager.getHsa(groupId);
                int locationOrdinal = _riverMonLocGroupDataManager.getLocationOrdinal(precipMonitorRowData.getLid());

             //   precipMonitorRowData.setLid(precipMonitorRowData.getLid());
                precipMonitorRowData.setLocationOrdinal(locationOrdinal);
               // precipMonitorRowData.setHsa(hsa);
                precipMonitorRowData.setGroupName(groupName);
                precipMonitorRowData.setGroupId(groupId);
                precipMonitorRowData.setGroupOrdinal(groupOrdinal);

                /*     _logger.log(precipMonitorRowData.getLid() + "|" + precipMonitorRowData.getName() + "|"
                    + precipMonitorRowData.getGroupId() + "|" + precipMonitorRowData.getGroupName() + "|"
                    + precipMonitorRowData.getLocationOrdinal() + "|"
                    + precipMonitorRowData.getGroupOrdinal() );*/


                precipMonitorRowData.addAllCellsToMap();

                getDerivedColumnsInfo(precipMonitorRowData);
                
                precipMonitorRowData.addDerivedDataToCellMap();
               
             
            }
            
            int count = _precipMonitorRowDataList.size();
            
            timer.stop(header + " for loop with " + count + " iterations took: ");
            
            _logger.log(header + "Before missing precip filter is applied, alllocationinfo list size:" + _precipMonitorRowDataList.size());
                   
            //timer.start();
           // alterPrecipMonitorRowDataListBasedOnMissingPrecipFilter();
           // timer.stop(header + "alterPrecipMonitorRowDataListBasedOnMissingPrecipFilter took: ");
            
            _logger.log(header + "After missing precip filter is applied, alllocationinfo list size:" + _precipMonitorRowDataList.size());
        }
        Runtime.getRuntime().gc();
        _logger.log(header +" Memory Info after creating rowdata list:" + printMemoryInfo());

        methodTimer.stop(header + " the whole method took: ");
        
    }

//  ---------------------------------------------------------------------------------------------------
    
    public String printMemoryInfo()
    {
        String str = "Free:" + Runtime.getRuntime().freeMemory()+ " Max:"+ Runtime.getRuntime().maxMemory()+ " total:"+ Runtime.getRuntime().totalMemory();
        return str;
    }

    private double roundTo2DecimalPlaces(double number)
    {
        double result = DbTable.getNullDouble();
        if (number != DbTable.getNullDouble())
            result = MathHelper.roundToNDecimalPlaces(number, 2);

        return result;
    }

//  ---------------------------------------------------------------------------------------------------

    private void initializeBSF() throws Exception
    {
        _bsfManager = new BSFManager();
        BSFManager.registerScriptingEngine("beanshell", "bsh.util.BeanShellBSFEngine", null);
        _beanShellEngine = _bsfManager.loadScriptingEngine("beanshell");
    }
//  ---------------------------------------------------------------------------------------------------

    protected void getDerivedColumnsInfo(PrecipMonitorJTableRowData precipMonitorRowData)
    {
        String header = "PrecipMonitorDataManger.getDerivedColumnsInfo():" ;
        
        if(_derivedColumnsList != null)
        {
            try
            {
                _bsfManager.declareBean("row", precipMonitorRowData, PrecipMonitorJTableRowData.class);
                _bsfManager.declareBean("precip", precipMonitorRowData.getPrecipData(), PrecipData.class);
                
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
        precipMonitorRowData.setDerivedColumnsDetailsList(_derivedColumnsList);
    }
 
//  ---------------------------------------------------------------------------------------------------
    public List createStringArrayListOfFilterItems()
    {
        String header = "PrecipMonitorDataManager.createStringArrayListOfFilterItems(): ";

        List<String> locationIdList = getFullListOfLocationIds();

        List<String[]> listOfNodePathStringArray = 
            _riverMonLocGroupDataManager.createStringArrayListOfFilterItems(locationIdList);

        return listOfNodePathStringArray;
    }

//  ---------------------------------------------------------------------------------------------------

    public String getDefaultHsa()
    {
        return _defaultHsa;
    }


    protected void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " + exception.getErrorCode() + " " + exception.getMessage());

        exception.printStackTrace(_logger.getPrintWriter());

        _logger.log("End of stack trace");

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
}
