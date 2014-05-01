package ohd.hseb.rivermonlocgroup;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantReadWriteLock.ReadLock;

import javax.swing.JOptionPane;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.LocRiverMonRecord;
import ohd.hseb.ihfsdb.generated.LocRiverMonView;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.ihfsdb.generated.RiverMonGroupRecord;
import ohd.hseb.ihfsdb.generated.RiverMonGroupTable;
import ohd.hseb.ihfsdb.generated.RiverMonLocationRecord;
import ohd.hseb.ihfsdb.generated.RiverMonLocationTable;
import ohd.hseb.rivermonlocgroup.RiverMonLocationJTableRowData;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.jtable.JTableRowData;

public class RiverMonLocGroupDataManager 
{
    private Database _db ;
    private SessionLogger _logger;
    private String _missingRepresentation; 

    private Map<String, RiverMonLocationRecord> _lidToRiverMonLocationMap;
    private Map<String, List<String>> _groupIdToLocationIdListMap = new HashMap<String, List<String>>();
    private List _locationInfoList;
    private String _defaultHsa;
    private Map<String, Integer> _locationIdToOrdinalMap;

    private Map _groupIdToRiverMonGroupMap = null;
    private List _groupInfoList = null;

    private Map _groupHsaMap = null;

    private Set _hsaInfoSet = null;
    private Map _locationHsaMap = null;

    private List<String> _locationsUnderDefaultGroupList = null;
    private int  _locationOrdinalForDefaultLocations = 1;

    private final String _defaultGroupId = "DEFAULT";

    public RiverMonLocGroupDataManager(Database dbHandler, SessionLogger logger, 
            String missingRepresentation, String defaultHsa)
    {
        super();
        _db = dbHandler;
        _logger = logger;
        _missingRepresentation = missingRepresentation;
        _defaultHsa = defaultHsa;

        if (_db == null)
        {
            JOptionPane.showMessageDialog(null, "Database should be provided","RiverMonLocation Application", JOptionPane.PLAIN_MESSAGE);
            System.exit(0);
        }
    }
    
    public void initializeRiverMonTableData()
    {
        String header = "RiverMonLocGroupDataManager.initializeRiverMonTableData()";
        
        initLocationHsaMap();
        initRiverMonGroupMap();       
        initRiverMonLocationMap();
        initGroupHsaMap();
         
        //any undefined default hsa-based groups will be created here
        //any defined hsa-based groups groups will be overwritten in the following for loop
        initAllDefaultGroupsToMap();
        
        //updateRiverMonitorGroupWithAppropriateHsa();
    }
    
    public List createStringArrayListOfFilterItems( List<String> locationIdList)
    {
        return createStringArrayListOfFilterItemsNew(locationIdList);
    }
    
    public List createStringArrayListOfFilterItemsNew( List<String> locationIdList)
    {
        //This version of createStringArrayListOfFilterItems() does not create empty groups and HSAs
        String header =  "RiverMonLocGroupDataManager.createStringArrayListOfFilterItems(): ";
        List<String[]> listOfStringArray = new ArrayList<String[]>();

        String rootItem = "HSA";
        String hsaArray[] = getAllHsa();
        String[] nodePathStringArray;

        buildGroupIdToLocationIdListMap(locationIdList);
        LocationOrdinalComparator locationComparator = new LocationOrdinalComparator();

        if(hsaArray != null && hsaArray.length > 0)
        {
            String hsa = null;
            for(int i=0; i < hsaArray.length; i++)
            {
                hsa = hsaArray[i];
                String groupIdArray[] = getGroupsForHsa(hsaArray[i]);
                if(groupIdArray != null && groupIdArray.length > 0)
                {
                    String groupName = null;
                    for(int j=0; j < groupIdArray.length; j++)
                    {
                        groupName = getGroupName(groupIdArray[j]);
                        String locationId[] = getAllLocationsForGroup(groupIdArray[j], locationIdList, locationComparator);
                        if(locationId != null && locationId.length > 0)
                        {
                            for(int k=0; k < locationId.length; k++)
                            {
                                nodePathStringArray = new String[4];
                                nodePathStringArray[0] = rootItem;
                                nodePathStringArray[1] = hsa;
                                nodePathStringArray[2] = groupName;
                                nodePathStringArray[3] = locationId[k];
                                listOfStringArray.add(nodePathStringArray);
                            } //for k
                        }
                    } //for j
                }
            } //for i
        }
        else  //it is the root
        {
            System.out.println(header + rootItem + " might be an empty root");
            nodePathStringArray = new String[1];
            nodePathStringArray[0] = rootItem;
            listOfStringArray.add(nodePathStringArray);
        }

        System.out.println(header +  "listOfStringArray.size() = " + listOfStringArray.size());

        return listOfStringArray;
    }
    
    public List createStringArrayListOfFilterItemsOld( List<String> locationIdList)
    {
        String header =  "RiverMonLocGroupDataManager.createStringArrayListOfFilterItemsOrig(): ";
        List<String[]> listOfStringArray = new ArrayList<String[]>();

        String rootItem = "HSA";
        String hsaArray[] = getAllHsa();
        String[] nodePathStringArray;

        buildGroupIdToLocationIdListMap(locationIdList);
        LocationOrdinalComparator locationComparator = new LocationOrdinalComparator();

        if(hsaArray != null && hsaArray.length > 0)
        {
            String hsa = null;
            for(int i=0; i < hsaArray.length; i++)
            {
                hsa = hsaArray[i];
                String groupIdArray[] = getGroupsForHsa(hsaArray[i]);
                if(groupIdArray != null && groupIdArray.length > 0)
                {
                    String groupName = null;
                    for(int j=0; j < groupIdArray.length; j++)
                    {
                        groupName = getGroupName(groupIdArray[j]);
                        String locationId[] = getAllLocationsForGroup(groupIdArray[j], locationIdList, locationComparator);
                        if(locationId != null && locationId.length > 0)
                        {
                            for(int k=0; k < locationId.length; k++)
                            {
                                nodePathStringArray = new String[4];
                                nodePathStringArray[0] = rootItem;
                                nodePathStringArray[1] = hsa;
                                nodePathStringArray[2] = groupName;
                                nodePathStringArray[3] = locationId[k];
                                listOfStringArray.add(nodePathStringArray);
                            }
                        }
                        else //it is a group
                        {
                            System.out.println(header + groupName + " might be an empty group");
                            nodePathStringArray = new String[3];
                            nodePathStringArray[0] = rootItem;
                            nodePathStringArray[1] = hsa;
                            nodePathStringArray[2] = groupName;
                            listOfStringArray.add(nodePathStringArray);
                        }
                    }
                }
                else //it is an hsa
                {
                    System.out.println(header + hsa + " might be an empty hsa");
                    nodePathStringArray = new String[2];
                    nodePathStringArray[0] = rootItem;
                    nodePathStringArray[1] = hsa;
                    listOfStringArray.add(nodePathStringArray);
                }
            }
        }
        else  //it is the root
        {
            System.out.println(header + rootItem + " might be an root");
            nodePathStringArray = new String[1];
            nodePathStringArray[0] = rootItem;
            listOfStringArray.add(nodePathStringArray);
        }

        System.out.println(header +  "listOfStringArray.size() = " + listOfStringArray.size());

        return listOfStringArray;
    }
    
    

    private String getDefaultHsa()
    {
        return _defaultHsa;
    }

    public void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " +
            exception.getErrorCode() +  " " +
            exception.getMessage());

        exception.printStackTrace(_logger.getPrintWriter());

        _logger.log("End of stack trace");

    }

    public int delete(RiverMonLocationRecord rec)
    {
        int ret = -1;
        RiverMonLocationTable riverMonLocationTable = new RiverMonLocationTable(_db);
        try
        {
            ret = riverMonLocationTable.delete(rec);
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        return ret;
    }

    public int updateOrInsert(RiverMonLocationRecord rec)
    {
        int ret = -1;
        RiverMonLocationTable riverMonLocationTable = new RiverMonLocationTable(_db);
        try
        {
            ret = riverMonLocationTable.insertOrUpdate(rec);
            updateOrdinalValuesInRiverMonLocation(rec.getLid(), rec.getGroup_id(), rec.getOrdinal());
        }
        catch(SQLException e)
        { 
            logSQLException(e);
        }
        return ret;
    }

    public void updateOrdinalValuesInRiverMonLocation(String locationId, String groupId, int locationOrdinal)
    {   

        RiverMonLocationTable table = new RiverMonLocationTable(_db);
        List<RiverMonLocationRecord> recordList = null;

        String where = "WHERE group_id = '" + groupId + "' ORDER BY ordinal ASC, lid ASC";

        try
        {
            recordList = table.select(where);

            int ordinalValue = locationOrdinal + 1;
            for (RiverMonLocationRecord oldRecord : recordList)
            {
                //if the location is not the same as that passed in as argument
                if (! oldRecord.getLid().equalsIgnoreCase(locationId))
                {
                    //if the ordinal is equal or higher than that passed in 
                    if (oldRecord.getOrdinal() >= locationOrdinal)
                    {
                        //alter and insert the rest of the records.
                        RiverMonLocationRecord newRecord = new RiverMonLocationRecord(oldRecord);

                        newRecord.setOrdinal(ordinalValue);

                        table.update(oldRecord, newRecord);

                        ordinalValue++;
                    }

                }

            }

        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }

    }

    /**
     * Read loc rivermon view location and precip location ( lids which report PC / PP data and have
     * ingest filter on)
     * @return
     */
    public String[] getAllLid()
    {
        String header = "RiverMonLocationDataManager.getAllLid(): ";
        List<String> lidList = null;
        String lid[] = null;
        List<LocRiverMonRecord> locRiverMonRecordList = null;
        LocRiverMonView rivermonView = new LocRiverMonView(_db);

        LocationTable locationTable = new LocationTable(_db);
        String whereClause = null;

        try
        {
            whereClause = " order by lid ";
            locRiverMonRecordList = rivermonView.select(whereClause);
            _logger.log(header+" Where Clause:"+ whereClause);
            if(locRiverMonRecordList != null)
            {
                lidList = new ArrayList<String>();
                
                for(LocRiverMonRecord locRiverMonRecord: locRiverMonRecordList)
                {
                    lidList.add(locRiverMonRecord.getLid());
                }

                whereClause = "where lid in (select distinct(lid) from ingestfilter where pe in('PC', 'PP') and ingest='T')";
                List<LocationRecord> locationRecordList = locationTable.select(whereClause);
                if(locationRecordList != null)
                {
                   for(LocationRecord locationRecord: locationRecordList)
                    {
                        lidList.add(locationRecord.getLid());
                    }
                }
                
                if(! lidList.isEmpty() )
                {
                    lid = new String[lidList.size()];
                    int i=0;
                    for(String str: lidList)
                    {
                        lid[i++] = str;
                    }
                }
                else
                {
                    _logger.log(header+"read location table for precip lids --- lid list is null");
                }
            }
            else
            {
                _logger.log(header+"read loc rivermon --- lid list is null");
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);         
        }
        
        //Sort the 2 merged lists
        Arrays.sort(lid);
        
        return lid;
    }

    private void initRiverMonLocationMap()
    {
        List riverMonLocationInfoList = null;

        RiverMonLocationTable riverMonLocationTable = new RiverMonLocationTable(_db);
        String whereClause = null;
        String header = "RiverMonitorLocGroupDataManager.initRiverMonLocationMap(): ";

        try
        {
            whereClause = "where group_id not like 'DEFAULT' order by ordinal";
            riverMonLocationInfoList = riverMonLocationTable.select(whereClause);
            _logger.log(header + " Where Clause:" + whereClause);
            if (riverMonLocationInfoList == null)
            {
                _logger.log(header + " RiverMonlocation list is null");
            }
            else
            {
                _lidToRiverMonLocationMap = new HashMap();
                for (int i = 0; i < riverMonLocationInfoList.size(); i++)
                {
                    RiverMonLocationRecord record = (RiverMonLocationRecord) riverMonLocationInfoList.get(i);
                    _lidToRiverMonLocationMap.put(record.getLid(), record);
                }
            }
        }
        catch (SQLException e)
        {
            _logger.log("In " + header);
            logSQLException(e);
        }

        _locationInfoList = riverMonLocationInfoList;
    }

    public String[] getAllGroup()
    {
        String group[] = null;
        List groupList = null;
        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        RiverMonGroupRecord riverMonGroupRecord = null;
        String header = "RiverMonLocationDataManager.getAllGroup(): ";
        String whereClause = null;

        try
        {
            whereClause = " order by group_id ";
            groupList = riverMonGroupTable.select(whereClause);
            _logger.log(header+" Where Clause:"+ whereClause);
            if(groupList != null)
            {
                group = new String[groupList.size()];
                for(int i=0; i < groupList.size(); i++)
                {
                    riverMonGroupRecord = (RiverMonGroupRecord) groupList.get(i);
                    group[i] = riverMonGroupRecord.getGroup_id();
                }
            }
            else
            {
                _logger.log(header+" Group list is null");
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);         
        }
        return group;
    }

    public boolean checkIfRiverMonLocationTableHasRecords()
    {
        boolean result = false;
        List riverMonLocationInfoList = null;

        RiverMonLocationTable riverMonLocationTable = new RiverMonLocationTable(_db);
        String whereClause = null; 
        String header = "RiverMonLocationDataManager.checkIfRiverMonLocationTableHasRecords()";
        try
        {
            whereClause = "";
            riverMonLocationInfoList = riverMonLocationTable.select(whereClause);
            _logger.log(header+" Where Clause:"+ whereClause);
            if(riverMonLocationInfoList != null)
            {
                if(riverMonLocationInfoList.size() > 0)
                    result = true;          
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }

        return result;
    }

    public List<RiverMonLocationJTableRowData> readDataFromRiverMonLocation()
    {
        List<RiverMonLocationRecord> riverMonLocationInfoList = null;
        List<RiverMonLocationJTableRowData> dataList = null;
        
        _locationIdToOrdinalMap = new HashMap<String, Integer>();

        RiverMonLocationTable riverMonLocationTable = new RiverMonLocationTable(_db);
        RiverMonLocationRecord  riverMonLocationRecord = null;
        String whereClause = null; 
        String header = "RiverMonLocationDataManager.readDataFromRiverMonLocation(): ";

        try
        {
            whereClause = "order by lid";
            riverMonLocationInfoList = riverMonLocationTable.select(whereClause);
            _logger.log(header+" Where Clause:"+ whereClause);
            if(riverMonLocationInfoList != null)
            {
                dataList = new ArrayList<RiverMonLocationJTableRowData>();

                for(int i=0;i < riverMonLocationInfoList.size(); i++)
                {
                    riverMonLocationRecord = (RiverMonLocationRecord) riverMonLocationInfoList.get(i);
                    RiverMonLocationJTableRowData rowData = new RiverMonLocationJTableRowData(_missingRepresentation);

                    _locationIdToOrdinalMap.put(riverMonLocationRecord.getLid(), riverMonLocationRecord.getOrdinal());
                    
                    rowData.setLid(riverMonLocationRecord.getLid());
                    rowData.setGroupId(riverMonLocationRecord.getGroup_id());
                    rowData.setLocationOrdinal(riverMonLocationRecord.getOrdinal());

                    rowData.addAllCellsToMap();
                    dataList.add(rowData);
                }
            }
            else
            {
                _logger.log(header+" RiverMonLocation list is null");
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        return dataList;
    }

    private class LocationOrdinalComparator implements Comparator
    {
        public LocationOrdinalComparator()
        {
            if(_locationIdToOrdinalMap == null)
            {
                readDataFromRiverMonLocation();
            }
        }
        
        public int compare(Object obj1, Object obj2)
        {
            String lid1, lid2;
            lid1 = (String) obj1;
            lid2 = (String) obj2;

            Integer ordinal1 = (Integer) _locationIdToOrdinalMap.get(lid1);
            Integer ordinal2 = (Integer) _locationIdToOrdinalMap.get(lid2);
            
            int ret;
            
            if(ordinal1 == null && ordinal2 == null)
            {
              ret = lid1.compareTo(lid2);
            }
            else if (ordinal1 == null)
            {
                return -1; // ordinal2 is greater
            }
            else if (ordinal2 == null)
            {
                return 1; // ordinal1 is greater
            }
            else // both ordinals are not null
            {
                return ordinal1.compareTo(ordinal2);
            }

            return ret;
        }

    }

    public int delete(RiverMonGroupRecord rec)
    {
        int ret = -1;
        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        try
        {
            ret = riverMonGroupTable.delete(rec);
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        return ret;
    }

    public int updateOrInsert(RiverMonGroupRecord rec)
    {
        int ret = -1;
        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        try
        {
            ret = riverMonGroupTable.insertOrUpdate(rec);
            updateOrdinalValuesInRiverMonGroup(rec.getGroup_id(), rec.getHsa(), rec.getOrdinal());
        }
        catch(SQLException e)
        { 
            logSQLException(e);
        }
        return ret;
    }

    public void updateOrdinalValuesInRiverMonGroup(String groupId, String hsa, int groupOrdinal)
    {   

        RiverMonGroupTable table = new RiverMonGroupTable(_db);
        List<RiverMonGroupRecord> recordList = null;

        String where = "WHERE hsa = '" + hsa + "' ORDER BY ordinal ASC, group_id ASC";

        try
        {
            recordList = table.select(where);

            int ordinalValue = groupOrdinal + 1;
            for (RiverMonGroupRecord oldRecord : recordList)
            {
                //if the location is not the same as that passed in as argument
                if (! oldRecord.getGroup_id().equalsIgnoreCase(groupId))
                {
                    //if the ordinal is equal or higher than that passed in 
                    if (oldRecord.getOrdinal() >= groupOrdinal)
                    {
                        //alter and insert the rest of the records.
                        RiverMonGroupRecord newRecord = new RiverMonGroupRecord(oldRecord);

                        newRecord.setOrdinal(ordinalValue);

                        table.update(oldRecord, newRecord);

                        ordinalValue++;
                    }

                }

            }

        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }

    }

    public boolean checkIfRecordExists(RiverMonGroupRecord record)
    {
        boolean exists = false;
        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        String groupId = record.getGroup_id();
        String whereClause = "where group_id="+groupId ;
        List recordList = null;
        try
        {
            recordList = riverMonGroupTable.select(whereClause);
        }
        catch(SQLException e)
        {
            logSQLException(e);   
        }

        if(recordList != null)
        {
            if (recordList.size() > 0)
            {
                exists = true;
            }
        }

        return exists;
    }

    public boolean checkIfRiverMonGroupTableHasRecords()
    {
        boolean result = false;
        List riverMonGroupInfoList = null;

        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        String whereClause = null; 
        String header = "RiverMonGroupDataManager.checkIfRiverMonGroupTableHasRecords()";
        try
        {
            whereClause = "";
            riverMonGroupInfoList = riverMonGroupTable.select(whereClause);
            _logger.log(header+" Where Clause:"+ whereClause);
            if(riverMonGroupInfoList != null)
            {
                if(riverMonGroupInfoList.size() > 0)
                    result = true;          
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }

        return result;
    }

    public List readDataFromRiverMonGroup()
    {
        List riverMonGroupInfoList = null;
        List dataList = null;

        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        RiverMonGroupRecord  riverMonGroupRecord = null;
        String whereClause = null; 
        String header = "RiverMonGroupDataManager.readDataFromRiverMonGroup(): ";

        try
        {
            whereClause = "order by group_id";
            riverMonGroupInfoList = riverMonGroupTable.select(whereClause);
            _logger.log(header+" Where Clause:"+ whereClause);
            if(riverMonGroupInfoList != null)
            {
                dataList = new ArrayList();

                for(int i=0;i < riverMonGroupInfoList.size(); i++)
                {
                    riverMonGroupRecord = (RiverMonGroupRecord) riverMonGroupInfoList.get(i);
                    RiverMonGroupJTableRowData rowData = new RiverMonGroupJTableRowData(_missingRepresentation);

                    rowData.setGroupId(riverMonGroupRecord.getGroup_id());
                    rowData.setGroupName(riverMonGroupRecord.getGroup_name());
                    rowData.setGroupOrdinal(riverMonGroupRecord.getOrdinal());
                    rowData.setHsa(riverMonGroupRecord.getHsa());

                    rowData.addAllCellsToMap();
                    dataList.add(rowData);
                }
            }
            else
            {
                _logger.log(header+" RiverMonGroup list is null");
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        return dataList;
    }

    private void initRiverMonGroupMap()
    {
        List riverMonGroupInfoList = null;

        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        String whereClause = null;
        String header = "RiverMonLocGroupDataManager.initRiverMonGroupMap(): ";

        try
        {
            whereClause = "order by ordinal";
            riverMonGroupInfoList = riverMonGroupTable.select(whereClause);

            _logger.log(header + " Where Clause:" + whereClause);
            if (riverMonGroupInfoList == null)
            {
                _logger.log(header + " RiverMonGroup list is null");
            }
            else
            {
                _groupIdToRiverMonGroupMap = new HashMap();
                
                for (int i = 0; i < riverMonGroupInfoList.size(); i++)
                {
                    RiverMonGroupRecord record = (RiverMonGroupRecord) riverMonGroupInfoList.get(i);
                    _groupIdToRiverMonGroupMap.put(record.getGroup_id(), record);
                }
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }

        _groupInfoList = riverMonGroupInfoList;
    }

    
    public List<String> getAllHsaCustom()
    {
        //new Chip
        List<String> fullHsaList = new ArrayList(_locationHsaMap.values());
        
        Set<String> hsaSet = new HashSet<String>();
        
        for (String hsa : fullHsaList)
        {
            hsaSet.add(hsa);
        }
        
        List<String> hsaList = new ArrayList<String>(hsaSet);
        
        
        return hsaList;
        
    }
    
 
    private void  initAllDefaultGroupsToMap()
    {
        
        Map groupIdToGroupMap = _groupIdToRiverMonGroupMap;
        Map groupToHsaMap = _groupHsaMap;
        
        
        //new Chip
        String header = "RiverMonLocGroupDataManager.initAllDefaultGroupsToMap(): ";
        List<String> hsaList = getAllHsaCustom();
        
        for (String hsa : hsaList)
        {
            String groupId = "DEF_" + hsa;
            String groupName = hsa + " DEFAULT GROUP";
            
           // System.out.println(header + " groupId = " + groupId + "  groupName = " + groupName);
            
            RiverMonGroupRecord group = new RiverMonGroupRecord();
            group.setGroup_id(groupId);
            group.setGroup_name(groupName);
            group.setOrdinal(0);
            
            _groupIdToRiverMonGroupMap.put(groupId, group);
            _groupHsaMap.put(groupId, hsa);
            _groupInfoList.add(group);
            _hsaInfoSet.add(hsa);
        }
    }
    
    
    private void initGroupHsaMap()
    {
        Set hsaInfoSet = new HashSet();
        Map groupHsaMap = null;
        if (_groupInfoList != null)
            groupHsaMap = new HashMap();

        if (getDefaultHsa() != null)
            hsaInfoSet.add(getDefaultHsa());
        else
            _logger.log("RiverMonitorLocGroupDataManager.initGroupHsaMap() : Admin HSA is null");

        
        
        for (int i = 0; i < _groupInfoList.size(); i++)
        {
            RiverMonGroupRecord riverMonGroupRecord = (RiverMonGroupRecord) _groupInfoList.get(i);
            String groupId = riverMonGroupRecord.getGroup_id();
            if (! (groupId.equals(_defaultGroupId)))
            {
                String lids[] = getLocationsForGroup(groupId);
                String hsa = null;
                if (lids != null)
                {
                    hsa = riverMonGroupRecord.getHsa();
                    
                    if (hsa == null)
                    {
                        if (lids.length > 0)
                        {
                            //the default hsa for a group is defined by the HSA of the first location in that group
                            hsa = _locationHsaMap.get(lids[0]).toString();
                        }
                        else
                        {
                            hsa = getDefaultHsa();
                        }
                    }
                }
                else
                {
                    hsa = riverMonGroupRecord.getHsa();
                    if (hsa == null)
                    {
                        hsa = getDefaultHsa();
                    }
                }
                //     System.out.println("For GroupId :"+groupId+" HSA Old:"+
                //       riverMonGroupRecord.getHsa() +" New:"+hsa);
                groupHsaMap.put(groupId, hsa);
                hsaInfoSet.add(hsa);
            }
            else
            {
                String hsa = getDefaultHsa();
                // System.out.println("For GroupId :"+groupId+" HSA Old:"+
                //   riverMonGroupRecord.getHsa() +" New:"+hsa);
                groupHsaMap.put(groupId, hsa);
                hsaInfoSet.add(hsa);
            }
        }

        //check if groupHsaMap has an entry for DEFAULT group - if not add one for DEFAULT group
        if(groupHsaMap != null)
        {
            if(groupHsaMap.get(_defaultGroupId) == null)
            {
                //   System.out.println("For GroupId :"+_defaultGroupId+" HSA "+getDefaultHsa());
                groupHsaMap.put(_defaultGroupId, getDefaultHsa());
            }
        }

        _groupHsaMap = groupHsaMap;
        _hsaInfoSet = hsaInfoSet;
    }
    
    // --------------------------------------------------------------------------------------
    
    public String[] getAllLocationsForGroup(String groupId, List<String> locationIdList, LocationOrdinalComparator locationComparator)
    {
        String [] returnArray = null;
          
   /*     if (_groupIdToLocationIdListMap.size() == 0)
        {
            buildGroupIdToLocationIdListMap(locationIdList);
            locationComparator = new LocationOrdinalComparator();
        }
     */   
        List<String> listOfLocationsInGroup = _groupIdToLocationIdListMap.get(groupId);
       
        if (listOfLocationsInGroup !=  null)
        {

            Collections.sort(listOfLocationsInGroup, locationComparator);
            //convert to String array
            returnArray = new String[listOfLocationsInGroup.size()];
            int i = 0;
            for (String locationId : listOfLocationsInGroup)
            {
                returnArray[i] = locationId;
                i++;
            }
        }
        
        return returnArray;
    }
    
    
    // -------------------------------------------------------------------------------
    public void buildGroupIdToLocationIdListMap(List<String> locationIdList)
    {
        
        _groupIdToLocationIdListMap.clear();
        
        for (String locationId : locationIdList )
        {
            String groupId = getGroupId(locationId);

            List<String> groupLocationIdList = _groupIdToLocationIdListMap.get(groupId);
            if (groupLocationIdList == null)
            {
                groupLocationIdList = new ArrayList<String>();
               _groupIdToLocationIdListMap.put(groupId, groupLocationIdList);
            }
            
            groupLocationIdList.add(locationId);

        }
        return;
    }

    // -------------------------------------------------------------------------------
    public String[] getLocationsForGroup(String groupId)
    {
        String returnArray[] = null;
        if (_lidToRiverMonLocationMap == null)
            initRiverMonLocationMap();
        List locationList = new ArrayList();
        if(groupId.compareTo(_defaultGroupId) == 0)
        {
            returnArray = getLocationsUnderDefaultGroup();
        }
        else
        {
            for (int i = 0; i < _locationInfoList.size(); i++)
            {
                RiverMonLocationRecord riverMonLocationRecord = (RiverMonLocationRecord) _locationInfoList.get(i);
                String tempGroupId = riverMonLocationRecord.getGroup_id();
                if (tempGroupId.compareTo(groupId) == 0)
                    locationList.add(riverMonLocationRecord.getLid());
            }
            if (locationList != null)
            {
                returnArray = new String[locationList.size()];
                for (int i = 0; i < locationList.size(); i++)
                {
                    returnArray[i] = locationList.get(i).toString();
                }
            }
        }
        return returnArray;
    }

    protected String[] getLocationsUnderDefaultGroup()
    {
        String[] returnArray = null;
        if(_locationsUnderDefaultGroupList != null)
        {
            returnArray = new String[_locationsUnderDefaultGroupList.size()];
            for (int i = 0; i < returnArray.length; i++)
                returnArray[i] = (String) _locationsUnderDefaultGroupList.get(i);
        }
        return returnArray;

    }

    protected void addThisLocationToDefaultGroupList(String lid)
    {
        boolean isNewLocation = true;
        if(_locationsUnderDefaultGroupList == null)
        {
            _locationsUnderDefaultGroupList = new ArrayList<String> ();
        }
        else
        {
            for(String str: _locationsUnderDefaultGroupList)
            {
                if(str.compareTo(lid) == 0)
                {
                    isNewLocation = false;
                    break;
                }
            }
        }

        if(isNewLocation)
            _locationsUnderDefaultGroupList.add(lid);
    }


    public void initLocationHsaMap()
    {
        Map locationHsaMap = null;
        String header = "LocationInfoDataManager.readLocationHsaMap(): ";
        LocationTable locationTable = new LocationTable(_db);
        String whereClause = "order by lid";
        List locationList = null;
        try
        {
            locationList = locationTable.select(whereClause);
            if (locationList != null)
            {
                if (locationList.size() != 0)
                {
                    locationHsaMap = new HashMap();
                    for (int i = 0; i < locationList.size(); i++)
                    {
                        LocationRecord record = (LocationRecord) locationList.get(i);
                        String lid = record.getLid();
                        String hsa = record.getHsa();
                        locationHsaMap.put(lid, hsa);
                    }
                }
                else
                {
                    _logger.log(header + "No Records found in LocationTable ...");
                }
            }
            else
            {
                _logger.log(header + "No Records found in LocationTable ...");
            }
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
        _locationHsaMap = locationHsaMap;
    }


    //------------------------------------------------------------------------------------

    public void updateRiverMonitorGroupWithAppropriateHsa()
    {
        RiverMonGroupTable riverMonGroupTable = new RiverMonGroupTable(_db);
        List riverMonGroupList = null;
        String whereClause = "";
        CodeTimer timer = new CodeTimer();
        try
        {
            timer.start();
            riverMonGroupList = riverMonGroupTable.select(whereClause);
            if (riverMonGroupList != null)
            {
                if (riverMonGroupList.size() > 0)
                {
                    for (int i = 0; i < riverMonGroupList.size(); i++)
                    {
                        RiverMonGroupRecord oldRiverMonGroupRecord = (RiverMonGroupRecord) riverMonGroupList.get(i);
                        RiverMonGroupRecord newRiverMonGroupRecord = new RiverMonGroupRecord();
                        newRiverMonGroupRecord.setGroup_id(oldRiverMonGroupRecord.getGroup_id());
                        String hsa = (String) _groupHsaMap.get(oldRiverMonGroupRecord.getGroup_id());
                        if(hsa != null)
                            newRiverMonGroupRecord.setHsa(hsa);
                        else
                        {
                            System.out.println("hsa is null for:"+ oldRiverMonGroupRecord.getGroup_id());
                        }
                        newRiverMonGroupRecord.setOrdinal(oldRiverMonGroupRecord.getOrdinal());
                        newRiverMonGroupRecord.setGroup_name(oldRiverMonGroupRecord.getGroup_name());
                        riverMonGroupTable.update(oldRiverMonGroupRecord, newRiverMonGroupRecord);
                    }
                }
            }
            timer.stop("Time elapsed in updating rivermongrouptable for hsa:");
        }
        catch (SQLException e)
        {
            _logger.log("In " + " RiverMonitorLocGroupDataManager.updateRiverMonitorGroupWithAppropriateHsa()");
            logSQLException(e);
        }
    }

    public String[] getAllHsa()
    {
        String hsa[] = new String[_hsaInfoSet.size()]; // Stores the final hsa
        // array in the
        // alphabetical order,
        // with default hsa as
        // the first item
        String tempHsaArray[] = new String[_hsaInfoSet.size() - 1];
        int i = 0;
        Iterator it = _hsaInfoSet.iterator();

        while (it.hasNext() && i < _hsaInfoSet.size())
        {
            String tempHsa = it.next().toString();

            if (tempHsa.equals(getDefaultHsa())) // if default hsa found then
                // skip
            {
                continue;
            }
            else
                tempHsaArray[i++] = tempHsa;
        }
        Arrays.sort(tempHsaArray); // Alphabetize the array

        // Assign default hsa to be the first - so it will be displayed as first
        // in the tree, and the rest will be in alphabetical order
        hsa[0] = getDefaultHsa();
        System.arraycopy(tempHsaArray, 0, hsa, 1, tempHsaArray.length);

        return hsa;
    }

    private String[] getGroupsForHsa(String hsa)
    {
        String returnArray[] = null;
        List groupList = new ArrayList();
        for (int i = 0; i < _groupInfoList.size(); i++)
        {
            RiverMonGroupRecord riverMonGroupRecord = (RiverMonGroupRecord) _groupInfoList.get(i);
            String groupId = riverMonGroupRecord.getGroup_id();
            String groupHsa = _groupHsaMap.get(groupId).toString();
            if (hsa.equals(groupHsa))
            {
                groupList.add(groupId);
            }
        }
        if (groupList != null)
        {
            returnArray = new String[groupList.size()];
            for (int i = 0; i < groupList.size(); i++)
            {
                returnArray[i] = groupList.get(i).toString();
            }
        }

        return returnArray;
    }


    public String getGroupName(String groupId)
    {
        String groupName = null;
        if (_groupIdToRiverMonGroupMap == null)
        {
            initRiverMonGroupMap();
        }
        
        RiverMonGroupRecord riverMonGroupRecord = (RiverMonGroupRecord) _groupIdToRiverMonGroupMap.get(groupId);
        try
        {
            groupName = riverMonGroupRecord.getGroup_name();
        }
        catch (NullPointerException e)
        {
            System.out.println("groupId = " + groupId);
            System.exit(1);
        }
        return groupName;
    }

    public String getGroupId(String lid)
    {
        String groupId = null;
        RiverMonLocationRecord riverMonlocationRecord = null;

        if (_lidToRiverMonLocationMap != null)
        {
          
            
            riverMonlocationRecord = (RiverMonLocationRecord) _lidToRiverMonLocationMap.get(lid);

            // pick the group for the location
            if (riverMonlocationRecord != null)
            {
                groupId = riverMonlocationRecord.getGroup_id();
            }
            else // (riverMonlocationRecord == null)
            {

                String hsa = (String) _locationHsaMap.get(lid);

                if (hsa != null)
                {
                    groupId = "DEF_" + hsa;
                }
                else //hsa == null
                {
                    groupId = _defaultGroupId;
                    addThisLocationToDefaultGroupList(lid);
                }

            }
           

        }

        return groupId;
    }
    
    public String getHsa(String groupId)
    {
        //return getHsaOrig(groupId);
        return getHsa3(groupId);
    }

    public String getHsaOrig(String groupId)
    {
        String hsa ;
        RiverMonGroupRecord riverMonGroupRecord = null;
        if (_groupIdToRiverMonGroupMap != null)
        {
            if (_groupIdToRiverMonGroupMap.size() > 0)
                riverMonGroupRecord = (RiverMonGroupRecord) _groupIdToRiverMonGroupMap.get(groupId);
        }

        if (riverMonGroupRecord != null)
        {
            hsa = (String ) _groupHsaMap.get(riverMonGroupRecord.getGroup_id());
        }
        else
        {
            hsa = getDefaultHsa();
        }
        return hsa;
    }
    
    public String getHsa2(String groupId)
    {
        String hsa = getDefaultHsa();
  
        if (_groupIdToRiverMonGroupMap != null)
        {
                RiverMonGroupRecord riverMonGroupRecord = (RiverMonGroupRecord) _groupIdToRiverMonGroupMap.get(groupId);
            
                if (riverMonGroupRecord != null)
                {
                    hsa = _groupHsaMap.get(riverMonGroupRecord.getGroup_id()).toString();
                }
        }
     
        return hsa;
    }

    public String getHsa3(String groupId)
    {
        String hsa = (String)  _groupHsaMap.get(groupId);
         
        if (hsa == null)
        {
            hsa = getDefaultHsa();
        }
      
        return hsa;
    }

    public void resetLocationOrdinalForDefaultLocations()
    {
        _locationOrdinalForDefaultLocations = 1;
    }

    public int getLocationOrdinal(String lid)
    {
        String header = "RiverMonLocGroupDataManager.getLocationOrdinal(): ";
        int locationOrdinal = 1;
        RiverMonLocationRecord riverMonlocationRecord = null;
        
        String groupId = getGroupId(lid);
        
        if (groupId == null)
        {
            System.out.println(header + "groupId was null for lid = " + lid);
        }

        else
        {  
            if( ! groupId.equals(_defaultGroupId))
            {
                riverMonlocationRecord = (RiverMonLocationRecord) _lidToRiverMonLocationMap.get(lid);
                
                if (riverMonlocationRecord != null)
                {
                    locationOrdinal = riverMonlocationRecord.getOrdinal();
                }
            }
            else  // default group id
            {
                locationOrdinal = _locationOrdinalForDefaultLocations++;
            }
        }

        return locationOrdinal;
    }
    
    
    public int getGroupOrdinal(String groupId)
    {
        int groupOrdinal = 1;

        RiverMonGroupRecord riverMonGroupRecord = null;
        if (_groupIdToRiverMonGroupMap != null)
        {
            riverMonGroupRecord = (RiverMonGroupRecord) _groupIdToRiverMonGroupMap.get(groupId);
        }

        if (riverMonGroupRecord != null)
        {
            groupOrdinal = riverMonGroupRecord.getOrdinal();
        }
        else   
        {
            // scenario where default group doesn't have entry in
            // RivermonGroup

            if ((_groupInfoList != null) && (_groupInfoList.size() > 0)  )// RiverMonGroup has
                // some entries
            {

                RiverMonGroupRecord tempRiverMonGroupRecord = (RiverMonGroupRecord) _groupInfoList.get(0);
                int max = tempRiverMonGroupRecord.getOrdinal();
                
                for (int k = 1; k < _groupInfoList.size(); k++)
                {
                    tempRiverMonGroupRecord = (RiverMonGroupRecord) _groupInfoList.get(k);
                    if (max < tempRiverMonGroupRecord.getOrdinal())
                    {
                        max = tempRiverMonGroupRecord.getOrdinal();
                    }
                }
                groupOrdinal = max + 1;
            }
        }
   
        return groupOrdinal;
    }

    private int getGroupOrdinalOld(String groupId)
    {
        int groupOrdinal = 1;

        RiverMonGroupRecord riverMonGroupRecord = null;
        if (_groupIdToRiverMonGroupMap != null)
        {
            if (_groupIdToRiverMonGroupMap.size() > 0)
                riverMonGroupRecord = (RiverMonGroupRecord) _groupIdToRiverMonGroupMap.get(groupId);
        }

        if (riverMonGroupRecord != null)
        {
            groupOrdinal = riverMonGroupRecord.getOrdinal();
        }
        else
            // scenario where default group doesn't have entry in
            // RivermonGroup
        {
            if (_groupIdToRiverMonGroupMap != null)
            {
                if (_groupIdToRiverMonGroupMap.size() > 0)// RiverMonGroup has
                    // some records
                {
                    if (_groupInfoList != null) // RiverMonGroup has
                        // some entries
                    {
                        if (_groupInfoList.size() > 0)
                        {
                            RiverMonGroupRecord tempRiverMonGroupRecord = (RiverMonGroupRecord) _groupIdToRiverMonGroupMap
                            .get(_defaultGroupId);
                            int max;
                            tempRiverMonGroupRecord = (RiverMonGroupRecord) _groupInfoList.get(0);
                            max = tempRiverMonGroupRecord.getOrdinal();
                            for (int k = 1; k < _groupInfoList.size(); k++)
                            {
                                tempRiverMonGroupRecord = (RiverMonGroupRecord) _groupInfoList.get(k);
                                if (max < tempRiverMonGroupRecord.getOrdinal())
                                    max = tempRiverMonGroupRecord.getOrdinal();
                            }
                            groupOrdinal = max + 1;
                        }
                        else
                        {
                            groupOrdinal = 1;
                        }
                    }
                }
                else
                    // No entries in RiverMonGroup
                {
                    groupOrdinal = 1;
                }
            }
        }

        return groupOrdinal;
    }

}

