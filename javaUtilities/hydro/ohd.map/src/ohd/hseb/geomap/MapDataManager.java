package ohd.hseb.geomap;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.LocationRecord;
import ohd.hseb.ihfsdb.generated.LocationTable;
import ohd.hseb.ihfsdb.generated.RiverstatRecord;
import ohd.hseb.ihfsdb.generated.RiverstatTable;
import ohd.hseb.ihfsdb.generated.RpfFcstPointRecord;
import ohd.hseb.ihfsdb.generated.RpfFcstPointTable;
import ohd.hseb.ihfsdb.generated.StnClassRecord;
import ohd.hseb.ihfsdb.generated.StnClassTable;
import ohd.hseb.util.Logger;
import ohd.hseb.geomap.examples.StationPoint;
import ohd.hseb.geomap.model.LatLonPoint;


public class MapDataManager
{
    private Database _db = null;
    private Logger _logger = null;
    private Map<String, LocationRecord> _locationMap = new HashMap<String, LocationRecord>();
    
    // --------------------------------------------------------------------------------------------
    
    public MapDataManager(String connectionString, Logger logger)
    {       
        super();
        
        _db = new Database();
        _logger = logger;
      
        _db.connectWithDriverSearch(connectionString);  
        
        initializeLocationMap();

        return;
    }
    
    // --------------------------------------------------------------------------------------------
    
    private void initializeLocationMap()
    {
        
        LocationTable table = new LocationTable(_db);
 
        try
        {
            List<LocationRecord> recordList = (List<LocationRecord>) table.select(" order by lid");
            
            for (LocationRecord record :  recordList)
            {
                 _locationMap.put(record.getLid(), record);
            }
        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }
    }
    
    // --------------------------------------------------------------------------------------------
    
    public List<StationPoint> getPrecipMonitorPointList()
    {
        List<StationPoint> pointList = new ArrayList<StationPoint>();
        
        StnClassTable table = new StnClassTable(_db);
          
        List<StnClassRecord> recordList = null;
        
        String whereString = "WHERE disp_class like '%P%' ";
        
        try
        {

            recordList = (List<StnClassRecord>) table.select(whereString);

            for (StnClassRecord record : recordList)
            {   
                StationPoint point = getStationPoint(record.getLid());
                pointList.add(point);
            }

        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }
        
        return pointList;
    }
    
    // --------------------------------------------------------------------------------------------
    
    private StationPoint getStationPoint(String lid)
    {
       StationPoint stationPoint = null;
       LocationRecord record = _locationMap.get(lid);
       
       if (record != null)
       {
           LatLonPoint latLonPoint = new LatLonPoint(record.getLat(), -record.getLon());
           stationPoint = new StationPoint(record.getLid(), record.getName(), latLonPoint );   
       }
        
       return stationPoint;
    }
    // --------------------------------------------------------------------------------------------
    
    public List<StationPoint> getRiverProFcstPointList()
    {
        List<StationPoint> stationList = new ArrayList<StationPoint>();

        RpfFcstPointTable table = new RpfFcstPointTable(_db);
        
        try
        {
            List<RpfFcstPointRecord> recordList = (List<RpfFcstPointRecord>) table.select("");
            
            for (RpfFcstPointRecord record: recordList)
            {
                StationPoint point = getStationPoint(record.getLid());
                stationList.add(point);
            }
        }
        catch (SQLException e)
        {
            e.printStackTrace();
        }
        
        return stationList;
    }
    // --------------------------------------------------------------------------------------------
      
    public List<StationPoint> getSshpFcstPointList()
    {
        //stolen from SSHP's DataMgr
        List locationIdList = new ArrayList();
        List<StationPoint> sshpList = new ArrayList<StationPoint>();

        List recordList = null;

        RiverstatTable table = new RiverstatTable(_db);
        RiverstatRecord record = null;
        String whereClause = null;

        try
        {  
            whereClause = " WHERE lid IN (SELECT lid FROM SshpConfig)" +
            " AND lid IN (SELECT distinct(lid) FROM Rating) " +
            " AND lid IN (SELECT distinct(lid) FROM UnitGraph) " +
            " AND ((fs IS NOT NULL) OR (fq is NOT NULL)) ORDER BY lid ";

            recordList = table.select(whereClause);

            for (int i = 0; i < recordList.size(); i++)
            {
                record = (RiverstatRecord) recordList.get(i);
            
                locationIdList.add(record.getLid());
            }
            
            sshpList = loadSshpLocations(locationIdList);

        }
        catch (SQLException e)
        {
            logSQLException(e);
        }    

        return sshpList;         


    }
    // -----------------------------------------------------------------------------------------   
    
    private List<StationPoint> loadSshpLocations(List<String> locationIdList)
    {
        String header = "MapDataManager.loadSshpLocation(): ";
        
        List<StationPoint> sshpList = new ArrayList<StationPoint>();

        LocationTable table = new LocationTable(_db);
        
        
        //create the where clause
        
        StringBuffer whereBuffer = new StringBuffer("WHERE lid in ( ");
        
        boolean firstTime = true;
        
        for (String id : locationIdList)
        {
            if (! firstTime)
            {
                whereBuffer.append(", ");
            }
            else
            {
                firstTime = false;
            }
            
            whereBuffer.append("'" + id +"'");
        }
        whereBuffer.append(") ");
        
        System.out.println(header + whereBuffer);
        
        // run the Location table query to load up the objects
        
        try
        {

            List<LocationRecord> recordList = (List<LocationRecord>) table.select(whereBuffer.toString());
            
            for (LocationRecord record : recordList)
            {
                LatLonPoint latLonPoint = new LatLonPoint(record.getLat(), -record.getLon());
                
                StationPoint fcstPoint = new StationPoint(record.getLid(), record.getName(), latLonPoint );
            
              //  System.out.println(header + " fcstPoint = " + fcstPoint);
                sshpList.add(fcstPoint);
            }
            
        }
        catch (SQLException e)
        {
            logSQLException(e);
        }
        
        return sshpList;
        
    }
    // -----------------------------------------------------------------------------------------   
    private void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " +
                    exception.getErrorCode() +  " " +
                    exception.getMessage());
                         
        exception.printStackTrace(_logger.getPrintWriter());
        
        _logger.log("End of stack trace");
         
    }
    // --------------------------------------------------------------------------------------------
    
    public void disconnect()
    {
        _db.disconnect();   
    }
    
    //  -------------------------------------------------------------------------------------------
    public void finalize()
    {
        disconnect();   
    }
    
    //  ------------------------------------------------------------------------

} //MapDataManager
