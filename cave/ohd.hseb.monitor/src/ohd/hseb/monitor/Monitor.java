package ohd.hseb.monitor;

import ohd.hseb.db.Database;
import ohd.hseb.monitor.precip.manager.PrecipMonitorAppManager;
import ohd.hseb.monitor.river.manager.RiverMonitorAppManager;

public class Monitor
{
    private static Monitor precipInstance = null;
    private static Monitor riverInstance = null;
    
    public Monitor() {
   
    }
    
    /**
     * Gets the river instance.
     * @return Monitor
     */
    public static Monitor getRiverInstance() {
       if(riverInstance == null) {
          riverInstance = new Monitor();
       }
       return riverInstance;
    }
    
    /**
     * Gets the precip instance.
     * @return Monitor
     */
    public static Monitor getPrecipInstance() {
       if(precipInstance == null) {
          precipInstance = new Monitor();
       }
       return precipInstance;
    }
    
    /**
     * Check to see if river instance exists.
     * @return boolean
     */
    public static boolean riverExists() {
       boolean exists = false;
       if(riverInstance != null) {
          exists = true;
       }
       return exists;
    }
    
    /**
     * Check to see if river instance exists.
     * @return boolean
     */
    public static boolean precipExists() {
       boolean exists = false;
       if(precipInstance != null) {
          exists = true;
       }
       return exists;
    }
    
    /**
     * Get rid of river instance.
     */
    public static void disposeRiver() {
       riverInstance = null;
    }
    
    /**
     * Get rid of precip instance.
     */
    public static void disposePrecip() {
       precipInstance = null;
    }
   
    public void main(String args[])
    {
        String connectionString = args[0];
        Database db = createDatabaseConnection(connectionString);

        String missingRepresentation = args[1];
        String version = "OB8.3";
        String versionDate = "Mar 18, 2008";

        String monitorName = args[2];

        MonitorTimeSeriesLiteManager monitorTimeSeriesLiteManager = new MonitorTimeSeriesLiteManager(connectionString);

        if(monitorName.equalsIgnoreCase("PRECIP"))
        {
            new PrecipMonitorAppManager(db, missingRepresentation, version, versionDate, monitorTimeSeriesLiteManager);
        }
        else // RIVER
        {
            new RiverMonitorAppManager(db, missingRepresentation, version, versionDate, monitorTimeSeriesLiteManager);
        }
    }

    public static String getVersion()
    {
        return "OB8.3";
    }

    private static Database createDatabaseConnection(String connectionString)
    {
        Database db;
        db = new Database();
       
        if (connectionString == null)
        {
            System.out.println( "RiverMonitor Application.........Database should be provided........");
            
        }
        db.connectWithDriverSearch(connectionString);
        return db;
    }
}
