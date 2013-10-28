package ohd.hseb.monitor;

import ohd.hseb.db.Database;
import ohd.hseb.monitor.precip.manager.PrecipMonitorAppManager;
import ohd.hseb.monitor.river.manager.RiverMonitorAppManager;

public class Monitor
{
    public static void main(String args[])
    {
        String connectionString = args[0];
        Database db = createDatabaseConnection(connectionString);

        String missingRepresentation = args[1];
        String version = "OB9.0";
        String versionDate = "September 19, 2008";

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
            System.out.println( "Monitor Application.........Database should be provided........");
            System.exit(0);
        }
        db.connectWithDriverSearch(connectionString);
        return db;
    }
}
