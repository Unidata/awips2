package ohd.hseb.monitor;

public class MonitorFactory
{

    // -------------------------------------------------------------------------------------------------------------------
    public static Monitor buildLastModifiedTimeFileMonitor(long ageInSeconds, String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        FileMonitor monitor = new AgeFileMonitor(ageInSeconds, dataDirPath, logFilePath, fileToMonitorPath );
               
        return monitor;
    }
    
    // -------------------------------------------------------------------------------------------------------------------
    public static Monitor buildChangeFileMonitor(String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        FileMonitor monitor = new ChangeFileMonitor(dataDirPath, logFilePath, fileToMonitorPath );
          
        return monitor;
    }
    
    // -------------------------------------------------------------------------------------------------------------------
    public static Monitor buildExistenceFileMonitor(String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        Monitor monitor = new ExistenceFileMonitor(true, dataDirPath, logFilePath, fileToMonitorPath );
           
        return monitor;
     }
    
    // -------------------------------------------------------------------------------------------------------------------
    public static Monitor buildNonExistenceFileMonitor(String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
       Monitor monitor = new ExistenceFileMonitor(false, dataDirPath, logFilePath, fileToMonitorPath );
          
       return monitor;
    }
    
    // -------------------------------------------------------------------------------------------------------------------
    public static Monitor buildFileCountFileMonitor(MonitorOperator operator, long fileCount, String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        FileMonitor monitor = new CountFileMonitor(operator, fileCount, dataDirPath, logFilePath, fileToMonitorPath );
           
        return monitor;
    }
    
    // -------------------------------------------------------------------------------------------------------------------
    public static FileMonitor buildSizeFileMonitor(MonitorOperator operator, long fileSize, String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        FileMonitor monitor = new SizeFileMonitor(operator, fileSize, dataDirPath, logFilePath, fileToMonitorPath );
        //monitor.setOperator(operator);
        //monitor.setFileCount(fileSize);
   
        return monitor;
    }
    // -------------------------------------------------------------------------------------------------------------------

}
