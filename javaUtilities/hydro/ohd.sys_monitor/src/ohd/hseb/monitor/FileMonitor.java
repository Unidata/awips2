package ohd.hseb.monitor;



abstract public class FileMonitor implements Monitor
{

    // private long _fileCount = -1;
    private long _fileSize = -1;
    private MonitorOperator _operator = null;
    
    private String _dataDirPath = null; //for monitor's private data
    private String _logFilePath = null; //for monitor's log file
    private String _fileToMonitorPath = null; //the file that monitor is examining
    
    //   CHANGE, EXISTENCE, NON_EXISTENCE, LAST_MODIFIED_TIME, FILE_COUNT, SIZE
   
    // -------------------------------------------------------------------------------------------------------------------

    
    // -------------------------------------------------------------------------------------------------------------------
    
    public FileMonitor(String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        setDataDirPath(dataDirPath);
        setLogFilePath(logFilePath);
        setFileToMonitorPath(fileToMonitorPath);
    }
    
    abstract public Status monitor();
 
    private void setDataDirPath(String dataDirPath)
    {
        _dataDirPath = dataDirPath;
    }

    protected String getDataDirPath()
    {
        return _dataDirPath;
    }

    private void setLogFilePath(String logFilePath)
    {
        _logFilePath = logFilePath;
    }

    protected String getLogFilePath()
    {
        return _logFilePath;
    }

    private void setFileToMonitorPath(String fileToMonitorPath)
    {
        _fileToMonitorPath = fileToMonitorPath;
    }

    protected String getFileToMonitorPath()
    {
        return _fileToMonitorPath;
    }

    
    public void setOperator(MonitorOperator operator)
    {
        _operator = operator;
    }

    public MonitorOperator getOperator()
    {
        return _operator;
    }
    
    
   
   

}
