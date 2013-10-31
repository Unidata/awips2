package ohd.hseb.monitor;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class CountFileMonitor extends FileMonitor
{

    private long _fileCount = -1;
    
    // --------------------------------------------------------------------------------------
    
    public CountFileMonitor(MonitorOperator operator, long fileCount,
                                String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        super(dataDirPath, logFilePath, fileToMonitorPath);
        setOperator(operator);
        setFileCount(fileCount);
        
    }
    // --------------------------------------------------------------------------------------
    
    public void setFileCount(long fileCount)
    {
        _fileCount = fileCount;
    }
    // --------------------------------------------------------------------------------------
    
    public long getFileCount()
    {
        return _fileCount;
    }
    // --------------------------------------------------------------------------------------
    
    @Override
    public Status monitor()
    {
      //  String header = "CountFileMonitor.monitor(): ";
        Status status = null;
        File file = new File(getFileToMonitorPath());
       
        
       
        //System.out.println(header);
        
        if (file != null)
        {

            String[] fileListStringArray = file.list();

            long actualFileCount = fileListStringArray.length;

            long comparisonFileCount = getFileCount();

            MonitorOperator operator = getOperator();

            boolean result = operator.evaluate(actualFileCount, comparisonFileCount);

            if (result)
            {
                status = new TextStatus( getFileToMonitorPath() + " contains " + actualFileCount + 
                        " files, which is "+ operator.getSymbol() + " " + comparisonFileCount );
            }
            else
            {
                //System.out.println(header + "monitoring " + getFileToMonitorPath() + " and there are " + actualFileCount + " files present. ");
            }
        }
        else
        {
           // System.out.println(header + " file is null");
        }
       
        return status;
    }
    // --------------------------------------------------------------------------------------
    
} //end class FileCountFileMonitor
