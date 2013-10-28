package ohd.hseb.monitor;

import java.io.File;


public class SizeFileMonitor extends FileMonitor
{

    private long _fileSize = -1;
    
    // --------------------------------------------------------------------------------------
    
    public SizeFileMonitor(MonitorOperator operator, long fileSize,
                                String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        super(dataDirPath, logFilePath, fileToMonitorPath);
        setOperator(operator);
        setFileSize(fileSize);
        
    }
    // --------------------------------------------------------------------------------------
    
    
    public void setFileSize(long fileSize)
    {
        _fileSize = fileSize;
    }
    // --------------------------------------------------------------------------------------

    public long getFileSize()
    {
        return _fileSize;
    }
    // --------------------------------------------------------------------------------------

    
    @Override
    public Status monitor()
    {
        //String header = "SizeFileMonitor.monitor(): ";
        Status status = null;
        File file = new File(getFileToMonitorPath());
       
        //System.out.println(header);
        
        if (file != null)
        {

            long actualFileSize = file.length();
            
    
            long comparisonFileSize = getFileSize();

            MonitorOperator operator = getOperator();

            boolean result = operator.evaluate(actualFileSize, comparisonFileSize);

            if (result)
            {
                status = new TextStatus( getFileToMonitorPath() + " contains " + actualFileSize + 
                        " files, which is "+ operator.getSymbol() + " " + comparisonFileSize );
            }
            else
            {
                //System.out.println(header + "monitoring " + getFileToMonitorPath() + " and there are " + actualFileSize + " files present. ");
            }
        }
        else
        {
            //System.out.println(header + " file is null");
        }
       
        return status;
    }
    // --------------------------------------------------------------------------------------
    
} //end class SizeFileMonitor
