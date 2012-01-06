package ohd.hseb.monitor;

import java.io.File;


public class ExistenceFileMonitor extends FileMonitor
{
    
    private boolean _checkForExistence = true;

    // --------------------------------------------------------------------------------------
    
    public ExistenceFileMonitor(boolean checkForExistence,
                                    String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        super(dataDirPath, logFilePath, fileToMonitorPath);
        setCheckForExistence(checkForExistence);
        
    }
    // --------------------------------------------------------------------------------------
    
    
   

    
    @Override
    public Status monitor()
    {
        String header = "ExistenceFileMonitor.monitor(): ";
        Status status = null;
        File file = new File(getFileToMonitorPath());
       
        System.out.println(header);
        
        
        boolean fileExists = file.exists();
        
        if (shouldCheckForExistence())
        {    
            
            if (fileExists)
            {
                status = new TextStatus("You are being notified that " +  getFileToMonitorPath() + " exists.");
            }
            else
            {
                System.out.println(header + "Log: " +  getFileToMonitorPath() + " does not exist. And we are looking for it.");
            }
        }
        else //check for non existence
        {
            if (! fileExists)
            {
                status = new TextStatus( "You are being notified that " +  getFileToMonitorPath() + " does not exist.");
            }
            else
            {
                System.out.println(header + "Log: " +  getFileToMonitorPath() + " does exist. And we looking for it NOT to exist.");
            }
        }

        
        return status;
    }
    // --------------------------------------------------------------------------------------

    public void setCheckForExistence(boolean checkForExistence)
    {
        _checkForExistence = checkForExistence;
    }
    // --------------------------------------------------------------------------------------

    public boolean shouldCheckForExistence()
    {
        return _checkForExistence;
    }
    // --------------------------------------------------------------------------------------


} //end class FileCountFileMonitor
