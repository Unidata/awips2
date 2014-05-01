package ohd.hseb.monitor;

import java.io.File;


public class AgeFileMonitor extends FileMonitor
{
    private long _ageInSeconds = -1;
 
    // --------------------------------------------------------------------------------------
    
    public AgeFileMonitor(long ageInSeconds,
                          String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        super(dataDirPath, logFilePath, fileToMonitorPath);
        setAgeInSeconds(ageInSeconds);
        
    }
    // --------------------------------------------------------------------------------------
    
    public void setAgeInSeconds(long ageInSeconds)
    {
        _ageInSeconds = ageInSeconds;
    }
    // --------------------------------------------------------------------------------------

    public long getAgeInSeconds()
    {
        return _ageInSeconds;
    }

    // --------------------------------------------------------------------------------------
    
    
    @Override
    public Status monitor()
    {
       // String header = "AgeFileMonitor.monitor(): ";
        Status status = null;
        File file = new File(getFileToMonitorPath());
       
       // System.out.println(header);
        
        
        boolean fileExists = file.exists();
        
        long currentTimeInMillis = System.currentTimeMillis();

        if (fileExists)
        {
            long lastModifiedTime = file.lastModified();
            long diffInMillis = currentTimeInMillis - lastModifiedTime;
            long diffInSeconds = diffInMillis/1000;

            if (diffInSeconds > getAgeInSeconds())
            {
                status = new TextStatus("You are being notified that " +  getFileToMonitorPath() + " was last modified " + diffInSeconds + 
                        " seconds ago, which is more than the limit of " + getAgeInSeconds() + " seconds.");
            }

            else
            {
                //System.out.println(header + "Log: " +  getFileToMonitorPath() + " was last modified " + diffInSeconds + 
                //        " seconds ago, which is less than the limit of " + getAgeInSeconds() + " seconds.");
            }
        }
        else
        {
            status = new TextStatus("You are being notified that " +  getFileToMonitorPath() + ", which you are monitoring for age, does not exist.");
        }


        
        return status;
    }
    // --------------------------------------------------------------------------------------


} //end class AgeFileMonitor
