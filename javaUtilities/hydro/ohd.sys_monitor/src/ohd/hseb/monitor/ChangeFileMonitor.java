package ohd.hseb.monitor;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;



public class ChangeFileMonitor extends FileMonitor
{
  
    // --------------------------------------------------------------------------------------
    
    public ChangeFileMonitor(String dataDirPath, String logFilePath, String fileToMonitorPath)
    {
        super(dataDirPath, logFilePath, fileToMonitorPath);
        
    }
    // --------------------------------------------------------------------------------------
      
    
    @Override
    public Status monitor()
    {
        String header = "ChangeFileMonitor.monitor(): ";
        System.out.println(header);
        Status status = null;
        
        String fileToMonitorPath = getFileToMonitorPath();
        String fileToMonitorBasename = getBaseFileName(fileToMonitorPath);
        
        
        String prevFilePath =  getDataDirPath() + '/' +  fileToMonitorBasename + ".prev";
        String diffFilePath =  getDataDirPath() + '/' +  fileToMonitorBasename + ".diff";
               
        File originalFile = new File(fileToMonitorPath);
        boolean originalFileExists = originalFile.exists();
        
        File previousFile = new File(prevFilePath);
        boolean previousFileExists = previousFile.exists();
        
        if (originalFileExists)
        {
            
            if (previousFileExists)
            {

                boolean fileChanged = false;
                FileDiff fileDiff = new FileDiff();
                //String fileChangesString = fileDiff.diffToString(prevFilePath, fileToMonitorPath, diffFilePath);
                
                String fileChangesString = fileDiff.showNewLines(prevFilePath, fileToMonitorPath);

                fileChanged = (fileChangesString.length() > 0);

                if (fileChanged)
                {                  
                    String message = "You are being notified that " +  getFileToMonitorPath() + " was changed.\n " +
                    "The following lines were added:\n" + fileChangesString;
                    
                    status = new TextStatus(message);
                    System.out.println(message);
                }

                else
                {
                    System.out.println(header + "Log: " +  "You are being notified that " +  getFileToMonitorPath() + 
                    " was not changed.");
                }
            }
            else // the originalFile exists, but the previous file does not
            {
                // do nothing
            }
        }
        
        //copy file to a .prev version of the file
       
        copyFile(prevFilePath, fileToMonitorPath);
        
        return status;
    }
    // --------------------------------------------------------------------------------------
    private String getBaseFileName(String fullPathName)
    {
        String baseFileName = null;
        
        File file = new File(fullPathName);
        baseFileName = file.getName();
        
        
        
        return baseFileName;
    }
    // --------------------------------------------------------------------------------------
    private void copyFile(String toFilePath, String fromFilePath)
    {
        File fromFile = new File(fromFilePath);
        File toFile = new File(toFilePath);
        
        try
        {
            FileChannel fromChannel = new FileInputStream(fromFile).getChannel();
            FileChannel toChannel = new FileOutputStream(toFile).getChannel();
            fromChannel.transferTo(0, fromFile.length(), toChannel);
            fromChannel.close();
            toChannel.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        
        return;          
    }
  
    // --------------------------------------------------------------------------------------
    

} //end class ChangeFileMonitor
