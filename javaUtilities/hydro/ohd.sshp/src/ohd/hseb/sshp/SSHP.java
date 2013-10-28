/*
 * Created on Oct 3, 2003
 *
 * 
 */
package ohd.hseb.sshp;

import javax.swing.UIManager;

import ohd.hseb.util.FileLogger;
import ohd.hseb.util.Logger;

/**
 * @author Chip Gobs
 *
 * This is the driver class for the SSHP Application
 */
public class SSHP
{

	public static void main(String[] args)
	{
        try
        {
          //UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
          UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
        }
        catch (Exception e)
        {
            System.err.print(e.getMessage());
            e.printStackTrace();
        }
        
        if (args.length < 2)
        {
            System.err.println("Usage: ohd.hseb.sshp.SSHP connection_string logFileDir [location_id] "); 
            System.exit(0);
        }    
        
        
        String baseConnectionString = args[0];
        
        String logDirName = args[1];
    
        
        String logFileName = logDirName + "/SSHP.log";
        
        Logger logger = new FileLogger(logFileName, true, true); 
		DataMgr dataMgr = new DataMgr(baseConnectionString, logger);
        
     	
		AppController controller = new AppController(dataMgr, logDirName);	
    
        if (args.length >= 3)
        {
            String locationId = args[2];
            controller.displayAnalysisWindow(locationId);    
        }
        else
        {
            controller.displayControlWindowIfNotDisplayed();   
        }
	} //end main

} //end class SSHP
