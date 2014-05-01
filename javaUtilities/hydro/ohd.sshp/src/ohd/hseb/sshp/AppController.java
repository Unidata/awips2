/*
 * Created on Sep 17, 2003
 *
 */
package ohd.hseb.sshp;

import java.awt.Rectangle;

import ohd.hseb.model.RainfallRunoffModelType;
import ohd.hseb.sshp.window.AnalysisWindow;
import ohd.hseb.sshp.window.ControlWindow;
import ohd.hseb.util.gui.*;
   
/**
 * @author Chip Gobs
 *
 * This class is the controller under the Model-View-Controller pattern.
 * It provides the "glue" application functionality.
 * (as a separate concept from the GUI functionality.);
*/
public class AppController
{
    	
    private DataMgr _dataMgr = null;
    private ControlWindow _controlWindow = null;
   // private FileLogger _logger = null;
    private String _logDirName = null;
    
    private String _operatingSystemString = null;
   
//-----------------------------------------------------------    	
	
	public AppController(DataMgr dataMgr, String logDirName)
	{
		_dataMgr = dataMgr;
		
        _logDirName = logDirName;
     //   _logger = new FileLogger(_logDirName, true, true);
        
		return;
	}
//	-----------------------------------------------------------   
   /* 
    public FileLogger getLogger()  
    {
        return _logger;    
    }
   */ 
 
//  -----------------------------------------------------------   
   
    public String getLogDirName()  
    {
        return _logDirName;   
    }
  
//  -----------------------------------------------------------   
  
    private StreamModel createStreamModel(String locationId)
    {
        
        RainfallRunoffModelType rrModelType =
                  _dataMgr.getPreferredRainfallRunoffModelType(locationId);
      
        StreamModel streamModel = new StreamModel(locationId,
                                                  rrModelType,
                                                  _dataMgr);
                                                  
        return streamModel;
    }

//  -----------------------------------------------------------   
    
    public void displayAnalysisWindow(String locationId)
    {
        //make sure that a launch window is available
        displayControlWindowIfNotDisplayed();    
        _controlWindow.enableLaunchButton(false);
        
        if (_dataMgr.isLocationIdValidForSSHP(locationId))
        {
        
          
            ModelManager modelManager = new ModelManager(locationId, _dataMgr);
            
            StreamModel streamModel = createStreamModel(locationId);
            modelManager.setPrimaryStreamModel(streamModel);
       
            AnalysisWindow analysisWindow = new AnalysisWindow(modelManager, this); 

            Rectangle bounds = analysisWindow.getBounds();
            bounds.x = 80;
            analysisWindow.setBounds(bounds);
            
            analysisWindow.setVisible(true);      
        } 
        else // this location is not valid for display in SSHP
        {
            String message = "Location :" + locationId + ": is not properly configured for use with SSHP.";
            String dialogTitle = "SSHP Error Dialog";
            DialogHelper.displayErrorDialog(_controlWindow, message, dialogTitle);
        } 
        
        _controlWindow.enableLaunchButton(true);
    }
 
//  -----------------------------------------------------------   

    
    public void displayControlWindowIfNotDisplayed()
    {
           //displays launch window if one is not already displayed
           if (_controlWindow == null)
           {
       
               _controlWindow = new ControlWindow(_dataMgr, this); 

               _controlWindow.setVisible(true); 
               
               Rectangle bounds = _controlWindow.getBounds();
               bounds.x = 80;
               _controlWindow.setBounds(bounds);
               
               _controlWindow.show();
           }
    } //end displayLaunchWindow()
//  -----------------------------------------------------------   

    
    public void bringLaunchWindowToFront()
    {
        //displays launch window if one is not already displayed
        if (_controlWindow != null)
        {
            _controlWindow.requestFocus();
            _controlWindow.setVisible(false);
            _controlWindow.setVisible(true); 
        }
    
    } //end displayLaunchWindow()
    
    public boolean isRunningOnWindows()
    {
        boolean result = false;
   
        if (_operatingSystemString == null)
        {
            _operatingSystemString = System.getProperty("os.name").toLowerCase();
        }
        
        if ( (_operatingSystemString.indexOf("windows") > -1) ||
             (_operatingSystemString.indexOf("nt") > -1))
        {
            // thanks to JuanFran for the xp fix!
            result = true;
        }
        
        return result;
    }
    
//  -----------------------------------------------------------   
   /* public void finalize()
    {
        if (_logger!= null)
        {
            _logger.close();    
        }
    }
   */ 
} //end class AppController
