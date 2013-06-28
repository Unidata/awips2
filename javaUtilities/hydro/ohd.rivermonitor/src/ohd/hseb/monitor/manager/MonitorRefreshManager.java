package ohd.hseb.monitor.manager;

import javax.swing.JSplitPane;

import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.manager.BaseManager;
import ohd.hseb.monitor.manager.Receiver;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.SessionLogger;

public abstract class MonitorRefreshManager extends BaseManager
{
    protected SessionLogger _logger;
    protected JSplitPane _splitPane;

    public MonitorRefreshManager(MessageSystemManager msgSystemManager, SessionLogger logger, JSplitPane splitPane)
    {
        _logger = logger;
         _splitPane = splitPane;
   
        setMessageSystemManager(msgSystemManager);
            
        MessageType outgoingMessageType = MessageType.REFRESH_DISPLAY;
        RefreshReceiver reloadDataReceiver = new RefreshReceiver(outgoingMessageType);
        _msgSystemManager.registerReceiver(reloadDataReceiver, MessageType.RELOAD_DATA);
               
        outgoingMessageType = MessageType.UPDATE_DISPLAY_WITH_SETTINGS;
        RefreshReceiver reloadDataWithNewSettingsReceiver = new RefreshReceiver(outgoingMessageType);       
       _msgSystemManager.registerReceiver(reloadDataWithNewSettingsReceiver, MessageType.RELOAD_DATA_WITH_NEW_SETTINGS);
     
    }
    
    protected abstract void reloadData(MonitorMessage incomingMessage, MessageType outgoingMessageType);
  
    private void setSplitPaneDividerLocation()
    {
        String header = "MonitorRefreshManager.setSplitPaneDividerLocation(): ";
      
        int newDividerLocation =  _splitPane.getDividerLocation();
       // int lastDividerLocation = _splitPane.getLastDividerLocation();
        
        //System.out.println(header + "divider location = " + newDividerLocation);
        //System.out.println(header + "last divider location = " + lastDividerLocation);
        
         _splitPane.setDividerLocation(newDividerLocation);
      
        //newDividerLocation =  _splitPane.getDividerLocation();
        //lastDividerLocation = _splitPane.getLastDividerLocation();
        
       // System.out.println(header + "after reset divider location, dividerLocation = " + newDividerLocation);
       // System.out.println(header + "after reset divider location, lastDividerLocation = " + lastDividerLocation);
        
        return;
    }
    
    private class RefreshReceiver implements Receiver
    {
        private MessageType _outgoingMessageType = null;
           
        public RefreshReceiver(MessageType outgoingMessageType)
        {
            _outgoingMessageType = outgoingMessageType;
        }
        
        public void receive(MonitorMessage message)
        {
            String header = "MonitorRefreshManager.RefreshReceiver.receive(): ";
        
            //Note: setSplitPaneDividerLocation() is called to prevent the
            //inadvertent shrinking of the Filter (left side of SplitPane) when,
            //for example, the user maximizes the window while the 
            //application is refreshing.
            //Why setting the divider location BEFORE actually refreshing the
            //data and the JTable works is still a mystery to us as of 1/11/08.
            //Mysteries are bad!
            //Related code is in MonitorSplitPane.  In that class, the setting of
            //left and right SplitPane components occurs between calls to maintain the
            //divider location (again, to avoid having the  left side of the JSplitPane shrink.)
            //That code is not a mystery.
           
            setSplitPaneDividerLocation();

            
            CodeTimer reloadTimer = new CodeTimer();
            reloadTimer.start();
            
            reloadData(message, _outgoingMessageType);
            
            reloadTimer.stop(header + "reloadData took: ");
        }
    }

}

