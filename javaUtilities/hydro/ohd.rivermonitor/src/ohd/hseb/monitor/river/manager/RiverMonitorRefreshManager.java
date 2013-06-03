package ohd.hseb.monitor.river.manager;

import javax.swing.JSplitPane;

import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.manager.MonitorRefreshManager;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.river.RiverMonitorDataManager;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.DialogHelper;

public class RiverMonitorRefreshManager extends MonitorRefreshManager
{
      
    private RiverMonitorDataManager _riverMonitorDataManager;

    public RiverMonitorRefreshManager(MessageSystemManager msgSystemManager, SessionLogger logger, 
            RiverMonitorDataManager riverMonitorDataManager, JSplitPane splitPane)
    {
        super(msgSystemManager, logger, splitPane);
    
        _riverMonitorDataManager = riverMonitorDataManager;     
    }

    protected void reloadData(MonitorMessage incomingMessage, MessageType outgoingMessageType)
    {
        
        DialogHelper.setWaitCursor(_splitPane);
        
        String header = "MonitorRefreshManager.reloadData: ";
        System.out.println(header +" Invoked"+ "...Source:" + incomingMessage.getMessageSource() + " Type:" + incomingMessage.getMessageType());
        
        Runtime.getRuntime().gc();
        
        _riverMonitorDataManager.createRiverMonitorRowDataList();
        
        Runtime.getRuntime().gc();
        
        System.out.println(header);
        send(this, outgoingMessageType);
        
        DialogHelper.setDefaultCursor(_splitPane);
        
    }
  
}

