package ohd.hseb.monitor.precip.manager;

import javax.swing.JSplitPane;

import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.manager.MonitorRefreshManager;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.monitor.precip.PrecipMonitorDataManager;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.util.gui.DialogHelper;

public class PrecipMonitorRefreshManager extends MonitorRefreshManager
{
    private PrecipMonitorDataManager _precipMonitorDataManager;

    public PrecipMonitorRefreshManager(MessageSystemManager msgSystemManager, SessionLogger logger, 
            PrecipMonitorDataManager precipMonitorDataManager, JSplitPane splitPane)
    {
        super(msgSystemManager, logger, splitPane);
    
        _precipMonitorDataManager = precipMonitorDataManager;     
    }

    protected void reloadData(MonitorMessage incomingMessage, MessageType outgoingMessageType)
    {
        DialogHelper.setWaitCursor(_splitPane);
        
        String header = "MonitorRefreshManager.reloadData(): ";
        System.out.println(header +" Invoked"+ "...Source:" + incomingMessage.getMessageSource() +
                         " Type:" + incomingMessage.getMessageType());
        
        Runtime.getRuntime().gc();
        _precipMonitorDataManager.createPrecipMonitorRowDataList();
        Runtime.getRuntime().gc();
        
        System.out.println(header);
        send(this, outgoingMessageType);
        
        DialogHelper.setDefaultCursor(_splitPane);
    }
  
}

