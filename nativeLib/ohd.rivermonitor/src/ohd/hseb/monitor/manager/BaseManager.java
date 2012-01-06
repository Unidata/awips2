package ohd.hseb.monitor.manager;

import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.messaging.MessageSystemManager;
import ohd.hseb.monitor.messaging.MessageType;
import ohd.hseb.util.gui.jtable.JTableRowData;

public class BaseManager implements Manager
{
    public MessageSystemManager _msgSystemManager;
    
    public void setMessageSystemManager(MessageSystemManager msgSystemManager)
    {
        _msgSystemManager = msgSystemManager;
    }
  
    public void send(MonitorMessage message)
    {
        _msgSystemManager.send(message);
    }
    
    public void send(Manager manager, MessageType messageType)
    {
        MonitorMessage message = new MonitorMessage(manager, messageType);
        System.out.println("Sending..." + message.getMessageType());
        send(message);
    }
    
    public void send(Manager manager, MessageType messageType, JTableRowData selectedRowData)
    {
        MonitorMessage message = new MonitorMessage(manager, messageType, selectedRowData);
        System.out.println("Sending..." + message.getMessageType());
        send(message);
    }
    
    public String toString()
    {
        String className = this.getClass().toString();
        int lastIndexOfDot = className.lastIndexOf('.');
        String result = className.substring(lastIndexOfDot + 1, className.length());
        
        return result;
    }
    
}
