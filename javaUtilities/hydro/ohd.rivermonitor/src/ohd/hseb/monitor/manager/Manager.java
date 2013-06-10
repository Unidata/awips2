package ohd.hseb.monitor.manager;

import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.messaging.MessageSystemManager;

public interface Manager
{
    public String toString();
    
    public void send(MonitorMessage message);
    
    public void setMessageSystemManager(MessageSystemManager msgSystemManager);
}
