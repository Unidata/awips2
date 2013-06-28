package ohd.hseb.monitor;

import ohd.hseb.monitor.messaging.Message;
import ohd.hseb.monitor.messaging.MessageType;

public class MonitorMessage extends Message
{
    private Object _messageData;
    
    public MonitorMessage(Object source, MessageType messageType, Object messageData)
    {
        super(source, messageType);
        _messageData = messageData;
    }
    
    public MonitorMessage(Object source, MessageType messageType)
    {
        super(source, messageType);
        _messageData = null;
    }

    public Object getMessageData()
    {
        return _messageData;
    }

    public void setMessageData(Object  messageData)
    {
        _messageData = messageData;
    }
    
    
}
