package ohd.hseb.monitor.messaging;

public class Message
{
    private MessageType _messageType;
    private Object _source;
    
    public Message(Object source, MessageType messageType)
    {
        _source = source;
        _messageType = messageType;
    }
    
    public MessageType getMessageType()
    {
        return _messageType;
    }
    
    public Object getMessageSource()
    {
        return _source;
    }
    
    public void setMessageType(MessageType messageType)
    {
        _messageType = messageType;
    }
    
    public void setMessageSource(Object source)
    {
        _source = source;
    }
}
