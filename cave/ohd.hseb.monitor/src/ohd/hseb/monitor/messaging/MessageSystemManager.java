package ohd.hseb.monitor.messaging;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import ohd.hseb.monitor.MonitorMessage;
import ohd.hseb.monitor.manager.Receiver;

public class MessageSystemManager
{
    private Map<MessageType, List<Receiver>> _messageTypeToReceiverListMap;

    public MessageSystemManager()
    {

        _messageTypeToReceiverListMap = new HashMap<MessageType, List<Receiver>>();
    }

    public void registerReceiver(Receiver receiver, MessageType messageType)
    {
        String header = "MessageSystemManager.addMeAsListener(): ";

        List<Receiver> receiverList = getReceiverList(messageType);
      
        receiverList.add(receiver);
    }

    public void send(MonitorMessage message)
    {
        String header = "MessageSystemManager.send(): ";

        List<Receiver> receiverList = getReceiverList(message.getMessageType());

        if(receiverList != null)
        {
            for(Receiver receiver : receiverList)
            {
               receiver.receive(message);
            }
        }
        else
        {
            System.out.println(header + "ReceiverList is null");
        }
    }

    private List<Receiver> getReceiverList(MessageType messageType)
    {
        String header = "MessageSystemManager.getReceiverList(): ";
        
        List<Receiver> receiverList = _messageTypeToReceiverListMap.get(messageType);

        if(receiverList == null)
        {
            receiverList = new ArrayList<Receiver>();
            _messageTypeToReceiverListMap.put(messageType, receiverList);
        }
        
        return receiverList;
    }
}
