package org.rzo.netty.ahessian.rpc.stream;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class ClientStreamManager
{
	Map<Long, ClientInputStream> _streams = new HashMap<Long, ClientInputStream>();
	
	public synchronized InputStream newInputStream(long id)
	{
		ClientInputStream result = new ClientInputStream(id);
		_streams.put(id, result);
		return result;
	}
	
	public synchronized void removeInputStream(ClientInputStream stream)
	{
		_streams.remove(stream.getId());
	}
	
	public synchronized void messageReceived(InputStreamReplyMessage msg)
	{
		ClientInputStream stream;
		if (msg.isClosed())
			stream = _streams.remove(msg.getId());
		else
			stream = _streams.get(msg.getId());
		if (stream != null)
			stream.addMessage(msg);
		else
			System.out.println("message for non existing stream "+msg.getId());
	}

}
