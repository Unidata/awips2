package org.rzo.netty.ahessian.rpc.stream;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executor;

import org.jboss.netty.channel.Channel;

public class ServerInputStreamManager
{
	long _id = 0;
	Map <Long, ServerInputStream> _streams = new HashMap<Long, ServerInputStream>();
	Executor _executor;
	
	public ServerInputStreamManager(Executor executor)
	{
		_executor = executor;
	}
	
	synchronized public  ServerInputStream createServerInputStream(InputStream stream, Channel channel)
	{
		ServerInputStream result = new ServerInputStream(stream, _executor, channel, _id);
		synchronized (_streams)
		{
			_streams.put(_id, result);
		}
		_id++;
		return result;
	}
	

}
