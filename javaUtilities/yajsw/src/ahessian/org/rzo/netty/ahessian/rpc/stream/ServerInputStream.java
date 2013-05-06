package org.rzo.netty.ahessian.rpc.stream;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicBoolean;

import org.jboss.netty.channel.Channel;

public class ServerInputStream
{
	static final int BATCH_SIZE = 1000;
	private InputStream _in;
	private Executor	_executor;
	private Channel	_channel;
	private long	_id;
	private static ServerInputStreamBoss _boss = new ServerInputStreamBoss();
	static {_boss.start();}
	private AtomicBoolean _isDoingStream = new AtomicBoolean(false);

	
	private static class ServerInputStreamBoss extends Thread implements Runnable
	{
		private List<ServerInputStream> _streams = Collections.synchronizedList(new ArrayList<ServerInputStream>());

		public void add(ServerInputStream stream)
		{
				_streams.add(stream);
		}
		
		public void remove(ServerInputStream stream)
		{
			_streams.remove(stream);
		}
		
		public void run()
		{
			// TODO optimize, so we do not have a thread running for nothing
			while (true)
			{
			synchronized(_streams)
			{
			for (ServerInputStream stream : _streams)
			{
				stream.doStream();
			}
			}
			try
			{
				Thread.sleep(500);
			}
			catch (Exception ex)
			{
				
			}
			}
		}
		
	}
	
	public ServerInputStream(InputStream in, Executor executor, Channel channel, long id)
	{
		_in = in;
		_executor = executor;
		_channel = channel;
		_id = id;
	}

	public void start()
	{
		_boss.add(this);
	}
	
	protected void doStream()
	{
		// only one executor at a time
		if (_isDoingStream.get())
			return;
		else
			_isDoingStream.set(true);
		_executor.execute(new Runnable()
		{

			public void run()
			{
				try
				{
				while (_in.available() > 0 && _channel.isWritable())
				{
					byte[] data = new byte[BATCH_SIZE];
					int l = -1;
					try
					{
						_in.read(data);
					}
					catch (Exception ex)
					{
						ex.printStackTrace();
					}
					if (l == -1)
					{
						doClose();
						return;
					}
					else
					{
						doSendData(l, data);
					}
				}
				}
				catch (Exception ex)
				{
					doClose();
				}
				_isDoingStream.set(false);
			}
			
		});
	}

	private void doSendData(int length, byte[] data)
	{
		if (length == 0)
			return;
		if (length < data.length)
		{
			byte[] newData = new byte[length];
			System.arraycopy(data, 0, newData, 0, length);
			data = newData;
		}
		InputStreamReplyMessage msg = new InputStreamReplyMessage();
		msg.setId(_id);
		msg.setData(data);
		_channel.write(msg);		
	}

	private void doClose()
	{
		try
		{
			_in.close();
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		_boss.remove(this);
		InputStreamReplyMessage msg = new InputStreamReplyMessage();
		msg.setId(_id);
		msg.setClosed(true);
		_channel.write(msg);		

	}

	public long getId()
	{
		return _id;
	}

}
