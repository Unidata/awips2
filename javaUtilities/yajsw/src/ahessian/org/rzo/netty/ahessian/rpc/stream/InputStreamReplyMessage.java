package org.rzo.netty.ahessian.rpc.stream;

public class InputStreamReplyMessage
{
	private long _id = -1;
	private byte[] _data = null;
	private boolean _closed = false;
	private boolean _created = false;
	
	public void setId(long id)
	{
		_id = id;
	}
	public void setData(byte[] data)
	{
		_data = data;
	}
	public void setClosed(boolean b)
	{
		_closed = b;
	}
	public boolean isClosed()
	{
		return _closed;
	}
	public byte[] getData()
	{
		return _data;
	}
	public long getId()
	{
		return _id;
	}
	public void setCreated(boolean b)
	{
		_created = b;
	}
}
