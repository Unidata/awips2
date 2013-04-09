package org.rzo.netty.ahessian.io;

import java.io.IOException;

import org.jboss.netty.channel.ChannelHandlerContext;

public class CRCOutputStream extends OutputStreamBuffer
{
	public CRCOutputStream(ChannelHandlerContext ctx)
	{
		super(ctx);
	}

	byte _crc = 0;
	
	public void resetCRC()
	{
		_crc = 0;
	}
	
	public byte getCRC()
	{
		return _crc;
	}
	
	@Override
	public void write(int b) throws IOException
	{
		super.write(b);
		_crc ^= (byte)b;
	}
	
	@Override
	public void write(byte b[], int off, int len) throws IOException
	{
		super.write(b, off, len);
		for (int i=off; i<off+len; i++)
		{
			_crc ^= (byte)b[i];			
		}
	}


	

}
