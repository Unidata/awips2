package org.rzo.netty.ahessian.io;

import java.io.IOException;

public class CRCInputStream extends InputStreamBuffer
{
	byte _crc = 0;
	
	public void resetCRC()
	{
		_crc = 0;
	}
	
	public byte getCRC()
	{
		byte result = _crc;
			resetCRC();
		return result;
	}
	
	@Override
	public int read() throws IOException
	{
		int result = super.read();
		_crc ^= (byte)result;
		return result;
	}
	
	@Override
	public int read(byte[] b, int off, int len) throws IOException
	{
		int result = super.read(b, off, len);
		for (int i=off; i<off+result; i++)
		{
			_crc ^= (byte)b[i];			
		}
		return result;
	}

}
