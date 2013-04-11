package org.rzo.yajsw.io;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.rzo.yajsw.util.DaemonThreadFactory;

public class NonBlockingWriter extends Writer
{
	OutputStream			_out;
	CircularBuffer			_buffer;
	boolean					_closed		= false;
	byte[]					_writeBuffer;

	static final Executor	executor	= Executors.newCachedThreadPool(new DaemonThreadFactory("nonblockingwriter"));

	public NonBlockingWriter(OutputStream out, int size, String fullIndicator)
	{
		_out = out;
		_buffer = new CircularBuffer(size, true);
		_buffer.setFullIndicator(fullIndicator);
		_buffer.setWriteBlocking(false);
		int writeSize = size / 10;
		if (writeSize > 1024)
			writeSize = 1024;
		else if (writeSize < 100)
			writeSize = size / 2;
		if (writeSize == 0)
			writeSize = 1;
		_writeBuffer = new byte[writeSize];
		executor.execute(new Runnable()
		{

			public void run()
			{
				while (!_closed)
				{
					int len = _buffer.get(_writeBuffer, 0, _writeBuffer.length);
					try
					{
						_out.write(_writeBuffer, 0, len);
					}
					catch (IOException e)
					{
						e.printStackTrace();
					}
				}
			}

		});
	}

	@Override
	public void close() throws IOException
	{
		_closed = true;
		_out.close();
	}

	@Override
	public void flush() throws IOException
	{
	}

	@Override
	public void write(char[] cbuf, int off, int len) throws IOException
	{
		_buffer.write(cbuf, off, len);
	}

	public static void main(String[] args) throws IOException
	{
		OutputStream s = new FileOutputStream("c:/test.txt");
		NonBlockingWriter w = new NonBlockingWriter(s, 1024, "!!! BUFFER FULL !!!");
		int i = 0;
		while (i < 10000)
		{
			w.write("12345678" + "\n\r");
			try
			{
				Thread.yield();
			}
			catch (Exception e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			i++;
		}
	}

}
