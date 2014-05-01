/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import org.rzo.yajsw.util.DaemonThreadFactory;
import org.rzo.yajsw.util.MyReentrantLock;

// TODO: Auto-generated Javadoc
/**
 * The Class TeeInputStream.
 */
public class TeeInputStream extends InputStream
{

	/** The sources. */
	Source[]						sources			= new Source[0];

	/** The lock. */
	ReentrantLock					lock			= new MyReentrantLock();

	/** The data available. */
	Condition						dataAvailable	= lock.newCondition();

	/** The Constant executor. */
	static private final Executor	executor		= Executors.newCachedThreadPool(new DaemonThreadFactory("TeeInputStream"));

	/**
	 * Connect.
	 * 
	 * @param source
	 *            the source
	 */
	public synchronized void connect(InputStream source)
	{
		lock.lock();
		Source[] newsources = new Source[sources.length + 1];
		for (int i = 0; i < sources.length; i++)
		{
			if (source != sources[i].getInputStream())
				newsources[i] = sources[i];
			else
			{
				lock.unlock();
				return;
			}
		}
		newsources[newsources.length - 1] = new Source(source, dataAvailable);
		sources = newsources;
		executor.execute(newsources[newsources.length - 1]);
		lock.unlock();
	}

	/**
	 * Disconnect.
	 * 
	 * @param source
	 *            the source
	 */
	public synchronized void disconnect(InputStream source)
	{
		lock.lock();
		if (sources.length == 0)
		{
			lock.unlock();
			return;
		}
		Source[] newsources = new Source[sources.length - 1];
		int j = 0;
		boolean removed = false;
		for (int i = 0; i < sources.length && j < newsources.length; i++)
		{
			if (source != sources[i].getInputStream())
			{
				newsources[j] = sources[i];
				j++;
			}
			else
				removed = true;
		}
		if (removed)
			sources = newsources;
		lock.unlock();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#read()
	 */
	@Override
	public int read() throws IOException
	{
		lock.lock();
		while (true)
		{
			for (int i = 0; i < sources.length; i++)
				if (!sources[i].isStop() && sources[i].getBuffer().size() > 0)
				{
					int result = sources[i].getBuffer().get();
					lock.unlock();
					return result;
				}
			try
			{
				dataAvailable.await();
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#read(byte[], int, int)
	 */
	@Override
	public int read(byte b[], int off, int len) throws IOException
	{
		lock.lock();
		try
		{
			while (true)
			{
				for (int i = 0; i < sources.length; i++)
					if (!sources[i].isStop() && sources[i].getBuffer().size() > 0)
					{
						int result = sources[i].getBuffer().get(b, off, len);
						lock.unlock();
						return result;
					}
				try
				{
					// System.out.println("+await");
					dataAvailable.await();
					// System.out.println("-await");
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
					Thread.currentThread().interrupt();
				}
			}
		}

		finally
		{
			// lock.unlock();
		}

	}

	/**
	 * The Class Source.
	 */
	class Source implements Runnable
	{

		/** The in. */
		InputStream		in;

		/** The buffer. */
		CircularBuffer	buffer	= new CircularBuffer(512, true);

		/** The buff. */
		byte[]			buff	= new byte[512];

		/** The stop. */
		boolean			stop	= false;

		/** The data available. */
		Condition		dataAvailable;

		/**
		 * Instantiates a new source.
		 * 
		 * @param in
		 *            the in
		 * @param dataAvailable
		 *            the data available
		 */
		Source(InputStream in, Condition dataAvailable)
		{
			this.in = in;
			this.dataAvailable = dataAvailable;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Runnable#run()
		 */
		public void run()
		{
			while (!stop)
			{
				int c;
				try
				{
					// System.out.println("read");
					c = in.read();
					// System.out.println(c);
					if (c != -1)
					{
						lock.lock();
						buffer.put((byte) c);
						// System.out.println("put");
						try
						{
							// System.out.println("+signal");
							dataAvailable.signal();
							// System.out.println("-signal");
						}
						catch (Exception ex)
						{
							//ex.printStackTrace();
							System.err.println("could not read from InputStream "+ex.getMessage());
						}
						lock.unlock();
					}
					else
						stop = true;
				}
				catch (IOException e)
				{
					e.printStackTrace();
					stop = true;
				}
			}

		}

		/**
		 * Gets the buffer.
		 * 
		 * @return the buffer
		 */
		CircularBuffer getBuffer()
		{
			return buffer;
		}

		/**
		 * Checks if is stop.
		 * 
		 * @return true, if is stop
		 */
		boolean isStop()
		{
			return stop;
		}

		/**
		 * Close.
		 */
		void close()
		{
			try
			{
				in.close();
				buffer.close();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
			stop = true;
		}

		/**
		 * Gets the input stream.
		 * 
		 * @return the input stream
		 */
		InputStream getInputStream()
		{
			return in;
		}
	}

	public static void main(String[] args) throws IOException
	{
		TeeInputStream in = new TeeInputStream();
		InputStream inp = System.in;
		System.setIn(in);
		in.connect(inp);
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		String line;
		while ((line = reader.readLine()) != null)
			System.out.println(">" + line);
	}

}
