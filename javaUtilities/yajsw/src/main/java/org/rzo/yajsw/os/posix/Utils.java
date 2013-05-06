package org.rzo.yajsw.os.posix;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.concurrent.Callable;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.rzo.yajsw.util.DaemonThreadFactory;

public class Utils
{
	protected static final Executor	executor	= Executors.newCachedThreadPool(new DaemonThreadFactory("util.osCommand"));
	protected Logger				_logger;

	public void setLog(Logger logger)
	{
		_logger = logger;
	}

	public String readFile(String file)
	{
		String result = "";
		File f = new File(file);
		if (f.exists())
			try
			{
				InputStream in = new FileInputStream(f);
				byte[] buffer = new byte[10 * 1024];
				int size = 0;
				
				while ((size = in.read(buffer)) > 0)
				{
				// System.out.println("size "+size);
				for (int i = 0; i < size; i++)
					if (buffer[i] == 0)
						buffer[i] = (byte) ' ';
				result += new String(buffer, 0, size);
				}
				in.close();
			}
			catch (Exception e)
			{
				if (_logger != null)
					_logger.throwing(Utils.class.getName(), "readFile", e);
			}
		else
		{
			if (_logger != null)
				_logger.info("could not find file " + f.getAbsolutePath());
			// throw new NullPointerException();
		}
		return result;

	}

	public String osCommand(String cmd)
	{
		StringBuffer result = new StringBuffer();
		try
		{
			Process p = Runtime.getRuntime().exec(cmd);
			InputStream in = p.getInputStream();
			int x;
			while ((x = in.read()) != -1)
				result.append((char) x);
		}
		catch (Exception ex)
		{
			if (_logger != null)
				_logger.warning("Error executing \"" + cmd + "\": " + ex);
		}
		return result.toString();
	}

	public String osCommand(String cmd, long timeout)
	{
		Process p = null;
		try
		{
			p = Runtime.getRuntime().exec(cmd);
			final Process fp = p;
			FutureTask<String> future = new FutureTask(new Callable()
			{

				public String call() throws Exception
				{
					StringBuffer result = new StringBuffer();
					InputStream in = fp.getInputStream();
					int x;
					while ((x = in.read()) != -1)
						result.append((char) x);

					return result.toString();
				}
			});
			executor.execute(future);
			String result = future.get(timeout, TimeUnit.MILLISECONDS);
			return result;

		}
		catch (Exception e)
		{
			if (_logger != null)
				_logger.warning("Error executing \"" + cmd + "\": " + e);
			if (p != null)
				p.destroy();
		}
		return null;
	}

	public static void main(String[] args)
	{
		System.out.println(new Utils().osCommand("cmd /C dir", 500));
	}

}
