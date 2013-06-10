package org.rzo.yajsw.os.posix.bsd;

import java.util.ArrayList;
import java.util.List;

import org.rzo.yajsw.os.posix.PosixProcess;
import org.rzo.yajsw.os.posix.PosixProcess.CLibrary;

public class AppStarter
{
	public static void main(String[] args)
	{
		// get pid and send it to parent
		int pid = CLibrary.INSTANCE.getpid();
		System.out.println("PID:" + pid);
		System.out.flush();

		// set priority
		if (CLibrary.INSTANCE.nice(1) == -1)
			System.out.println("could not set priority ");
		if (getUser() != null)
			try
		{
			new PosixProcess().switchUser(getUser(), getPassword());
		}
		catch (Throwable ex)
		{
			ex.printStackTrace();
		}


		// detach from parent
		CLibrary.INSTANCE.umask(0);
		CLibrary.INSTANCE.setsid();

		/*
		 * bkowal
		 * Suppress extraneous output.
		 */
		//System.out.println("calling exec");
		// close streams ?
		if (!isPipeStreams())
		{
			/*
			try
			{
				System.in.close();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
			*/
			
			System.out.close();
			System.err.close();
		}
		
		String[] env = null;//getEnv();

		// start the subprocess
		int ret = -1;
		try
		{
			if (env == null)
				CLibrary.INSTANCE.execvp(args[0], args);
			else
				CLibrary.INSTANCE.execve(args[0], args, env);
			System.out.println("ret "+ret);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}

	}

	private static boolean isPipeStreams()
	{
		return System.getProperty("wrapperx.pipeStreams") != null;
	}

	private static String getPassword()
	{
		return System.getProperty("wrapperx.password");
	}

	private static String getUser()
	{
		return System.getProperty("wrapperx.user");
	}

	private static String[] getEnv()
	{
		List<String> result = new ArrayList<String>();
		for (String key : System.getenv().keySet())
		{
			result.add(key+"="+System.getenv(key));
		}
		if (result.isEmpty())
			return null;
		String[] arr = new String[result.size()];
		int i = 0;
		for (String x : result)
		{
			arr[i] = x;
			System.out.println(x);
			i++;
		}
		return arr;
	}


}
