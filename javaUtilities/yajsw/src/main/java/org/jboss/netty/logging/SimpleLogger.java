package org.jboss.netty.logging;

public class SimpleLogger implements InternalLogger
{

	public void debug(String arg0)
	{
		System.out.println(arg0);
	}

	public void debug(String arg0, Throwable arg1)
	{
		System.out.println(arg0);
		arg1.printStackTrace();
	}

	public void error(String arg0)
	{
		System.out.println(arg0);
	}

	public void error(String arg0, Throwable arg1)
	{
		System.out.println(arg0);
		arg1.printStackTrace();
	}

	public void info(String arg0)
	{
		System.out.println(arg0);
	}

	public void info(String arg0, Throwable arg1)
	{
		System.out.println(arg0);
		arg1.printStackTrace();
	}

	public boolean isDebugEnabled()
	{
		return true;
	}

	public boolean isEnabled(InternalLogLevel arg0)
	{
		return true;
	}

	public boolean isErrorEnabled()
	{
		return true;
	}

	public boolean isInfoEnabled()
	{
		return true;
	}

	public boolean isWarnEnabled()
	{
		return true;
	}

	public void log(InternalLogLevel arg0, String arg1)
	{
		// TODO Auto-generated method stub
		
	}

	public void log(InternalLogLevel arg0, String arg1, Throwable arg2)
	{
		// TODO Auto-generated method stub
		
	}

	public void warn(String arg0)
	{
		System.out.println(arg0);
	}

	public void warn(String arg0, Throwable arg1)
	{
		System.out.println(arg0);
		arg1.printStackTrace();
	}

}
