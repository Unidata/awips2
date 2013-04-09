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
package org.rzo.yajsw.script;

import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.wrapper.WrappedProcess;

// TODO: Auto-generated Javadoc
/**
 * The Class ShellScript.
 */
public class ShellScript extends AbstractScript
{
	volatile Process p = null;

	/**
	 * Instantiates a new shell script.
	 * 
	 * @param script
	 *            the script
	 * @param timeout
	 */
	public ShellScript(String script, String id, WrappedProcess process, String[] args, int timeout)
	{
		super("scripts/" + script, id, process, args, timeout);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.script.AbstractScript#execute(java.lang.String,
	 * java.lang.String, java.lang.String, java.lang.String, java.lang.String,
	 * java.lang.String, java.lang.Object)
	 */
	public Object execute(String line)
	{
		String id = _id;
		String state = _process.getStringState();
		String count = "" + _process.getRestartCount();
		String pid = "" + _process.getAppPid();
		String exitCode = "" + _process.getExitCode();
		try
		{
			p = OperatingSystem.instance().processManagerInstance().createProcess();
			p.setCommand(getScript() + " " + id + " " + state + " " + count + " " + pid + " " + exitCode);
			p.setPipeStreams(false, false);
			p.start();
			p.waitFor(getTimeout());
			if (p.isRunning())
				p.kill(999);
			if (p.getExitCode() != 0)
				System.out.println("script " + getScript() + "returned " + p.getExitCode());
			p.destroy();
			p = null;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		return null;
	}

	public Object execute()
	{
		return execute("");
	}

	public void executeWithTimeout()
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void interrupt()
	{
		if (p != null)
		{
			p.destroy();
		}
	}
	
	void log(String msg)
	{
		if (_process != null && _process.getInternalWrapperLogger() != null)
			_process.getInternalWrapperLogger().info(msg);
		else
			System.out.println(msg);
	}


}
