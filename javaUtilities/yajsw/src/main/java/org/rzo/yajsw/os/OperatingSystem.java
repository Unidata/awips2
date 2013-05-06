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
package org.rzo.yajsw.os;

import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.os.ms.win.w32.OperatingSystemWindowsXP;
import org.rzo.yajsw.os.posix.bsd.OperatingSystemBSD;
import org.rzo.yajsw.os.posix.bsd.macosx.OperatingSystemMacOsX;
import org.rzo.yajsw.os.posix.linux.OperatingSystemLinux;
import org.rzo.yajsw.os.posix.solaris.OperatingSystemSolaris;

// TODO: Auto-generated Javadoc
/**
 * The Class OperatingSystem.
 */
public abstract class OperatingSystem
{

	/** The _instance. */
	static OperatingSystem	_instance;

	/** The _os name. */
	static String			_osName;
	
	static boolean _isPosix = true;

	/**
	 * Instance.
	 * 
	 * @return the operating system
	 */
	public static OperatingSystem instance()
	{
		if (_instance != null)
			return _instance;
		_osName = System.getProperty("os.name");
		if (_osName.toLowerCase().startsWith("windows"))
		{
			_instance = new OperatingSystemWindowsXP();
			_isPosix = false;
		}
		else if (_osName.toLowerCase().startsWith("mac os x"))
			_instance = new OperatingSystemMacOsX();
		else if (_osName.contains("BSD"))
			_instance = new OperatingSystemBSD();
		else if (_osName.contains("AIX"))
			_instance = new OperatingSystemBSD();
		else if (_osName.toLowerCase().startsWith("linux"))
			_instance = new OperatingSystemLinux();
		else if (_osName.toLowerCase().contains("sunos"))
			_instance = new OperatingSystemSolaris();
		if (_instance == null)
			System.out.println("OS not supported " + _osName);
		return _instance;

	}

	/**
	 * Gets the operating system name.
	 * 
	 * @return the operating system name
	 */
	public String getOperatingSystemName()
	{
		return _osName;
	}
	
	public boolean isPosix()
	{
		return _isPosix;
	}

	/**
	 * Keyboard instance.
	 * 
	 * @return the keyboard
	 */
	public abstract Keyboard keyboardInstance();
	
	public abstract Mouse mouseInstance();

	/**
	 * Process manager instance.
	 * 
	 * @return the process manager
	 */
	public abstract ProcessManager processManagerInstance();

	public abstract FileManager fileManagerInstance();

	/**
	 * Service manager instance.
	 * 
	 * @return the process manager
	 */
	public abstract ServiceManager serviceManagerInstance();

	/**
	 * Error handler instance.
	 * 
	 * @return the error handler
	 */
	public abstract ErrorHandler errorHandlerInstance();

	public abstract JavaHome getJavaHome(Configuration config);

	public abstract Object getServiceFailureActions(Configuration config);

	public abstract SystemInformation systemInformation();

	public abstract boolean setWorkingDir(String name);

}
