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
package org.rzo.yajsw.os.posix.linux;

import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.os.ErrorHandler;
import org.rzo.yajsw.os.FileManager;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.Keyboard;
import org.rzo.yajsw.os.ProcessManager;
import org.rzo.yajsw.os.ServiceManager;
import org.rzo.yajsw.os.SystemInformation;
import org.rzo.yajsw.os.posix.OperatingSystemPosix;
import org.rzo.yajsw.os.posix.PosixSystemInformation;

// TODO: Auto-generated Javadoc
/**
 * The Class OperatingSystemWindowsXP.
 */
public class OperatingSystemLinux extends OperatingSystemPosix
{

	private static ProcessManager		_processManagerInstance;
	private static ServiceManager		_serviceManagerInstance;
	private static SystemInformation	_systemInformation	= new PosixSystemInformation();
	private static FileManager			_fileManagerInstance;

	@Override
	public ErrorHandler errorHandlerInstance()
	{
		return new LinuxErrorHandler();
	}

	@Override
	public JavaHome getJavaHome(Configuration config)
	{
		return new LinuxJavaHome(config);
	}

	@Override
	public Keyboard keyboardInstance()
	{
		return null;
	}

	@Override
	public ProcessManager processManagerInstance()
	{
		if (_processManagerInstance == null)
			_processManagerInstance = new LinuxProcessManager();
		return _processManagerInstance;
	}

	@Override
	public ServiceManager serviceManagerInstance()
	{
		if (_serviceManagerInstance == null)
			_serviceManagerInstance = new LinuxServiceManager();
		return _serviceManagerInstance;
	}

	@Override
	public SystemInformation systemInformation()
	{
		return _systemInformation;
	}

	@Override
	public FileManager fileManagerInstance()
	{
		if (_fileManagerInstance == null)
			_fileManagerInstance = new LinuxFileManager();
		return _fileManagerInstance;
	}
}
