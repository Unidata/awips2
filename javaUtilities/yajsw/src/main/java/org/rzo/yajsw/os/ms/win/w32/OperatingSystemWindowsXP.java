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
package org.rzo.yajsw.os.ms.win.w32;

import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.os.ErrorHandler;
import org.rzo.yajsw.os.FileManager;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.Keyboard;
import org.rzo.yajsw.os.Mouse;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.ProcessManager;
import org.rzo.yajsw.os.ServiceManager;
import org.rzo.yajsw.os.SystemInformation;

// TODO: Auto-generated Javadoc
/**
 * The Class OperatingSystemWindowsXP.
 */
public class OperatingSystemWindowsXP extends OperatingSystem
{

	/** The _keyboard instance. */
	static Keyboard				_keyboardInstance;
	static Mouse				_mouseInstance;

	/** The _process manager. */
	static ProcessManager		_processManager;
	static FileManager			_fileManager;

	/** The _process manager. */
	static ServiceManager		_serviceManager;

	/** The _error handler. */
	static ErrorHandler			_errorHandler		= new WindowsXPErrorHandler();

	static SystemInformation	_systemInformation	= new WindowsXPSystemInformation();

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.OperatingSystem#keyboardInstance()
	 */
	@Override
	public Keyboard keyboardInstance()
	{
		if (_keyboardInstance == null)
			_keyboardInstance = WindowsXPKeyboard.instance();
		return _keyboardInstance;
	}

	public Mouse mouseInstance()
	{
		if (_mouseInstance == null)
			_mouseInstance = WindowsXPMouse.instance();
		return _mouseInstance;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.OperatingSystem#processManagerInstance()
	 */
	@Override
	public ProcessManager processManagerInstance()
	{
		if (_processManager == null)
			_processManager = WindowsXPProcessManager.instance();
		return _processManager;
	}

	public FileManager fileManagerInstance()
	{
		if (_fileManager == null)
			_fileManager = WindowsXPFileManager.instance();
		return _fileManager;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.OperatingSystem#errorHandlerInstance()
	 */
	@Override
	public ErrorHandler errorHandlerInstance()
	{
		// TODO Auto-generated method stub
		return _errorHandler;
	}

	public JavaHome getJavaHome(Configuration config)
	{
		return new WindowsJavaHome(config);
	}

	@Override
	public ServiceManager serviceManagerInstance()
	{
		if (_serviceManager == null)
			_serviceManager = WindowsXPServiceManager.instance();
		return _serviceManager;
	}

	@Override
	public SystemInformation systemInformation()
	{
		return _systemInformation;
	}

	@Override
	public boolean setWorkingDir(String name)
	{
		return new WindowsXPProcess().changeWorkingDir(name);
	}

	@Override
	public Object getServiceFailureActions(Configuration config)
	{
		return WindowsXPService.getServiceFailureActions(config);
	}

}
