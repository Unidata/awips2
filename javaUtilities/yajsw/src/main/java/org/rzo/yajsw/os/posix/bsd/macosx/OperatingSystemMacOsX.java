package org.rzo.yajsw.os.posix.bsd.macosx;

import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.os.ErrorHandler;
import org.rzo.yajsw.os.FileManager;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.Keyboard;
import org.rzo.yajsw.os.ProcessManager;
import org.rzo.yajsw.os.ServiceManager;
import org.rzo.yajsw.os.SystemInformation;
import org.rzo.yajsw.os.posix.OperatingSystemPosix;
import org.rzo.yajsw.os.posix.PosixProcess;

public class OperatingSystemMacOsX extends OperatingSystemPosix
{

	private static ProcessManager		_processManagerInstance;
	private static ServiceManager		_serviceManagerInstance;
	private static SystemInformation	_systemInformation	= new MacOsXSystemInformation();
	private static FileManager			_fileManagerInstance;

	@Override
	public ErrorHandler errorHandlerInstance()
	{
		return new MacOsXErrorHandler();
	}

	@Override
	public JavaHome getJavaHome(Configuration config)
	{
		return new MacOsXJavaHome(config);
	}

	@Override
	public Keyboard keyboardInstance()
	{
		return null;
	}
	
	@Override
	public boolean setWorkingDir(String name)
	{
		return new MacOsXProcess().changeWorkingDir(name);
	}


	@Override
	public ProcessManager processManagerInstance()
	{
		if (_processManagerInstance == null)
			_processManagerInstance = new MacOsXProcessManager();
		return _processManagerInstance;
	}

	@Override
	public ServiceManager serviceManagerInstance()
	{
		if (_serviceManagerInstance == null)
			_serviceManagerInstance = new MacOsXServiceManager();
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
			_fileManagerInstance = new MacOsXFileManager();
		return _fileManagerInstance;
	}

}
