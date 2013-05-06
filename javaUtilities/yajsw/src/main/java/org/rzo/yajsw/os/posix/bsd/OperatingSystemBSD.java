package org.rzo.yajsw.os.posix.bsd;

import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.os.ErrorHandler;
import org.rzo.yajsw.os.FileManager;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.Keyboard;
import org.rzo.yajsw.os.ProcessManager;
import org.rzo.yajsw.os.ServiceManager;
import org.rzo.yajsw.os.SystemInformation;
import org.rzo.yajsw.os.posix.OperatingSystemPosix;

public class OperatingSystemBSD extends OperatingSystemPosix
{

	private static ProcessManager		_processManagerInstance;
	private static FileManager			_fileManagerInstance;
	private static ServiceManager		_serviceManagerInstance;
	private static SystemInformation	_systemInformation	= new BSDSystemInformation();

	@Override
	public ErrorHandler errorHandlerInstance()
	{
		return new BSDErrorHandler();
	}

	@Override
	public JavaHome getJavaHome(Configuration config)
	{
		return new BSDJavaHome(config);
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
			_processManagerInstance = new BSDProcessManager();
		return _processManagerInstance;
	}

	@Override
	public ServiceManager serviceManagerInstance()
	{
		if (_serviceManagerInstance == null)
			_serviceManagerInstance = new BSDServiceManager();
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
			_fileManagerInstance = new BSDFileManager();
		return _fileManagerInstance;
	}

}
