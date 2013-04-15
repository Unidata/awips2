package org.rzo.yajsw.os;

import java.io.Serializable;

import org.rzo.yajsw.tools.JCLParser;

public class ServiceInfoImpl implements ServiceInfo, Serializable, Comparable<ServiceInfo>
{
	private String		_account					= "?";
	private String		_command					= "?";
	private String		_description				= "?";
	private String		_name						= "?";
	private int			_pid						= -1;
	private String		_startType					= "?";
	private int			_state						= -1;
	private int			_wrapperAppPid				= -1;
	private String		_wrapperConfigurationPath	= "?";
	private int			_wrapperJmxPort				= -1;
	private boolean		_interactive				= false;		;
	private String		_wrapped					= "-";
	private String[]	_dependencies;
	private String		_displayName;
	private String		_host						= "localhost";

	public String getAccount()
	{
		return _account;
	}

	public String getCommand()
	{
		return _command;
	}

	public String getDescription()
	{
		return _description;
	}

	public String getName()
	{
		return _name;
	}

	public int getPid()
	{
		return _pid;
	}

	public String getStartType()
	{
		return _startType;
	}

	public int getState()
	{
		return _state;
	}

	public int getWrapperAppPid()
	{
		return _wrapperAppPid;
	}

	public String getWrapperConfigurationPath()
	{
		return _wrapperConfigurationPath;
	}

	public int getWrapperJmxPort()
	{
		return _wrapperJmxPort;
	}

	public boolean isInteractive()
	{
		return _interactive;
	}

	public String getWrapped()
	{
		return _wrapped;
	}

	public void setAccount(String account)
	{
		_account = account;
	}

	public void setCommand(String command)
	{
		_command = command.trim();
		JCLParser p = null;
		try
		{
			p = JCLParser.parse(_command);
		}
		catch (Exception ex)
		{
		}
		if (p == null)
			return;
		if ("org.rzo.yajsw.boot.WrapperServiceBooter".equals(p.getMainClass()))
		{
			_wrapped = "Service";
			for (String option : p.getVmOptions())
				if (option.startsWith("-Dwrapper.config="))
					_wrapperConfigurationPath = option.substring("-Dwrapper.config=".length());
		}
		else if (p.getJar() != null && p.getJar().endsWith("wrapper.jar") && p.getArgs().size() > 1 && p.getArgs().get(0).equals("-c"))
		{
			_wrapped = "Console";
			_wrapperConfigurationPath = p.getArgs().get(1);
		}
	}

	public void setDescription(String description)
	{
		_description = description;
	}

	public void setName(String name)
	{
		_name = name;
	}

	public void setPid(int pid)
	{
		_pid = pid;
	}

	public void setStartType(String startType)
	{
		_startType = startType;
	}

	public void setState(int state)
	{
		_state = state;
	}

	public void setWrapperAppPid(int wrapperAppPid)
	{
		_wrapperAppPid = wrapperAppPid;
	}

	public void setWrapperConfigurationPath(String wrapperConfigurationPath)
	{
		_wrapperConfigurationPath = wrapperConfigurationPath;
	}

	public void setWrapperJmxPort(int wrapperJmxPort)
	{
		_wrapperJmxPort = wrapperJmxPort;
	}

	public void setIsInteractive(boolean istInteractive)
	{
		_interactive = istInteractive;
	}

	public void setWrapped(String wrapped)
	{
		_wrapped = wrapped;
	}

	public String[] getDependencies()
	{
		return _dependencies;
	}

	public void setDependencies(String[] dependencies)
	{
		_dependencies = dependencies;
	}

	public String getDisplayName()
	{
		return _displayName;
	}

	public void setDisplayName(String displayName)
	{
		_displayName = displayName;
	}

	public int compareTo(ServiceInfo o)
	{
		if (getDisplayName() != null)
			return getDisplayName().compareTo(o.getDisplayName());
		return 0;
	}

	public String getHost()
	{
		return _host;
	}

	public void setHost(String host)
	{
		_host = host;
	}

}
