package org.rzo.yajsw.os.ms.win.w32;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jnacontrib.jna.Advapi32;
import jnacontrib.jna.Advapi32.ENUM_SERVICE_STATUS_PROCESS;
import jnacontrib.jna.Advapi32.SC_ACTION;
import jnacontrib.jna.Advapi32.SERVICE_FAILURE_ACTIONS;
import jnacontrib.win32.Win32Service;

import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.os.AbstractService;
import org.rzo.yajsw.os.Service;
import org.rzo.yajsw.os.ServiceInfo;

public class WindowsXPService extends AbstractService
{

	class MyWin32Service extends Win32Service
	{

		public MyWin32Service(String name)
		{
			super(name);
		}

		@Override
		public void onStart()
		{
		}

		@Override
		public void onStop()
		{
		}

		@Override
		public void log(String txt)
		{
			if (_debug)
			System.out.println(txt);
		}

	}

	MyWin32Service	_service;

	public boolean install()
	{
		String command = "";
		for (int i = 0; i < _command.length; i++)
			if (_command[i].startsWith("\""))
				command += _command[i] + " ";
			else
				command += '"' + _command[i] + "\" ";

		return _service.install(_displayName, _description, _dependencies, _account, _password, command, _startType, _interactive, _failureActions);
	}

	public boolean start()
	{
		return _service.start();
	}

	public boolean stop()
	{
		try
		{
			return _service.stop();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return false;
		}
	}

	// TODO add further data from Info
	protected static Service getService(String name)
	{
		WindowsXPService result = new WindowsXPService();
		result.setName(name);
		result.init();
		return result;
	}

	public void init()
	{
		if (_service == null)
		{
			_service = new MyWin32Service(_name);
		}
		if (_config != null && _config.getBoolean("wrapper.ntservice.interactive", false))
			_interactive = true;
	}

	public boolean uninstall()
	{
		return _service.uninstall();
	}

	public int state()
	{
		if (_service == null)
			return STATE_UNKNOWN;
		return _service.state();

	}

	public static Map<String, ServiceInfo> getServiceList()
	{
		Map<String, ServiceInfo> result = new HashMap<String, ServiceInfo>();
		Map<String, ENUM_SERVICE_STATUS_PROCESS> services = Win32Service.enumerateServices(null);
		for (String name : services.keySet())
		{
			result.put(name, getServiceInfo(name));
		}
		return result;
	}

	public static ServiceInfo getServiceInfo(String name)
	{
		return Win32Service.serviceInfo(name);
	}

	public boolean requestElevation()
	{
		if (_service != null)
			return _service.requestElevation();
		return false;
	}
	
	public static Object getServiceFailureActions(Configuration config)
	{
		String cmd = config.getString("wrapper.ntservice.failure_actions.command", null);
		List<Object> actions = config.getList("wrapper.ntservice.failure_actions.actions", null);
		List<Object> actionsDelay = config.getList("wrapper.ntservice.failure_actions.actions_delay", new ArrayList());
		if (actions == null)
			return null;
		
		SC_ACTION[] scActions = (SC_ACTION[]) new SC_ACTION().toArray(actions.size());
			int i = 0;
			int lastDelay = 0;
			for (Object action : actions)
			{
				//scActions[i] = new SC_ACTION();
				if ("NONE".equals(action))
				{
					scActions[i].Type = Advapi32.SC_ACTION_NONE;
				}
				else if ("REBOOT".equals(action))
				{
					scActions[i].Type = Advapi32.SC_ACTION_REBOOT;
				}
				else if ("RESTART".equals(action))
				{
					scActions[i].Type = Advapi32.SC_ACTION_RESTART;
				}
				else if ("COMMAND".equals(action))
				{
					scActions[i].Type = Advapi32.SC_ACTION_RUN_COMMAND;
				}
				else
				{
					System.out.println("ERROR: unknown failure action : " + action);
					System.out.println("Aborting setting failure actions");
					return null;
				}
				if (actionsDelay.size() > i)
				try
				{
					Object d = actionsDelay.get(i);
					lastDelay = Integer.parseInt((String) d);
				}
				catch (Exception ex)
				{
					System.out.println("Error: failure actions delay is not a number.");
				}
				scActions[i].Delay = lastDelay;
				i++;
			}

		SERVICE_FAILURE_ACTIONS result = new SERVICE_FAILURE_ACTIONS();
		result.dwResetPeriod = config.getInt("wrapper.ntservice.failure_actions.reset_period", 0);
		result.lpCommand = config.getString("wrapper.ntservice.failure_actions.command", "");
		result.lpRebootMsg = config.getString("wrapper.ntservice.failure_actions.reboot_msg", null);
		result.cActions = scActions.length;
		scActions[0].autoWrite();
		result.lpsaActions = scActions[0].getPointer();
		result.write();
//		for (int z = 0; z<result.cActions*scActions[0].size(); z++)
//		{
//			System.out.print(z + ": ");
//			System.out.println(Integer.toHexString(result.lpsaActions.getByte(z)));
//		}
		return result;

	}

}
