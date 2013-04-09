package org.rzo.yajsw.os.ms.win.w32;

import java.util.Map;

import org.rzo.yajsw.os.Service;
import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.os.ServiceManager;

public class WindowsXPServiceManager implements ServiceManager
{

	/** The _instance. */
	static ServiceManager	_instance;

	/**
	 * Instance.
	 * 
	 * @return the process manager
	 */
	public static ServiceManager instance()
	{
		if (_instance == null)
			_instance = new WindowsXPServiceManager();
		return _instance;
	}

	public Service createService()
	{
		return new WindowsXPService();
	}

	public Service getService(String name)
	{
		return WindowsXPService.getService(name);
	}

	public Map<String, ServiceInfo> getServiceList()
	{
		return WindowsXPService.getServiceList();
	}

	public ServiceInfo getServiceInfo(String name)
	{
		// TODO Auto-generated method stub
		return null;
	}

}
