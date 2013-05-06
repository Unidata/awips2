package org.rzo.yajsw.os.posix.bsd.macosx;

import java.util.Map;

import org.rzo.yajsw.os.Service;
import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.os.ServiceManager;

public class MacOsXServiceManager implements ServiceManager
{

	public Service createService()
	{
		return new MacOsXService();
	}

	public Service getService(String name)
	{
		// TODO Auto-generated method stub
		return null;
	}

	public Map<String, ServiceInfo> getServiceList()
	{
		// TODO Auto-generated method stub
		return null;
	}

	public ServiceInfo getServiceInfo(String name)
	{
		// TODO Auto-generated method stub
		return null;
	}

}
