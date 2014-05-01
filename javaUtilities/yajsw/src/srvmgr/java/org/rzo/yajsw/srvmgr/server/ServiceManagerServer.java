package org.rzo.yajsw.srvmgr.server;

import java.util.List;
import java.util.Map;

import org.rzo.yajsw.os.Service;
import org.rzo.yajsw.os.ServiceInfo;

public interface ServiceManagerServer
{
	
	public Map<String, ServiceInfo> getServiceList();
	public ServiceInfo getService(String name);
	public boolean start(String name);
	public boolean stop(String name);
	public boolean yajswInstall(String configuration);
	public boolean yajswUninstall(String name);
	public boolean yajswReloadConsole(String name, String newConfig);
	public boolean isServiceManager();
	
}
