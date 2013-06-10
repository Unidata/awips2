package org.rzo.yajsw.srvmgr.hub;

import java.util.List;

import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.srvmgr.client.Host;

public interface HubService
{
	
	public List<Host> getHosts();
	public List<ServiceInfo> getServices();
	public void start(String serviceName, String hostName);
	public void stop(String serviceName, String hostName);
	public void hide(String serviceName, String hostName);
}
