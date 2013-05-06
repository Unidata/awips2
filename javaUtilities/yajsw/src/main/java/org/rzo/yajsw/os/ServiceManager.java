package org.rzo.yajsw.os;

import java.util.Map;

public interface ServiceManager
{
	public Service createService();

	public Service getService(String name);

	public ServiceInfo getServiceInfo(String name);

	public Map<String, ServiceInfo> getServiceList();
}
