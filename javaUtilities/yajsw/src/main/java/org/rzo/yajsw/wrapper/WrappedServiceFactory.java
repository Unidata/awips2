package org.rzo.yajsw.wrapper;

import java.util.List;
import java.util.Map;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.MapConfiguration;

public class WrappedServiceFactory
{
	public static Object createService(Map map)
	{
		Configuration localConf = new MapConfiguration(map);
		WrappedService service = new WrappedService();
		service.setLocalConfiguration(localConf);
		service.init();
		return service;
	}

	public static Object createServiceList(Map map, List<String> confFiles)
	{
		Configuration localConf = new MapConfiguration(map);
		WrappedService service = new WrappedService();
		service.setLocalConfiguration(localConf);
		service.setConfFilesList(confFiles);
		service.init();
		return service;
	}

}
