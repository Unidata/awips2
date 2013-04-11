package org.rzo.yajsw.wrapper;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.MapConfiguration;
import org.rzo.yajsw.config.YajswConfiguration;
import org.rzo.yajsw.config.YajswConfigurationImpl;

public class WrappedProcessFactory
{
	public static WrappedProcess createProcess(YajswConfiguration config)
	{
		if (config.getString("wrapper.image") != null)
			return new WrappedRuntimeProcess();
		else if (config.getString("wrapper.groovy") != null)
			return new WrappedGroovyProcess();
		return new WrappedJavaProcess();
	}

	public static WrappedProcess createProcess(Map map, boolean useSystemProperties)
	{
		Configuration localConf = new MapConfiguration(map);
		YajswConfiguration conf = new YajswConfigurationImpl(localConf, true);
		WrappedProcess process = createProcess(conf);
		process.setLocalConfiguration(localConf);
		process.setUseSystemProperties(useSystemProperties);
		process.init();
		return process;
	}

	public static WrappedProcessList createProcessList(Map map, List<Object> confFiles, boolean useSystemProperties)
	{
		WrappedProcessList list = new WrappedProcessList();
		for (Object conf : confFiles)
		{
			Map sConf = new HashMap(map);
			sConf.put("wrapper.config", conf);
			list.add(createProcess(sConf, useSystemProperties));
		}
		return list;
	}

}
