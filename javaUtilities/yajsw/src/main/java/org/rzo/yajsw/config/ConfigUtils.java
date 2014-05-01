package org.rzo.yajsw.config;

import java.util.Iterator;
import java.util.Properties;

import org.apache.commons.configuration.Configuration;


public class ConfigUtils
{
	
	public static Properties asProperties(Configuration config)
	{
		Properties result = new Properties();
		for (Iterator it = config.getKeys(); it.hasNext(); )
		{
			String key = (String) it.next();
			result.setProperty(key, config.getProperty(key).toString());
		}
		return result;
	}

}
