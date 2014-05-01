package org.rzo.yajsw.util;

import java.util.HashMap;
import java.util.Map;

public class CaseInsensitiveMap extends HashMap<String, String>
{

	public CaseInsensitiveMap(Map<String, String> map)
	{
		super();
		for (Map.Entry<String, String> entry : map.entrySet())
		{
			put(entry.getKey(), entry.getValue());
		}
	}

	@Override
	public String put(String key, String value)
	{
		return super.put(key.toLowerCase(), value);
	}

	@Override
	public String get(Object key)
	{
		return super.get(((String) key).toLowerCase());
	}
}
