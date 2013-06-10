package org.rzo.yajsw.groovy;

import java.util.HashMap;
import java.util.Iterator;

public class XHashMap extends HashMap
{
	public Object put(Object key, Object value)
	{
		// System.out.println("put "+key+" "+value);
		XHashMap v = (XHashMap) super.get(key);
		if (v == null)
			v = new XHashMap();
		v.setValue(value);
		return super.put(key, v);
	}

	void setValue(Object value)
	{
		super.put("value", value);
	}

	Object getValue()
	{
		return super.get("value");
	}

	public Object get(Object key)
	{
		Object value = super.get(key);
		// System.out.println("get "+key+" "+value);
		if (value == null)
		{
			value = new XHashMap();
			super.put(key, value);
		}
		return value;
	}

	public HashMap toMap()
	{
		HashMap result = new HashMap();
		for (Iterator it = keySet().iterator(); it.hasNext();)
			toMap("wrapper", (String) it.next(), result, this);
		return result;
	}

	private void toMap(String keyPrefix, String key, HashMap result, XHashMap map)
	{
		if ("value".equals(key))
			result.put(keyPrefix, map.getValue());
		else
		{
			XHashMap v = (XHashMap) map.get(key);
			for (Iterator it = v.keySet().iterator(); it.hasNext();)
				toMap(keyPrefix + "." + key, (String) it.next(), result, v);
		}
	}

}
