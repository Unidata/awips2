package org.apache.commons.configuration;

import groovy.lang.Binding;
import groovy.lang.GroovyShell;

import java.util.HashMap;
import java.util.Map;

import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;

public class GInterpolator implements Interpolator
{
	Binding			_binding;
	GroovyShell		_shell;
	Configuration	_conf;
	Map				_cache		= null;
	String[]		_imports	= null;
	InternalLogger			log						= InternalLoggerFactory.getInstance(this.getClass().getName());

	public GInterpolator(Configuration conf, boolean cache, String[] imports, Map utils)
	{
		_conf = conf;
		_binding = new ConfigurationBinding(conf, utils);
		_shell = new GroovyShell(_binding);
		setCache(cache);
		_imports = imports;
	}

	public GInterpolator(Configuration conf)
	{
		this(conf, false, null, null);
	}

	public void setCache(boolean cache)
	{
		if (cache)
			_cache = new HashMap();
	}

	public Object interpolate(Object value)
	{
		if (!(value instanceof String))
			return value;
		if (_cache != null)
		{
			Object cachedResult = _cache.get(value);
			if (cachedResult != null)
				return cachedResult;
		}
		String result = (String) value;
		int i = result.lastIndexOf("${");
		while (i != -1)
		{
			int r = getExpression(result, i + 2);
			String expression = result.substring(i + 2, r);
			String eval = evaluate(expression);
			String p1 = result.substring(0, i);
			String p2 = result.substring(r + 1);
			result = p1 + eval + p2;
			i = result.lastIndexOf("${");
		}
		if (_cache != null)
			_cache.put(value, result);
		return result;

	}

	private int getExpression(String value, int i)
	{
		int i1 = value.indexOf('{', i);
		int i2 = value.indexOf('}', i);
		while (i1 != -1 && i2 > i1)
		{
			i2 = value.indexOf('}', i2 + 1);
			i1 = value.indexOf('{', i1 + 1);
		}

		return i2;
	}

	private String evaluate(String value)
	{
		String result = null;
		Exception caught = null;
		try
		{
			result = (String)_binding.getVariable(value);
		}
		catch (Exception ex)
		{
			caught = ex;
			try
			{
			result =  _conf.getString(value);
			}
			catch (Exception ex1)
			{
				caught = ex1;
			}
		}
		if (result == null)
			try
			{
				if (_imports != null)
					for (String im : _imports)
					{
						value = "import " + im + "\n" + value;
					}
				result = _shell.evaluate(value).toString();
			}
			catch (Exception ex)
			{
				caught = ex;
			}
		if (result == null)
		{
			result = "?unresolved?";
			if (caught != null)
				//log.warn("error evaluating "+value, caught);
				log.warn("error evaluating "+value+" : "+caught.getMessage());
			else
				log.warn("error evaluating "+value);
		}
		return result;

	}
	
	public Binding getBinding()
	{
		return _binding;
	}

}
