package org.rzo.yajsw.os.ms.win.w32;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.vfs2.FileSystemException;
import org.jboss.netty.logging.InternalLogger;
import org.jboss.netty.logging.InternalLoggerFactory;
import org.rzo.yajsw.script.Script;
import org.rzo.yajsw.script.ScriptFactory;
import org.rzo.yajsw.util.VFSUtils;

public class ScriptCounter implements PdhCounter
{
	InternalLogger		_log	= InternalLoggerFactory.getInstance(getClass().getName());
	Script				_script;
	private String		_scriptFile;
	String				_counterString;
	private String[]	_args;
	private long		_lastModified;

	/**
	 * Format "\ScriptCounter(abc.groovy)\arg1,arg2,..."
	 */
	public ScriptCounter(String counterString)
	{
		init(counterString);
		checkScript();

	}

	private void init(String counterString)
	{
		_counterString = counterString;
		_scriptFile = StringUtils.substringBetween(counterString, "(", ")");
		String argsString = StringUtils.substringAfter(counterString, ")\\");
		if (argsString != null)
			_args = StringUtils.split(argsString, ",");
	}

	private void checkScript()
	{
		long lastModified;
		try
		{
			lastModified = VFSUtils.resolveFile(".", _scriptFile).getContent().getLastModifiedTime();
		}
		catch (FileSystemException e)
		{
			throw new IllegalArgumentException("Cannot find script " + _scriptFile + " ex=" + e.getMessage());
		}
		if (_lastModified == lastModified)
			return;
		else
		{
			_lastModified = lastModified;
			_script = ScriptFactory.createScript(_scriptFile, _counterString, null, _args, _log, 0, null, false, false);
			if (_script == null)
				throw new IllegalArgumentException("Cannot find script " + _scriptFile);
		}
	}

	public void close()
	{
		// nothing
	}

	public double getDoubleValue()
	{
		checkScript();
		Object result = _script.execute();

		if (result instanceof Number)
			return ((Number) result).doubleValue();
		else
			return Double.parseDouble((String) result);
	}

	public int getIntValue()
	{
		checkScript();
		Object result = _script.execute();
		if (result instanceof Number)
			return ((Number) result).intValue();
		else
			return Integer.parseInt((String) result);
	}

	public boolean isValid()
	{
		try
		{
			getIntValue();
			return true;
		}
		catch (Exception e)
		{
			try
			{
				getDoubleValue();
				return true;
			}
			catch (Exception e2)
			{
				return false;
			}
		}
	}
}
