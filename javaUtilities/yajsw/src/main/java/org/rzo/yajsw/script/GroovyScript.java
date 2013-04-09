/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.script;

import groovy.lang.Binding;
import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyObject;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.vfs2.FileName;
import org.apache.commons.vfs2.FileObject;
import org.codehaus.groovy.control.CompilationFailedException;
import org.jboss.netty.logging.InternalLogger;
import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.util.VFSUtils;
import org.rzo.yajsw.wrapper.WrappedJavaProcess;
import org.rzo.yajsw.wrapper.WrappedProcess;

/**
 * The Class GroovyScript.
 */
public class GroovyScript extends AbstractScript
{

	public static Map		context	= Collections.synchronizedMap(new HashMap());
	/** The binding. */
	final Binding			binding;

	final InternalLogger _logger;

	volatile GroovyObject	_script;
	
	final boolean _reload;
	
	final String _encoding;
	


	/**
	 * Instantiates a new groovy script.
	 * 
	 * @param script
	 *            the script
	 * @param timeout 
	 * @throws IOException
	 * @throws CompilationFailedException
	 * @throws IllegalAccessException
	 * @throws InstantiationException
	 * @throws ClassNotFoundException 
	 */
	public GroovyScript(final String script, final String id, final WrappedProcess process, final String[] args, final int timeout, final InternalLogger logger, String encoding, boolean reload) throws CompilationFailedException, IOException,
			InstantiationException, IllegalAccessException, ClassNotFoundException
	{
		super(script, id, process, args, timeout);
		_reload = reload;
		_encoding = encoding;

		// let's call some method on an instance
		_script = getScriptInstance(script, encoding);
		binding = (Binding) _script.invokeMethod("getBinding", null);
		binding.setVariable("args", args);
		binding.setVariable("callCount", 0);
		binding.setVariable("context", context);
		if (process != null && logger == null)
		_logger = process.getInternalWrapperLogger();
		else 
			_logger = logger;
		binding.setVariable("logger", _logger);
	}

	private void setGroovyClasspath(GroovyClassLoader loader)
	{
		ArrayList cp = WrapperLoader.getGroovyClasspath();
		for (Iterator it = cp.listIterator(); it.hasNext(); )
			loader.addURL((URL)it.next());
	}

  static GroovyClassLoader groovyClassLoader;

  private GroovyObject getScriptInstance(String scriptFileName, String encoding) throws IOException, InstantiationException,
      IllegalAccessException, ClassNotFoundException
  {
    FileObject fileObject = VFSUtils.resolveFile(".", scriptFileName);
    FileName fileName = fileObject.getName();
    long lastModified = fileObject.getContent().getLastModifiedTime();
    String scriptName = StringUtils.removeEnd(fileName.getBaseName(), "." + fileName.getExtension()) + "_"
        + lastModified;

    synchronized (GroovyScript.class)
    {
      if (groovyClassLoader == null)
      {
        groovyClassLoader = new GroovyClassLoader(getClass().getClassLoader());        
        setGroovyClasspath(groovyClassLoader);
      }

      try
      {
        Class clazz = Class.forName(scriptName, true, groovyClassLoader);
        if (_script == null)
        	return (GroovyObject) clazz.newInstance();
        else
        	return _script;
      }
      catch (ClassNotFoundException e)
      {
    	if (_script != null)
    		log("script changed -> reloading");
        InputStream in = null;
        String scriptSrc = null;
        try
        {
          in = fileObject.getContent().getInputStream();
          if (encoding == null)
        	  scriptSrc = IOUtils.toString(in);
          else 
        	  scriptSrc = IOUtils.toString(in, encoding);
        }
        finally
        {
          if (in != null)
            in.close();
        }
        return (GroovyObject) groovyClassLoader.parseClass(scriptSrc, scriptName + ".groovy").newInstance();         
      }
    }
  }

    /*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.script.AbstractScript#execute(java.lang.String,
	 * java.lang.String, java.lang.String, java.lang.String, java.lang.String,
	 * java.lang.String, java.lang.Object)
	 */
	synchronized public Object execute(String line)
	{
		Object result = null;
		
		if (_script == null)
		{
			System.out.println("cannot execute script " + _name);
			return null;
		}
		if (_reload)
		{
			GroovyObject script = null;
			try
			{
				script = getScriptInstance(_name, _encoding);
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
			if (script != null)
			{
				if (_script != script)
				{
					script.invokeMethod("setBinding", binding);
					_script = script;
				}
			}
		}
		binding.setVariable("id", _id);
		if (_process != null)
		{
		binding.setVariable("state", _process.getStringState());
		binding.setVariable("count", _process.getRestartCount());
		binding.setVariable("pid", _process.getAppPid());
		binding.setVariable("exitCode", _process.getExitCode());
		binding.setVariable("line", line);
		binding.setVariable("process", _process);
		}
		try
		{
			result = _script.invokeMethod("run", new Object[]{});
		}
		catch (Throwable e)
		{
			if (_logger != null)
			_logger.info("execption in script "+this._name, e);
			else
				e.printStackTrace();
		}
		binding.setVariable("callCount", ((Integer) binding.getVariable("callCount")).intValue() + 1);
		return result;
	}

	public static void main(String[] args) throws Exception, IOException, InstantiationException, IllegalAccessException
	{
		WrappedJavaProcess w = new WrappedJavaProcess();
		w.getLocalConfiguration().setProperty("wrapper.config", "conf/wrapper.helloworld.conf");
		w.init();
		GroovyScript script = new GroovyScript("./scripts/timeCondition.gv", "id", w, new String[]
		{ "11", "12" }, 0, null, null, false);
		script.execute();
		script.execute();
		script = new GroovyScript("./scripts/fileCondition.gv", "id", w, new String[]
		{ "anchor.lck" }, 0, null, null, false);
		script.execute();
		script.execute();
		script = new GroovyScript("./scripts/snmpTrap.gv", "id", w, new String[]
		{ "192.168.0.1", "1", "msg" }, 0, null, null, false);
		script.execute();

	}

	public Object execute()
	{
		return execute("");
	}

	public void executeWithTimeout()
	{
		executeWithTimeout("");
	}
	
	public void interrupt()
	{
		if (_future != null)
			_future.cancel(true);
	}
	
	void log(String msg)
	{
		if (_logger != null)
			_logger.info(msg);
		else
			System.out.println(msg);
	}
	
	public Object invoke(String method, Object ... x )
	{
		Object result = null;
		try
		{
			result = _script.invokeMethod(method, x);
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
		return result;
	}

}
