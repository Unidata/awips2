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
package org.rzo.yajsw.wrapper;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Random;
import java.util.Map.Entry;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.rzo.yajsw.Constants;
import org.rzo.yajsw.app.WrapperMainServiceWin;
import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.controller.AbstractController.ControllerListener;
import org.rzo.yajsw.controller.jvm.JVMController;
import org.rzo.yajsw.os.JavaHome;
import org.rzo.yajsw.os.OperatingSystem;
import org.apache.commons.lang.StringUtils;

// TODO: Auto-generated Javadoc
/**
 * The Class WrappedJavaProcess.
 */
public class WrappedJavaProcess extends AbstractWrappedProcess
{
	/* Constants used to set and determine the order of jvm parameters. */
	private static final String LAST_KEYWORD = "LAST";

	private static final String _PERIOD_SEPARATOR_ = ".";

	/*
	 * bkowal - define the parameter order parameter name
	 */
	private static final String ORDER_PATTERN = "wrapper.jvm.parameter.order";

	private static final String NUMERIC_ORDER_PATTERN = ORDER_PATTERN
		+ _PERIOD_SEPARATOR_ + "([1-9][0-9]*)";

	private static final Pattern numericOrderPattern = Pattern
		.compile(NUMERIC_ORDER_PATTERN);

	private static final String LAST_ORDER_PATTERN = ORDER_PATTERN
		+ _PERIOD_SEPARATOR_ + LAST_KEYWORD;

	private static final Pattern lastOrderPattern = Pattern
		.compile(LAST_ORDER_PATTERN);
	/* End of Constants. */

	/** The _key. */
	String		_key;

	/** The _tee name. */
	String		_teeName;

	/** The _java pid file. */
	File		_javaPidFile;

	boolean		_initController			= false;

	Runnable	_serviceStartupListener	= null;

	public void init()
	{
		super.init();
		_key = "" + new Random(System.currentTimeMillis()).nextLong();
		_localConfiguration.setProperty("wrapper.key", _key);
		if (_controller == null)
		{
			_controller = new JVMController(this);
			configController();
		}

	}
	
	protected boolean pipeStreams()
	{
		return super.pipeStreams() || _teeName != null;

	}


	protected void setState(int state)
	{
		super.setState(state);
		if (state == STATE_IDLE)
		{
			removeJavaPidFile();
		}

	}

	/**
	 * Config process.
	 */
	void configProcess()
	{
		// _osProcess.destroy();

		_osProcess.setTmpPath(_tmpPath);

		if (!super.pipeStreams())
		{
			// _osProcess.setPipeStreams(true, false);
			_teeName = _key + "$" + System.currentTimeMillis();
			_localConfiguration.setProperty("wrapper.teeName", _teeName);
			_osProcess.setTeeName(_teeName);
			_osProcess.setPipeStreams(false, false);
		}
		else
		{
			_osProcess.setPipeStreams(true, true);
			_osProcess.setTeeName(null);

			if (!_haltAppOnWrapper)
				getWrapperLogger().log(Level.WARNING, "WARNING: application streams are piped, but wrapper.control setting may cause zombie processes. Please set to TIGHT");
		}

		JavaHome javaHome = OperatingSystem.instance().getJavaHome(_config);
		javaHome.setLogger(getInternalWrapperLogger());
		String java = javaHome.findJava(_config.getString("wrapper.java.command"), _config.getString("wrapper.java.customProcName"));
		if (java == null)
			getWrapperLogger().log(Level.SEVERE, "ERROR: could not get java command");
		List jvmOptions = jvmOptions();
		List wrapperOptions = wrapperOptions();
		String mainClass = getMainClass();
		List command = new ArrayList();
		command.add(java);
		command.addAll(jvmOptions);
		command.addAll(wrapperOptions);
		command.add(mainClass);
		// clean spaces ###
		List cleanCommand = new ArrayList<String>();
		for (int i = 0; i < command.size(); i++)
		{
			String text = command.get(i).toString();
			if (text.trim().equals(""))
			{
				continue;
			}
			cleanCommand.add(text);
		}
		String[] arrCmd = new String[cleanCommand.size()];
		for (int i = 0; i < arrCmd.length; i++)
			arrCmd[i] = (String) cleanCommand.get(i);
		_osProcess.setCommand(arrCmd);

		super.configProcess();
	}

	protected String getMainClass()
	{
		return _config.getString("wrapper.java.mainclass", "org.rzo.yajsw.app.WrapperJVMMain");
	}
	
	private String getDOption(String key, String value)
	{
		if (value != null && !value.contains(" "))
			return "-D"+key+"="+value;
		else
			return "-D"+key+"=\""+value+"\"";
	}

	/**
	 * Wrapper options.
	 * 
	 * @return the string
	 */
	private List wrapperOptions()
	{
		ArrayList result = new ArrayList();
		JVMController controller = (JVMController) _controller;

		result.add(getDOption("wrapper.port", ""+controller.getPort()));
		result.add(getDOption("wrapper.key", controller.getKey()));
		if (_teeName != null)
		result.add(getDOption("wrapper.teeName", _teeName));
		result.add(getDOption("wrapper.tmp.path", _tmpPath));
		result.add(getDOption("jna_tmpdir", _tmpPath));

		for (Iterator it = _config.getSystemConfiguration().getKeys("wrapper"); it.hasNext();)
		{
			String key = (String) it.next();
			if (("wrapper.service".equals(key) || "wrapper.console.visible".equals(key)) && _config.getBoolean("wrapper.service", false))
				continue;
			if ("wrapper.config".equals(key))
			{
				result.add(checkValue(getDOption(key, _config.getCachedPath())));
			}
			else
			{
				String opt = getDOption(key, _config.getProperty(key).toString());
				if (!result.contains(opt))
					result.add(checkValue(opt));
			}
		}

		String gcPattern = _config.getString("wrapper.java.monitor.gc", null);
		if ((gcPattern != null) && (gcPattern.length() > 0))
		{
			gcPattern = gcPattern.replaceAll(",","\\\\,");
			result.add(getDOption("wrapper.java.monitor.gc", gcPattern));
		}

		String preScript = _config.getString("wrapper.app.pre.script", null);
		if (preScript != null & !"".equals(preScript))
			try
			{
				File f = new File(preScript);
				if (!f.exists())
					getWrapperLogger().warning("app.pre.script not found: " + preScript);
				else
				{
					preScript = checkValue(f.getCanonicalPath());
					result.add(getDOption("wrapper.app.pre.script", preScript));
				}
			}
			catch (Exception ex)
			{
				getWrapperLogger().log(Level.SEVERE, "WrappedJavaProcess wrapperOptions", ex);
			}

		return result;
	}

	/**
	 * Jvm options.
	 * 
	 * @return the string
	 */
	private List jvmOptions()
	{
		ArrayList result = new ArrayList();
		StringBuffer sb = new StringBuffer();
		sb.append(WrapperLoader.getWrapperAppJar().trim());
		StringBuilder appCp = getAppClassPath(_config.getString("wrapper.working.dir", "."), _config.getKeys("wrapper.java.classpath"));
		if (appCp != null && appCp.length() > 0)
		{
			sb.append(PATHSEP);
			sb.append(appCp);
		}
		// Save the classpath
		final String classpath = checkValue(sb.toString());

		boolean hasXrs = false;
		boolean hasXmx = false;
		boolean hasXms = false;
		for (Iterator it = _config.getKeys("wrapper.java.additional"); it.hasNext();)
		{
			String key = (String) it.next();
			String value = _config.getString(key);
			if (value == null)
			{
				continue;
			}
			// exclude jvm parameters that could not be resolved.
			if (value.contains("?unresolved?"))
			{
				getWrapperLogger().warning("JVM Parameter: '" + key + "' COULD NOT BE RESOLVED!!!");
				continue;
			}
			result.add(checkValue(value));
			hasXrs |= value.contains("-Xrs");
			hasXmx |= value.contains("-Xmx");
			hasXms |= value.contains("-Xms");
		}
		sb = new StringBuffer();
		if (_config.getKeys("wrapper.java.library.path").hasNext())
		{
			for (Iterator it = _config.getKeys("wrapper.java.library.path"); it.hasNext();)
			{
				String key = (String) it.next();
				if (_config.getString(key) == null)
					continue;
				sb.append(checkValue(_config.getString(key)));
				if (it.hasNext())
					sb.append(PATHSEP);
			}
			result.add(getDOption("java.library.path", sb.toString()));
		}

		if (_config.getBoolean("wrapper.service", false) && !hasXrs)
		{
			result.add("-Xrs");
		}
		if (_config.getBoolean("wrapper.service", false))
		{
			result.add("-Dwrapper.service=true");
			result.add("-Dwrapper.console.visible=false");
		}
		else if (_config.getBoolean("wrapper.console.visible", Constants.DEFAULT_CONSOLE_VISIBLE))
			result.add("-Dwrapper.console.visible=true");

		if (_config.containsKey("wrapper.java.initmemory") || _config.containsKey("wrapper.java.initmemory.relative")
				|| _config.containsKey("wrapper.java.maxmemory") || _config.containsKey("wrapper.java.maxmemory.relative"))
		{
			long xmx = 0;
			long xmxr = 0;
			long xms = 0;
			long xmsr = 0;
			OperatingSystem.instance().systemInformation().setLogger(this.getWrapperLogger());
			long totalRAM = 0;
			if (!hasXms)
			{
				try
				{
					xms = _config.getLong("wrapper.java.initmemory", 0);
					xmsr = _config.getLong("wrapper.java.initmemory.relative", 0);
				}
				catch (Exception ex)
				{
					getWrapperLogger().info("error in wrapper.java.initmemory " + ex.getMessage());
				}
				if (xmsr > 0)
					totalRAM = OperatingSystem.instance().systemInformation().totalRAM();
				if (xmsr > 0 && totalRAM > 0)
					xms = (totalRAM * xmsr) / 100 / (1024 * 1024);
				if (xms > 0)
				{
					result.add("-Xms" + xms + "m");
				}
			}
			if (!hasXmx)
			{
				try
				{
					xmx = _config.getLong("wrapper.java.maxmemory", 0);
					xmxr = _config.getLong("wrapper.java.maxmemory.relative", 0);
				}
				catch (Exception ex)
				{
					getWrapperLogger().info("error in wrapper.java.maxmemory " + ex.getMessage());
				}
				if (xmxr > 0 && totalRAM == 0)
					totalRAM = OperatingSystem.instance().systemInformation().totalRAM();
				if (xmxr > 0 && totalRAM > 0)
					xmx = (totalRAM * xmxr) / 100 / (1024 * 1024);
				if (xmx > 0)
				{
					if (xmx < xms)
						xmx = xms;
					if (xmx < 3)
						xmx = 3;
					result.add("-Xmx" + xmx + "m");
				}
			}
		}
		int port = _config.getInt("wrapper.java.debug.port", -1);
		if (port != -1)
		{
			result.add("-Xdebug");
			/*
			 * bkowal - no longer suspend execution when debugging
			 */
			result.add("-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=" + port);
		}
		String preMainScript = _config.getString("wrapper.app.pre_main.script", null);
		if (preMainScript != null && preMainScript.length() > 0)
			result.add("-Dwrapper.app.pre_main.script=" + preMainScript);
		// if we are running as service "remember" the system properties and env vars we have used
		if (_config.getBoolean("wrapper.service", false))
		{
		for (Entry<String, String> e : _config.getEnvLookupSet().entrySet())
		{
			String opt = getDOption(e.getKey(), e.getValue());
			if (opt != null && !result.contains(opt))
				result.add(opt);
		}
		}
		//TODO ??? there seems to be a bug in windows or java socket ???
		// when piping stdout/stderr streams netty client connect hangs
		// it does not hang when jmxremote is set, or in debug mode.
		// probably due to open of socket in launcher before java code is executed
		// jmxremote causes problems with jboss7 loading java.util.Log
		/*
		if (Platform.isWindows() && _config.getBoolean("wrapper.console.pipestreams"))
			if (!result.contains("-Dcom.sun.management.jmxremote"))
				result.add("-Dcom.sun.management.jmxremote");
				*/

		return orderParameters(result, classpath);
	}
	
	/*
	 * List was left as a generic to match the code that would use it.
	 */
	private List orderParameters(List parameters, String classpath)
	{
		Map<String, String> orderedParametersMap = new HashMap<String, String>();

		/*
		 * bkowal - loop through the parameters and re-arrange the parameters
		 * in the specified order.
		 */
		Iterator<String> paramOrderIterator = 
			_config.getKeys("wrapper.jvm.parameter.order");
		while (paramOrderIterator.hasNext())
		{
			String paramOrderKey = paramOrderIterator.next();

			Matcher numericMatcher = 
				numericOrderPattern.matcher(paramOrderKey);
			Matcher lastMatcher =
				lastOrderPattern.matcher(paramOrderKey);

			String index = null;
			/* determine the order parameter type */
			if (numericMatcher.matches())
			{
				index = numericMatcher.group(1);
			}
			if (lastMatcher.matches())
			{
				index = LAST_KEYWORD;
			}

			if (index == null)
			{
				getWrapperLogger().warning("Invalid Parameter Order Specifier: '" + 
					paramOrderKey + "'; SKIPPING!!!");
				continue;
			}

			String parameter = _config.getString(paramOrderKey);
			orderedParametersMap.put(index, parameter);
		}

		/* Determine the total number of ordered parameters. */
		int numberOrdered = orderedParametersMap.size();
		
		/* Ensure that there are actually parameters that we will be ordering. */
		if (numberOrdered <= 0 || numberOrdered > parameters.size())
		{
			// Respect the original YAJSW JVM parameter order.
			parameters.add(0, "-classpath");
			parameters.add(1, classpath);
			return parameters;
		}

		String lastParameter = null;
		int classpathIndex = -1;
		if (orderedParametersMap.containsKey(LAST_KEYWORD))
		{
			numberOrdered -= 1;
			// extract and save off the "LAST" parameter
			String parameter = orderedParametersMap.get(LAST_KEYWORD);
			if (parameter.equals("-classpath"))
			{
				// the end of the list.
				classpathIndex = parameters.size() - 1;
			}
			else
			{
				lastParameter = lookupParameter(parameters, parameter);
				// remove the parameter from the list.
				parameters.remove(lastParameter);
			}
		}

		// loop through the parameters that will need to be ordered.
		for (int i = 1; i <= numberOrdered; i++)
		{
			String parameter = 
				orderedParametersMap.get(Integer.toString(i));
			if (parameter == null)
			{
				continue;
			}

			if (parameter.equals("-classpath"))
			{
				classpathIndex = i - 1;
				continue;
			}
			String orderedParameter = 
				lookupParameter(parameters, parameter);
			if (orderedParameter == null)
			{
				continue;
			}
			// remove the parameter from the list.
			parameters.remove(orderedParameter);
			// add the parameter at the requested location.
			parameters.add(i - 1, orderedParameter);
		} 

		if (lastParameter != null)
		{
			parameters.add(lastParameter);
		}

		if (classpathIndex > 0)
		{
			parameters.add(classpathIndex, "-classpath");
			parameters.add(classpathIndex + 1, classpath);
		}
		else
		{
			// Respect the original YAJSW JVM parameter order.
			parameters.add(0, "-classpath");
			parameters.add(1, classpath);
		}

		return parameters;
	}

	private String lookupParameter(List parameters, String parameter)
	{
		for (Object _parameter : parameters)
		{
			if (_parameter.toString().contains(parameter))
			{
				return _parameter.toString();
			}
		}

		getWrapperLogger().warning("Parameter Not Found: '" + 
			parameter + "'; UNABLE TO ADD TO ORDERED PARAMETERS!!!");
		return null;		
	}

	// call to java "-Ddir=c:\" will cause a parse exception in the java
	// launcher
	private String checkValue(String value)
	{
		value = value.trim();
		if (value.endsWith("\\") && !value.endsWith("\\\\"))
			value += "\\";
		return value;
	}

	/**
	 * Gets the app class path.
	 * 
	 * @param workingDir
	 *            the working dir
	 * @param config
	 *            the config
	 * 
	 * @return the app class path
	 */
	private StringBuilder getAppClassPath(String workingDir, Iterator keys)
	{
		List configList = new ArrayList();
		for (Iterator it = keys; it.hasNext();)
		{
			configList.add(it.next());
		}

		Collections.sort(configList, new AlphanumComparator());
		List files = new ArrayList();
		String jar = _config.getString("wrapper.java.app.jar", null);
		if (jar != null)
		{
			Collection jars = FileUtils.getFiles(workingDir, jar);
			files.addAll(jars);
			files.addAll(classpathFromJar(jars, workingDir));
		}
		for (Iterator it = configList.listIterator(); it.hasNext();)
		{
			String file = _config.getString((String) it.next());
			if (file == null)
				continue;
			files.addAll(FileUtils.getFiles(workingDir, file));
		}
		StringBuilder sb = new StringBuilder();
		for (Iterator it = files.iterator(); it.hasNext();)
		{
			try
			{
				sb.append(((File) it.next()).getCanonicalPath());
			}
			catch (IOException e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (it.hasNext())
				sb.append(PATHSEP);

		}

		/*
		 * bkowal - recursively search the directories specified 
		 * using the wrapper.search.java.classpath jvm parameter and add entries
		 * to the classpath.
		 */
		final String[] jarPattern = new String[] { "jar" };
		List<String> containingDirectories = new ArrayList<String>();
                Iterator<String> searchLocationIterator = _config.getKeys("wrapper.search.java.classpath");
		while (searchLocationIterator.hasNext())
		{
			String key = searchLocationIterator.next();
			String location = _config.getString(key);

			File locationDirectory = new File(location);
			if (locationDirectory.exists() == false || 
			locationDirectory.isDirectory() == false)
			{
				getWrapperLogger().warning(location + 
					" either does not exist or is not a directory; skipping!");
				continue;
			}
			Iterator<?> foundFilesIterator = 
				org.apache.commons.io.FileUtils.iterateFiles(locationDirectory, jarPattern, true);
			while (foundFilesIterator.hasNext())
			{
				File foundFile = (File) foundFilesIterator.next();
				String containingDirectory = org.apache.commons.io.FilenameUtils
					.getFullPath(foundFile.getAbsolutePath());
				if (containingDirectories.contains(containingDirectory) == false) {
					containingDirectories.add(containingDirectory);
					sb.append(PATHSEP);
                                	sb.append(containingDirectory + "*");
				}
			}
		}
		return sb;
	}

	private Collection classpathFromJar(Collection jars, String workingDir)
	{
		Collection result = new ArrayList();
		URL url = null;
		for (Object jar : jars)
		{
		try
		{
			url = ((File)jar).toURI().toURL();
		}
		catch (MalformedURLException e2)
		{
			e2.printStackTrace();
			continue;
		}
		Manifest manifest;
		try
		{
			manifest = new JarFile((File)jar).getManifest();
		}
		catch (IOException e1)
		{
			e1.printStackTrace();
			continue;
		}
		Attributes attr = manifest.getMainAttributes();

		String cl = attr.getValue("Class-Path");
		ClassLoader loader = null;
		if (cl != null)
		{
			String[] clArr = cl.split(" ");
			for (int i = 0; i < clArr.length; i++)
			{
				String file = clArr[i];
				Collection myFile;
				try
				{
					myFile = FileUtils.getFiles(workingDir, file);
					result.addAll(myFile);
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
			}
		}
		}
		return result;
	}

	/**
	 * Config controller.
	 */
	void configController()
	{
				
		JVMController controller = (JVMController) _controller;

		if (_config.getBoolean("wrapper.java.monitor.gc.restart", false))
		{
			long max = _config.getLong("wrapper.java.monitor.gc.threshold", -1);
			controller.setMaxFullGCTimeRestart(max);
		}

		if (_config.getBoolean("wrapper.java.monitor.heap.restart", false))
		{
			long max = _config.getLong("wrapper.java.monitor.heap.threshold.percent", -1);
			controller.setMaxHeapRestart(max);
		}

		controller.setLogger(getWrapperLogger());
		controller.setKey(_config.getString("wrapper.key"));
		if (_config.containsKey("wrapper.port"))
		{
			controller.setMinPort(_config.getInt("wrapper.port"));
			controller.setMaxPort(_config.getInt("wrapper.port"));
		}
		else
		{
			controller.setMinPort(_config.getInt("wrapper.port.min", Constants.DEFAULT_PORT));
			controller.setMaxPort(_config.getInt("wrapper.port.max", 65535));
		}

		controller.setStartupTimeout(_config.getInt("wrapper.startup.timeout", DEFAULT_STARTUP_TIMEOUT) * 1000);
		controller.setPingTimeout(_config.getInt("wrapper.ping.timeout", DEFAULT_PING_TIMEOUT) * 1000);
		if (!_initController)
		{
			ControllerListener restartHandler = new ControllerListener()
			{
				public void fire()
				{
					if (_state == STATE_RESTART_STOP || _state == STATE_RESTART || _state == STATE_RESTART_START || _state == STATE_RESTART_WAIT)
						return;
					if (allowRestart() && exitCodeRestart() && !exitCodeShutdown() && !exitCodeStop())
					{
						restartInternal();
					}
					else
					{
						if (_debug)
						{
							getWrapperLogger().info("giving up after " + _restartCount + " retries");
						}
						if (_state != STATE_USER_STOP)
							setState(STATE_ABORT);
						if (!_exiting)
							stop();
						setState(STATE_IDLE);
						if (exitCodeShutdown())
							stopWrapper();

					}

				}

			};
			ControllerListener killedRestartHandler = new ControllerListener()
			{
				public void fire()
				{
					if (_state == STATE_RESTART_STOP || _state == STATE_RESTART ||  _state == STATE_RESTART_WAIT)
						return;
					if (allowRestart() && exitCodeRestart() && !exitCodeShutdown()&& !exitCodeStop())
					{
						restartInternal();
					}
					else
					{
						if (_debug)
						{
							getWrapperLogger().info("giving up after " + _restartCount + " retries");
						}
						if (_state != STATE_USER_STOP)
							setState(STATE_ABORT);
						if (!_exiting)
							stop();
						setState(STATE_IDLE);
						if (exitCodeShutdown())
							stopWrapper();

					}

				}

			};
			controller.addListener(JVMController.STATE_STARTUP_TIMEOUT, restartHandler);
			controller.addListener(JVMController.STATE_THRESHOLD, restartHandler);
			controller.addListener(JVMController.STATE_PING_TIMEOUT, restartHandler);
			controller.addListener(JVMController.STATE_PROCESS_KILLED, killedRestartHandler);

			if (!_config.getBoolean("wrapper.ntservice.autoreport.startup", true))
				if (getService() instanceof WrapperMainServiceWin)
					setServiceStartupListener(new Runnable()
					{

						public void run()
						{
							((WrapperMainServiceWin) getService()).notifyStartup();
						}

					});

			controller.setServiceStartupListener(_serviceStartupListener);

			controller.init();
			_initController = true;
		}
	}

	void postStart()
	{
		saveJavaPidFile();

	}

	// test main
	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		WrappedProcess[] w = new WrappedProcess[20];
		for (int i = 0; i < w.length; i++)
		{
			w[i] = new WrappedJavaProcess();
			w[i].getLocalConfiguration().setProperty("wrapper.config", "conf/wrapper.helloworld.conf");
			w[i].getLocalConfiguration().setProperty("wrapper.debug", "true");
			w[i].setUseSystemProperties(false);
			w[i].init();
		}
		boolean done = false;
		while (!done)
		{
			// done = true;
			for (int i = 0; i < w.length; i++)
			{
				System.out.println("starting " + i);
				w[i].start();
			}
			try
			{
				Thread.sleep(5000);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
			for (int i = 0; i < w.length; i++)
			{
				System.out.println("stopping " + i);
				w[i].stop();
			}
			try
			{
				Thread.sleep(1000);
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
		}
	}

	/**
	 * Save java pid file.
	 */
	void saveJavaPidFile()
	{
		String file = _config.getString("wrapper.java.pidfile");
		if (file != null)
		{
			try
			{
				_javaPidFile = new File(file);
				if (!_javaPidFile.exists())
					_javaPidFile.createNewFile();
				FileWriter out = new FileWriter(_javaPidFile, false);
				out.write("" + getAppPid());
				out.flush();
				out.close();
				if (_debug)
					getWrapperLogger().info("created jva.pid file " + _javaPidFile.getAbsolutePath());
			}
			catch (Exception e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * Removes the java pid file.
	 */
	void removeJavaPidFile()
	{
		if (_javaPidFile != null)
		{
			try
			{
				_javaPidFile.delete();

				if (_debug)
					getWrapperLogger().info("removed java.pid file " + _javaPidFile.getAbsolutePath());
				_javaPidFile = null;
			}
			catch (Exception e)
			{
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * Reconnect.
	 * 
	 * @param pid
	 *            the pid
	 * 
	 * @return true, if successful
	 */
	public boolean reconnect(int pid)
	{
		if (_state != STATE_IDLE)
			return false;

		_osProcess = OperatingSystem.instance().processManagerInstance().getProcess(pid);
		if (_osProcess == null)
			return false;
		String cmd = _osProcess.getCommand();
		if (cmd == null)
			return false;
		String key = getPropertyFromCommandLine("wrapper.key=[^ \"]*", cmd);
		if (key == null)
			return false;
		String port = getPropertyFromCommandLine("wrapper.port=[^ \"]*", cmd);
		if (port == null)
			return false;
		String configFile = getPropertyFromCommandLine("wrapper.config=[^ \"]*", cmd);
		String teeName = getPropertyFromCommandLine("wrapper.teeName=[^ \"]*", cmd);
		String tmpPath = getPropertyFromCommandLine("wrapper.tmpPath=[^ \"]*", cmd);

		_localConfiguration.setProperty("wrapper.key", key);
		_localConfiguration.setProperty("wrapper.port", port);
		if (teeName != null)
		_localConfiguration.setProperty("wrapper.teeName", teeName);
		_localConfiguration.setProperty("wrapper.tmpPath", tmpPath);
		if (configFile != null)
			_localConfiguration.setProperty("wrapper.config", configFile);

		setReconnecting(true);

		super.init();
		_osProcess.setTeeName(teeName);
		_osProcess.setTmpPath(tmpPath);
		_osProcess.reconnectStreams();

		if (_controller == null)
			_controller = new JVMController(this);

		JVMController controller = (JVMController) _controller;

		// controller.setDebug(true);
		configController();

		_firstRestartTime = System.currentTimeMillis();

		// controller.setDebug(true);
		controller.start();
		controller.processStarted();
		setState(STATE_RUNNING);

		boolean result = controller.waitFor(_config.getInt("wrapper.ping.timeout", DEFAULT_PING_TIMEOUT) * 1000);
		if (result)
		{
			// wait for stream to be available
			for (int i = 0; i < 5 && _osProcess.getInputStream() == null; i++)
				try
				{
					Thread.sleep(200);
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
					Thread.currentThread().interrupt();
				}
			Map triggerActions = getTriggerActions();
			Map regexTriggerActions = getRegexTriggerActions();
			Map missingTriggerActions = getMissingTriggerActions();
			Map missingRegexTriggerActions = getMissingRegexTriggerActions();

			_gobler_in = new Gobler(_osProcess.getInputStream(), getAppLogger(), triggerActions, regexTriggerActions, missingTriggerActions,
					missingRegexTriggerActions, "OUTPUT " + _osProcess.getPid(), _osProcess.getPid());
			_gobler_err = new Gobler(_osProcess.getErrorStream(), getAppLogger(), triggerActions, regexTriggerActions, missingTriggerActions,
					missingRegexTriggerActions, "ERROR " + _osProcess.getPid(), _osProcess.getPid());
			executor.execute(_gobler_err);
			executor.execute(_gobler_in);
			setState(STATE_RUNNING);
			saveJavaPidFile();
			saveLockFile();
		}
		return result;
	}

	/**
	 * Gets the property from command line.
	 * 
	 * @param pattern
	 *            the pattern
	 * @param cmd
	 *            the cmd
	 * 
	 * @return the property from command line
	 */
	private String getPropertyFromCommandLine(String pattern, String cmd)
	{
		String result = null;
		Pattern p = Pattern.compile(pattern);
		Matcher m = p.matcher(cmd);
		if (m.find())
			result = m.group();
		if (result != null && result.length() > 0)
			return result.substring(result.indexOf("=") + 1).replaceAll("'", "");
		return null;
	}

	/**
	 * Gets the tee name.
	 * 
	 * @return the tee name
	 */
	public String getTeeName()
	{
		return _teeName;
	}

	/**
	 * Request thread dump.
	 */
	/**
	 * Request thread dump.
	 */
	public void requestThreadDump()
	{
		if (_controller != null)
		{
			JVMController controller = (JVMController) _controller;
			controller.requestThreadDump();
		}
	}

	public void requestGc()
	{
		if (_controller != null)
		{
			JVMController controller = (JVMController) _controller;
			controller.requestGc();
		}
	}

	public void requestDumpHeap(String fileName)
	{
		if (_controller != null)
		{
			JVMController controller = (JVMController) _controller;
			controller.requestDumpHeap(fileName);
		}
	}

	void stopController(int timeout, String reason)
	{
		JVMController controller = (JVMController) _controller;
		controller.stop(JVMController.STATE_USER_STOP, reason);
	}

	public String getType()
	{
		return "Java-" + super.getType();
	}

	public void setServiceStartupListener(Runnable serviceStartupListener)
	{
		_serviceStartupListener = serviceStartupListener;
	}

	protected void reloadConfiguration()
	{
		super.reloadConfiguration();
	}
	
	public float getHeapPercent()
	{
		if (_controller == null)
			return -1;
		return ((JVMController)_controller).getHeap();
	}

	public long getMinorGCTime()
	{
		if (_controller == null)
			return -1;
		return ((JVMController)_controller).getMinGC();
	}

	public long getFullGCTime()
	{
		if (_controller == null)
			return -1;
		return ((JVMController)_controller).getFullGC();
	}
	
	public long getHeapInBytes() 
	{
		if (_controller == null) 
		{
		       return -1;
		}
		return ((JVMController) _controller).getHeapInBytes();
	}



}
