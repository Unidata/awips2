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
package org.rzo.yajsw.tray;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;

// TODO: Auto-generated Javadoc
/**
 * A factory for creating WrapperTrayIcon objects.
 */
public class WrapperTrayIconFactory
{

	/**
	 * Creates a new WrapperTrayIcon object.
	 * 
	 * @param name
	 *            the name
	 * @param icon
	 *            the icon
	 * 
	 * @return the wrapper tray icon
	 */
	public static WrapperTrayIcon createTrayIcon(String name, String icon, YajswConfigurationImpl	config)
	{
		WrapperTrayIcon result = null;
		if (config == null)
			config = new YajswConfigurationImpl();
		try
		{
			result = new WrapperTrayIconImpl(name, icon, config);
		}
		catch (Throwable ex)
		{
			System.out.println("java version does not support SystemTray: " + ex.getMessage());
			ex.printStackTrace();
		}
		if (result == null || !result.isInit())
			result = new WrapperTrayIconDummy();
		return result;
	}
	
	static private String getDOption(String key, String value)
	{
		if (value != null && !value.contains(" "))
			return "-D"+key+"="+value;
		else
			return "-D"+key+"=\""+value+"\"";
	}


	/**
	 * Start tray icon process.
	 * 
	 * @param config
	 *            the config
	 * 
	 * @return the process
	 */
	public static Process startTrayIconProcess(YajswConfigurationImpl config, Logger logger)
	{
		if (config == null)
			return null;
		String wrapperConfFileName = config.getCachedPath(false);

		final Process osProcess = OperatingSystem.instance().processManagerInstance().createProcess();

		try
		{
			List<String> cmd = new ArrayList<String>();
			cmd.add(getJava());
			for (Entry<String, String> e : config.getEnvLookupSet().entrySet())
			{
				String opt = getDOption(e.getKey(), e.getValue());
				if (!cmd.contains(opt))
					cmd.add(opt);
			}
			String tmpDir = config.getString("wrapper.tmp.path", System.getProperty("jna_tmpdir", null));
			if (tmpDir != null)
			{
				String opt = getDOption("jna_tmpdir", tmpDir);
				if (!cmd.contains(opt))
					cmd.add(opt);
			}
			else 
			{
				tmpDir = config.getString("wrapper.tmp.path", System.getProperty("java.io.tmpdir", null));
				if (tmpDir != null)
				{
					String opt = getDOption("jna_tmpdir", tmpDir);
					if (!cmd.contains(opt))
						cmd.add(opt);
				}
			}
			
			cmd.add("-classpath");
			cmd.add(WrapperLoader.getWrapperJar());
			cmd.add(TrayIconMainBooter.class.getName());
			cmd.add(wrapperConfFileName);
			String[] arrCmd = new String[cmd.size()];
			for (int i = 0; i < arrCmd.length; i++)
				arrCmd[i] = (String) cmd.get(i);
			osProcess.setCommand(arrCmd);
			osProcess.setPipeStreams(false, false);
			osProcess.setVisible(false);
			osProcess.setLogger(logger);
			osProcess.setDebug(false);
			Runtime.getRuntime().addShutdownHook(new Thread()
			{
				public void run()
				{
					if (osProcess != null)
						osProcess.kill(0);
				}
			});
			osProcess.start();
			logger.info("spawned system tray icon process with pid "+osProcess.getPid());
			return osProcess;
		}
		catch (Exception e)
		{
			logger.throwing("WRapperTRayIconFactory", "startTrayIconProcess", e);
		}
		return null;
	}

	/**
	 * Gets the java.
	 * 
	 * @return the java
	 */
	public static String getJava()
	{
		String result = System.getenv("java_exe");
		if (result == null)
		{
			result = System.getProperty("sun.boot.library.path");
			if (result != null)
				result = result + "/javaw";
			else
				result = "java";
		}
		return result;
	}

}
