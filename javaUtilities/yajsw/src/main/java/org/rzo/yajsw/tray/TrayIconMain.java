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

import java.io.File;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;

import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.ObjectName;
import javax.management.remote.JMXServiceURL;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.tray.ahessian.client.AHessianJmxClient;
import org.rzo.yajsw.wrapper.AbstractWrappedProcessMBean;
import org.rzo.yajsw.wrapper.TrayIconProxy.Types;

// TODO: Auto-generated Javadoc
/**
 * The Class TrayIconMain.
 */
public class TrayIconMain
{

	/** The jmxc. */
	volatile static MBeanServerConnection		jmxc		= null;

	/** The url. */
	static JMXServiceURL						url			= null;

	/** The user. */
	static String								user		= null;

	/** The password. */
	static String								password	= null;

	/** The proxy. */
	volatile static AbstractWrappedProcessMBean	proxy		= null;

	/** The o name. */
	static ObjectName							oName		= null;

	/** The _tray icon. */
	static WrapperTrayIconImpl					_trayIcon	= null;

	/** The lock. */
	static FileLock								lock		= null;

	/** The _ahessian client. */
	static AHessianJmxClient					_ahessianClient;

	private static String getName(Configuration _config)
	{
		String result = "";
		if (_config == null)
			return result;
		if (_config.getBoolean("wrapper.service", false))
			result += "Service ";
		String name = _config.getString("wrapper.console.title");
		if (name == null)
			name = _config.getString("wrapper.ntservice.name");
		if (name == null)
			name = _config.getString("wrapper.image");
		if (name == null)
			name = _config.getString("wrapper.groovy");
		if (name == null)
			name = _config.getString("wrapper.java.app.mainclass");
		if (name == null)
			name = _config.getString("wrapper.java.app.jar");
		if (name == null)
			name = "";
		result += name;
		return result;

	}

	private static void reconnect() throws InterruptedException
	{
		if (jmxc != null)
		{
			try
			{
				_ahessianClient.close();
			} catch (Throwable e)
			{
				e.printStackTrace();
			}			
			jmxc = null;
		}
		_trayIcon.closeConsole();
		_trayIcon.setProcess(null);
		proxy = null;
		System.out.println("trying to connect");
		while (jmxc == null)
			try
			{
				if (_trayIcon.isStop())
					return;
				// TODO disableFunctions();

				jmxc = _ahessianClient.getMBeanServer();
				if (jmxc != null)
				{
					proxy = (AbstractWrappedProcessMBean) MBeanServerInvocationHandler.newProxyInstance(jmxc, oName,
							AbstractWrappedProcessMBean.class, false);
					_trayIcon.setProcess(proxy);
					_ahessianClient.open();
					System.out.println("connected");
				}
				else
					Thread.sleep(1000);
			}
			catch (Exception e)
			{
				e.printStackTrace();
				Thread.sleep(1000);
			}

	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 * 
	 * @throws Exception
	 *             the exception
	 */
	public static void main(String[] args) throws Exception
	{
		String config = null;
		if (args.length > 0)
			config = args[0];
		File f = new File(config);
		if (!f.exists())
		{
			System.out.println("file not found " + f);
			config = null;
		}

		String canonName = null;
		if (config != null)
		{
			canonName = new File(config).getCanonicalPath();
			File lockFile = new File(System.getProperty("java.io.tmpdir"), "" + "yajsw." + canonName.hashCode() + ".lck");
			System.out.println("system tray lock file: " + lockFile.getCanonicalPath());
			FileChannel channel = new RandomAccessFile(lockFile, "rw").getChannel();
			// Try acquiring the lock without blocking. This method returns
			// null or throws an exception if the file is already locked.
			try
			{
				lock = channel.tryLock();
			}
			catch (OverlappingFileLockException e)
			{
				// File is already locked in this thread or virtual machine
				return;
			}
			if (lock == null)
				return;
		}

		System.setProperty("wrapper.config", config);

		Configuration localConf = new BaseConfiguration();
		if (config != null)
			localConf.addProperty("wrapper.config", config);
		YajswConfigurationImpl _config = new YajswConfigurationImpl(localConf, true);

		_ahessianClient = new AHessianJmxClient(canonName, _config.getInt("wrapper.tray.port", 0));

		try
		{
			String name = _config.getString("wrapper.console.title");
			if (name == null)
				name = _config.getString("wrapper.ntservice.name");
			if (name == null)
				name = "yajsw.noname";
			oName = new ObjectName("org.rzo.yajsw", "name", name);
			_trayIcon = (WrapperTrayIconImpl) WrapperTrayIconFactory.createTrayIcon(getName(_config), _config.getString("wrapper.tray.icon"), _config);
			reconnect();
			while (!_trayIcon.isStop())
			{
				if (jmxc != null && proxy != null)
					try
					{
						if (!showInquire(proxy.getInquireMessage()))
						{
							showMessages(proxy.getTrayIconMessages());
							_trayIcon.showState(proxy.getState());
							_trayIcon.showColor(proxy.getUserTrayColor());
						}
					}
					catch (Exception ex)
					{
						ex.printStackTrace();
						System.out.println("error accessing server " + ex);
						reconnect();
					}
				// System.out.println(">> "+proxy);
				try
				{
					Thread.sleep(1000);
				}
				catch (InterruptedException e)
				{
					System.out.println(e.getMessage());
					Thread.currentThread().interrupt();
				}
			}
		}
		catch (Exception ex)
		{
			System.out.println(ex.getMessage());
			return;
		}
		Runtime.getRuntime().halt(0);
	}

	private static boolean showInquire(String message)
	{
		if (message == null)
			return false;
		if (_trayIcon == null)
			return false;
		if (_trayIcon._inquireMessage == null)
		{
			_trayIcon.message("Input Required", message + "\n enter data through response menue");
			_trayIcon._inquireMessage = message;
			return true;
		}
		return true;

	}

	private static void showMessages(String[][] messages)
	{
		if (_trayIcon == null)
			return;
		if (messages == null)
			return;
		for (String[] message : messages)
		{
			Types type = Types.valueOf(message[0]);
			switch (type)
			{
			case ERROR:
				_trayIcon.error(message[1], message[2]);
				break;
			case INFO:
				_trayIcon.info(message[1], message[2]);
				break;
			case MESSAGE:
				_trayIcon.message(message[1], message[2]);
				break;
			case WARNING:
				_trayIcon.warning(message[1], message[2]);
				break;
			default:
				System.out.println("wrong message type");
			}
		}
	}

}
