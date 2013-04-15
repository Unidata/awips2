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
package org.rzo.yajsw.app;

import java.net.URL;

import org.rzo.yajsw.boot.WrapperClassLoader;
import org.rzo.yajsw.boot.WrapperLoader;

// TODO: Auto-generated Javadoc
/**
 * The Class WrapperMangerProxy.
 */
public class WrapperManagerProxy
{

	/** The wrapper class loader. */
	static ClassLoader	wrapperClassLoader;

	/**
	 * Gets the wrapper manager.
	 * 
	 * @param args
	 *            the args
	 * 
	 * @return the wrapper manager
	 */
	public static WrapperManager getWrapperManager(final String[] args)
	{
		wrapperClassLoader = getWrapperClassLoader();
		Class wrapperManagerClass;
		final WrapperManager wm;
		WrapperManager result = null;
		try
		{
			wrapperManagerClass = wrapperClassLoader.loadClass("org.rzo.yajsw.app.WrapperManagerImpl");
			wm = (WrapperManager) wrapperManagerClass.newInstance();
			wm.init(args, wrapperClassLoader);
			// start the wrapper manager in a separate thread
			// so application may start even if communication to controller
			// takes time

			new Thread(new Runnable()
			{

				public void run()
				{
					try
					{
						wm.start();
					}
					catch (Exception ex)
					{
						ex.printStackTrace();
					}
				}
			}, "yajsw.app.manager.start").start();
			// give the manager.start a chance.
			Thread.yield();
			result = wm;
			String preScript = System.getProperty("wrapper.app.pre_main.script");
			if (preScript != null & !"".equals(preScript))
			{
				//Logger logger = new MyLogger();
				//logger.addHandler(new ConsoleHandler());
				wm.executeScript(preScript, wrapperClassLoader);
			}

		}
		catch (Exception e1)
		{
			e1.printStackTrace();
		}

		return result;
	}

	/**
	 * Gets the wrapper class loader.
	 * 
	 * @return the wrapper class loader
	 */
	private static ClassLoader getWrapperClassLoader()
	{
		URL[] urlsArr = WrapperLoader.getWrapperClasspath("App", true);
		if (urlsArr == null)
			return Thread.currentThread().getContextClassLoader();
		return new WrapperClassLoader(urlsArr, ClassLoader.getSystemClassLoader());
	}

}
