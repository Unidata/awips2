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
package org.rzo.yajsw.boot;

import java.net.URL;
import java.net.URLClassLoader;

/**
 * Class loader for handling YAJSW related libraries. The classloader will first
 * search the YAJSW related libraries before searching the parent (system
 * classloader). The libraries required by YAJSW will thus not be visible to the
 * application which is started within the system classloader. Exception is for
 * the interface WrapperManager and for the class WrapperManagerProxy. An
 * singleton instance of WrapperManager is visible to the application for
 * shutdown or for restart. WrapperManagerProxy is required for on application
 * startup.
 */
public class WrapperClassLoader extends URLClassLoader
{

	/** The _parent. */
	ClassLoader	_parent;

	/**
	 * Instantiates a new wrapper manager class loader.
	 * 
	 * @param urls
	 *            the urls
	 * @param parent
	 *            the parent
	 */
	public WrapperClassLoader(URL[] urls, ClassLoader parent)
	{
		super(urls, null);
		_parent = parent;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.ClassLoader#loadClass(java.lang.String, boolean)
	 */
	@Override
	public synchronized Class<?> findClass(String name) throws ClassNotFoundException
	{
		// First, check if the class has already been loaded
		Class c = findLoadedClass(name);
		if (c == null)
		{
			if (!"org.rzo.yajsw.app.WrapperManager".equals(name) && !"org.rzo.yajsw.app.WrapperManagerProxy".equals(name))
				try
				{
					c = super.findClass(name);
					//System.out.println("got wrapper class "+name);

				}
				catch (ClassNotFoundException e)
				{
					// If still not found, then invoke findClass in order
					// to find the class.
				}
		}
		if (c == null)
			if (_parent != null)
			{
				c = _parent.loadClass(name);
				//System.out.println("got main class "+name);

			}

		return c;

	}

}
