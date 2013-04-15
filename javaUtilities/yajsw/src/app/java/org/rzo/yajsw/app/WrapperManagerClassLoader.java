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
import java.net.URLClassLoader;

// TODO: Auto-generated Javadoc
/**
 * The Class WrapperManagerClassLoader.
 */
public class WrapperManagerClassLoader extends URLClassLoader
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
	public WrapperManagerClassLoader(URL[] urls, ClassLoader parent)
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

				}
				catch (ClassNotFoundException e)
				{
					// If still not found, then invoke findClass in order
					// to find the class.
				}
		}
		if (c == null)
			if (_parent != null)
				c = _parent.loadClass(name);

		return c;

	}

}
