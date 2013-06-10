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
package org.rzo.yajsw;

import java.lang.reflect.Method;
import java.net.URLClassLoader;

import org.rzo.yajsw.boot.WrapperLoader;

public class WrapperExeBooter
{

	/**
	 * The main method. Loads the libs required by YAJSW and starts
	 * WrapperExe.main
	 * 
	 * @param args
	 *            the arguments
	 */
	public static void main(String[] args)
	{
		URLClassLoader cl = WrapperLoader.getWrapperClassLoader();
		Thread.currentThread().setContextClassLoader(cl);
		try
		{
			Class cls = Class.forName("org.rzo.yajsw.WrapperExe", true, cl);
			Method mainMethod = cls.getDeclaredMethod("main", new Class[]
			{ String[].class });
			mainMethod.invoke(null, new Object[]
			{ args });
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

}
