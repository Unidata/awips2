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

import java.io.*;
import java.io.IOException;
import java.lang.reflect.Method;

// TODO: Auto-generated Javadoc
/**
 * The Class WrapperMain.
 */
public class WrapperJVMMain extends AbstractWrapperJVMMain
{
	/**
	 * The main method.
	 * 
	 * @param args
	 *            the args
	 * @throws IOException
	 * 
	 * @throws IllegalAccessException 	 *
	 * @throws InstantiationException
	 */
	public static void main(String[] args) throws IOException
	{
		preExecute(args);

		executeMain();

		postExecute();

	}

	protected static void executeMain()
	{
		final Method mainMethod = WRAPPER_MANAGER.getMainMethod();
		if (mainMethod == null)
		{
			System.out.println("no java main method found -> aborting");
			System.exit(999);
		}
		Object[] mainMethodArgs = WRAPPER_MANAGER.getMainMethodArgs();
		try
		{
			mainMethod.invoke(null, new Object[]
			{ mainMethodArgs });
		}
		catch (Throwable e)
		{
			e.printStackTrace();
			exception = e;
		}
	}

}
