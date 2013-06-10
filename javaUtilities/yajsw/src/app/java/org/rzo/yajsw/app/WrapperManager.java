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

import java.lang.reflect.Method;
import java.util.Properties;

// TODO: Auto-generated Javadoc
/**
 * The Interface WrapperManager.
 */
public interface WrapperManager
{

	/**
	 * Gets the main method.
	 * 
	 * @return the main method
	 */
	Method getMainMethod();

	/**
	 * Gets the main method args.
	 * 
	 * @return the main method args
	 */
	Object[] getMainMethodArgs();

	/**
	 * Checks if is exit on main terminate.
	 * 
	 * @return true, if is exit on main terminate
	 */
	int getExitOnMainTerminate();

	/**
	 * Inits the.
	 * 
	 * @param args
	 *            the args
	 * @param wrapperClassLoader
	 *            the wrapper class loader
	 */
	void init(String[] args, ClassLoader wrapperClassLoader);

	/**
	 * Start.
	 */
	void start();

	/**
	 * Thread dump.
	 */
	public void threadDump();

	/**
	 * Gets the pid.
	 * 
	 * @return the pid
	 */
	public int getPid();

	/**
	 * Stop.
	 */
	public void stop();

	public void restart();

	String getGroovyScript();

	int getExitOnException();
	
	public void reportServiceStartup();

	void executeScript(String scriptFileName, ClassLoader wrapperClassLoader);
	
	public void signalStopping(int timeoutHint);
	
	public Properties getProperties();
	
	public String getStopReason();
}
