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

import org.rzo.yajsw.wrapper.AbstractWrappedProcessMBean;

// TODO: Auto-generated Javadoc
/**
 * The Interface WrapperTrayIcon.
 */
public interface WrapperTrayIcon
{

	/**
	 * Checks if is inits the.
	 * 
	 * @return true, if is inits the
	 */
	public boolean isInit();

	/**
	 * Info.
	 * 
	 * @param caption
	 *            the caption
	 * @param message
	 *            the message
	 */
	public void info(String caption, String message);

	/**
	 * Error.
	 * 
	 * @param caption
	 *            the caption
	 * @param message
	 *            the message
	 */
	public void error(String caption, String message);

	/**
	 * Warning.
	 * 
	 * @param caption
	 *            the caption
	 * @param message
	 *            the message
	 */
	public void warning(String caption, String message);

	/**
	 * Message.
	 * 
	 * @param caption
	 *            the caption
	 * @param message
	 *            the message
	 */
	public void message(String caption, String message);

	/**
	 * Sets the process.
	 * 
	 * @param proxy
	 *            the new process
	 */
	public void setProcess(AbstractWrappedProcessMBean proxy);

	/**
	 * Close console.
	 */
	public void closeConsole();

	/**
	 * Show state.
	 * 
	 * @param state
	 *            the state
	 */
	public void showState(int state);

	/**
	 * Checks if is stop.
	 * 
	 * @return true, if is stop
	 */
	public boolean isStop();

}
