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
package org.rzo.yajsw.script;

import org.rzo.yajsw.wrapper.TriggerAction;

// TODO: Auto-generated Javadoc
/**
 * The Interface Script.
 */
public interface Script extends TriggerAction
{

	/**
	 * Execute.
	 * 
	 * @param id
	 *            the id
	 * @param state
	 *            the state
	 * @param count
	 *            the count
	 * @param pid
	 *            the pid
	 * @param exitCode
	 *            the exit code
	 * @param line
	 *            the line
	 * @param wrapperJavaProcess
	 *            the wrapper java process
	 */
	public Object execute();

	public Object execute(String line);

	/**
	 * Gets the script.
	 * 
	 * @return the script
	 */
	public String getScript();

	public void executeWithTimeout();

	public void executeWithTimeout(String line);
}
