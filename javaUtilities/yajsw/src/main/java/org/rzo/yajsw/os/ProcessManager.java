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
package org.rzo.yajsw.os;

import java.util.List;

// TODO: Auto-generated Javadoc
/**
 * The Interface ProcessManager.
 */
public interface ProcessManager
{

	/**
	 * Creates the process.
	 * 
	 * @return the process
	 */
	public Process createProcess();

	/**
	 * Gets the process.
	 * 
	 * @param pid
	 *            the pid
	 * 
	 * @return the process
	 */
	public Process getProcess(int pid);

	/**
	 * Current process id.
	 * 
	 * @return the int
	 */
	public int currentProcessId();

	/**
	 * Process id of active window.
	 * 
	 * @return the int
	 */
	public int processIdOfActiveWindow();

	/**
	 * Gets the process tree.
	 * 
	 * @param pid
	 *            the pid
	 * 
	 * @return the process tree
	 */
	public List getProcessTree(int pid);

	/**
	 * Task list instance.
	 * 
	 * @return the task list
	 */
	public TaskList taskListInstance();

	public List getProcessIds();

}
