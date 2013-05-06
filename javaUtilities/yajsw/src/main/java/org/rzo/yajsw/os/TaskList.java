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

import java.util.Collection;
import java.util.Map;

// TODO: Auto-generated Javadoc
/**
 * The Interface TaskList.
 */
public interface TaskList
{

	/**
	 * Adds the listner.
	 * 
	 * @param listner
	 *            the listner
	 */
	public void addListner(TaskListListner listner);

	/**
	 * Removes the listner.
	 * 
	 * @param listner
	 *            the listner
	 */
	public void removeListner(TaskListListner listner);

	/**
	 * Task list.
	 * 
	 * @return the map
	 */
	public Map taskList();

	/**
	 * The Interface TaskListListner.
	 */
	public interface TaskListListner
	{

		/**
		 * Changed.
		 * 
		 * @param event
		 *            the event
		 */
		public void changed(TaskListEvent event);
	}

	/**
	 * The Interface TaskListEvent.
	 */
	public interface TaskListEvent
	{

		/**
		 * Gets the new tasks.
		 * 
		 * @return the new tasks
		 */
		public Collection getNewTasks();

		/**
		 * Gets the removed tasks.
		 * 
		 * @return the removed tasks
		 */
		public Collection getRemovedTasks();

		/**
		 * Gets the current tasks.
		 * 
		 * @return the current tasks
		 */
		public Collection getCurrentTasks();
	}

}
