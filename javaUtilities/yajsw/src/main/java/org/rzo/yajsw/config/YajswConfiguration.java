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
package org.rzo.yajsw.config;

// TODO: Auto-generated Javadoc
/**
 * The Interface AjswConfiguration.
 */
public interface YajswConfiguration
{

	/**
	 * Sets the debug.
	 * 
	 * @param b
	 *            the new debug
	 */
	void setDebug(boolean b);

	/**
	 * Inits the.
	 */
	void init();

	/**
	 * Gets the string.
	 * 
	 * @param string
	 *            the string
	 * 
	 * @return the string
	 */
	String getString(String string);

	String getString(String string, String string1);

	public String getCachedPath();

}
