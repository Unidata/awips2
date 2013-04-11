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

// TODO: Auto-generated Javadoc
/**
 * The Interface ErrorHandler.
 */
public interface ErrorHandler
{

	/**
	 * To string.
	 * 
	 * @param id
	 *            the id
	 * 
	 * @return the string
	 */
	public String toString(int id);

	/**
	 * Throw exception.
	 * 
	 * @param id
	 *            the id
	 * 
	 * @throws OsException
	 *             the os exception
	 */
	public void throwException(int id) throws OsException;
}
