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
package org.rzo.yajsw.os.ms.win.w32;

// TODO: Auto-generated Javadoc
/**
 * The Interface PdhCounter.
 */
public interface PdhCounter
{

	/**
	 * Gets the double value.
	 * 
	 * @return the double value
	 */
	public double getDoubleValue();

	/**
	 * Gets the int value.
	 * 
	 * @return the int value
	 */
	public int getIntValue();

	/**
	 * Checks if is valid.
	 * 
	 * @return true, if is valid
	 */
	public boolean isValid();

	/**
	 * Close.
	 */
	public void close();

}
