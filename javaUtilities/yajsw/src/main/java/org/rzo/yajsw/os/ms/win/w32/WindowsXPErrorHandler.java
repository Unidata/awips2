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

import org.rzo.yajsw.os.ErrorHandler;
import org.rzo.yajsw.os.OsException;

// TODO: Auto-generated Javadoc
/**
 * The Class WindowsXPErrorHandler.
 */
public class WindowsXPErrorHandler implements ErrorHandler
{

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.ErrorHandler#toString(int)
	 */
	public String toString(int id)
	{
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.os.ErrorHandler#throwException(int)
	 */
	public void throwException(int id) throws OsException
	{
		throw new OsException();
	}

}
