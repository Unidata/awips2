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
package org.rzo.yajsw.log;

import java.util.Arrays;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

// TODO: Auto-generated Javadoc
/**
 * The Class MyLogger.
 */
public class MyLogger extends Logger
{

	/** The _name. */
	String	_pid;
	String _name;

	/**
	 * Instantiates a new my logger.
	 */
	public MyLogger()
	{
		super(null, null);
	}

	/**
	 * Sets the name.
	 * 
	 * @param name
	 *            the new name
	 */
	public void setPID(String pid)
	{
		_pid = pid;
	}
	
	public void setName(String name)
	{
		_name = name;
	}

  @Override
  public void log(LogRecord record)
  {
    Object[] newParams = null;
    Object[] params = record.getParameters();
    if (params == null || params.length == 0)
      newParams = new String[] { _pid, _name };
    else
    {
      int newSize = params.length + 2;
      newParams = Arrays.copyOf(params, newSize);
      newParams[newSize - 2] = _pid;
      newParams[newSize - 1] = _name;
    }

    record.setParameters(newParams);
    super.log(record);
  }
}
