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
 * The Class WrapperTrayIconDummy.
 */
public class WrapperTrayIconDummy implements WrapperTrayIcon
{

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#isInit()
	 */
	public boolean isInit()
	{
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#error(java.lang.String,
	 * java.lang.String)
	 */
	public void error(String caption, String message)
	{
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#info(java.lang.String,
	 * java.lang.String)
	 */
	public void info(String caption, String message)
	{
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#message(java.lang.String,
	 * java.lang.String)
	 */
	public void message(String caption, String message)
	{
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#warning(java.lang.String,
	 * java.lang.String)
	 */
	public void warning(String caption, String message)
	{
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.rzo.yajsw.tray.WrapperTrayIcon#setProcess(org.rzo.yajsw.wrapper.
	 * AbstractWrappedProcessMBean)
	 */
	public void setProcess(AbstractWrappedProcessMBean proxy)
	{
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#closeConsole()
	 */
	public void closeConsole()
	{
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#showState(int)
	 */
	public void showState(int state)
	{
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#isStop()
	 */
	public boolean isStop()
	{
		// TODO Auto-generated method stub
		return false;
	}

}
